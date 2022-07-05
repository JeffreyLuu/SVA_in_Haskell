library(bigsnpr)
options(bigstatsr.check.parallel.blas = FALSE)
options(default.nproc.blas = NULL)
library(data.table)
library(magrittr)
library("R.utils")
source("LD_code.R")


# load summary statistics
sumstats <- bigreadr::fread2("external/fluid_intelligence.neale.tsv.gz")
names(sumstats) <- c("variant","rsid","n_eff","AC","ytx","beta","beta_se","tstat","p","chr","pos","a0","a1","info","MAF")
info <- readRDS("hap_map3.RData")
sumstats <- sumstats[sumstats$rsid%in% info$rsid,]
NCORES <- 4

# load raw genotype
c(map, info_snp, genotype) <- load_genotype()

# load phenotype
pheno <- load_phenotype()

# remove unknown phenotype and overlap
unknown <- which(is.na(pheno$FSIQ_First))
geno_filename <- "geno_IQ"
pheno_IQ <- remove_bad_data(unknown, paste0(geno_filename, ".RData"))

# load genotype and phenotype
geno_IQ <- load_genotype_clean(geno_filename)

# calculate ld score and matrix
ld_output <- "ld_IQ.RData"
corr_output <- "corr_IQ.RData"
calc_ld_mat(info_snp, map, geno_IQ, ld_output, corr_output)


# calculate h2_est from ld score regression
ld <- readRDS(ld_output)
corr <- readRDS(corr_output)
df_beta <- info_snp[,c("beta", "beta_se", "n_eff", "_NUM_ID_")]
ldsc <- snp_ldsc(   ld, 
                    length(ld), 
                    chi2 = (df_beta$beta / df_beta$beta_se)^2,
                    sample_size = df_beta$n_eff, 
                    blocks = NULL)
h2_est <- ldsc[["h2"]]
# h2_est = 0.1739733
# h2_est for training data = 0.1209745

# the null model
null.model <- paste("PC", 1:5, sep = "", collapse = "+") %>%
  paste0("FSIQ_Z_First~sex+Age_FSIQ_First +", .) %>%
  as.formula %>%
  lm(., data = pheno_IQ) %>%
  summary
null.r2 <- null.model$r.squared
# null.r2 =  0.01984344
# null.r2 for test data = 0.03418864

# inf model
beta_inf <- snp_ldpred2_inf(corr, df_beta, h2 = h2_est)

ind.test <- 1:nrow(geno_IQ)
pred_inf <- big_prodVec(    geno_IQ,
                            beta_inf,
                            ind.row = ind.test,
                            ind.col = info_snp$`_NUM_ID_`)

# final performance
reg.formula <- paste("PC", 1:5, sep = "", collapse = "+") %>%
    paste0("FSIQ_Z_First ~ PRS + sex + Age_FSIQ_First +", .) %>%
    as.formula
reg.dat <- pheno_IQ
reg.dat$PRS <- pred_inf
inf.model <- lm(reg.formula, dat=reg.dat)
inf_summ <- summary(inf.model)
r2 <- inf_summ$r.squared
# r2 = 0.07612382, difference = 0.05628038
# r2 on test data = 0.09082557, difference = 0.05663693


# LDpred-grid
message("start to run beta_grid")
grid.param <-
  expand.grid(p = 0.01,
              h2 = h2_est,
              sparse = c(FALSE, TRUE))
beta_grid <-
  snp_ldpred2_grid(corr, df_beta, grid.param, ncores = NCORES)

ind.test <- 1:nrow(geno_IQ)
pred_grid <- big_prodMat(   geno_IQ, 
                            beta_grid, 
                            ind.col = info_snp$`_NUM_ID_`)

reg.formula <- paste("PC", 1:5, sep = "", collapse = "+") %>%
  paste0("FSIQ_Z_First ~ PRS + sex + Age_FSIQ_First +", .) %>%
  as.formula
reg.dat <- pheno_IQ
max.r2 <- 0
for(i in 1:ncol(pred_grid)){
  reg.dat$PRS <- pred_grid[,i]
  grid.model <- lm(reg.formula, dat=reg.dat) %>%
    summary  
  if(max.r2 < grid.model$r.squared){
    max.r2 <- grid.model$r.squared
  }
}
# max.r2 = 0.07713089, difference = 0.05728745
# max.r2 for test data = 

# train-test split
n <- 722
set.seed(42)
ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
geno_train <- geno_IQ[ind, ]
geno_test <- geno_IQ[!ind, ]
big_write(geno_train, "geno_IQ_train.RData", every_nrow=100, progress=TRUE)
big_write(geno_test, "geno_IQ_test.RData", every_nrow=100, progress=TRUE)
pheno_train <- pheno_IQ[ind, ]
pheno_test <- pheno_IQ[!ind, ]
saveRDS(pheno_train, "pheno_IQ_train.RData")
saveRDS(pheno_test, "pheno_IQ_test.RData")
