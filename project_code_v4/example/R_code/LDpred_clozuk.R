library(bigsnpr)
options(bigstatsr.check.parallel.blas = FALSE)
options(default.nproc.blas = NULL)
library(data.table)
library(magrittr)
library("R.utils")
source("LD_code.R")


# load summary statistics
sumstats <- bigreadr::fread2("external/clozuk_pgc2.meta.sumstats.reformatted.txt.gz")
names(sumstats)<-c("SNP","MAF","chr","pos","a1","a0","OR","beta_se","p","variant","rsid")
sumstats$beta <- log(sumstats$OR)
info <- readRDS("hap_map3.RData")
sumstats <- sumstats[sumstats$rsid%in% info$rsid,]
NCORES <- 4

#load raw genotype
c(map, info_snp, genotype) <- load_genotype()

#load phenotype
pheno <- load_phenotype()

# remove unknown phenotype and overlap
unknown <- which((pheno$group2018 == "Case_AffectivePsychosis") | (pheno$group2018 == "Unknown"))
geno_filename <- "geno_clozuk"
pheno_clozuk <- remove_bad_data(unknown, paste0(geno_filename, ".RData"))

# load cleaned genotype
geno_clozuk <- load_genotype_clean(geno_filename)

# calculate ld score and matrix
ld_output <- "ld_clozuk.RData"
corr_output <- "corr_clozuk.RData"
calc_ld_mat(info_snp, map, geno_clozuk, ld_output, corr_output)


# calculate h2_est from ld score regression
ld <- readRDS(ld_output)
corr <- readRDS(corr_output)
df_beta <- info_snp[,c("beta", "beta_se", "_NUM_ID_")]
df_beta$n_eff <- 77096

message("ld score regression")
ldsc <- snp_ldsc(   ld, 
                    length(ld), 
                    chi2 = (df_beta$beta / df_beta$beta_se)^2,
                    sample_size = df_beta$n_eff, 
                    blocks = NULL)
h2_est <- ldsc[["h2"]]
# h2_est = 0.3986387
# h2_est for training data = 0.2906329

# null model
case <- array(NA, nrow(pheno_clozuk))
case[pheno_clozuk$group2018 == "Case_SSD"] <- 1
case[pheno_clozuk$group2018 == "Control"] <- 0
case[pheno_clozuk$group2018 == "PutativeControl"] <- 0
formula <- as.formula(paste0("case ~ sex + maxassessmentage +", paste0("PC", 1:5, collapse = "+")))
m <- rms::lrm(formula, data = pheno_clozuk)
null.r2 <- m$stats["R2"]
# null.r2 = 0.1189401
# null.r2 for test data = 0.1268158 

# LDpred-inf
message("start to run beta_inf")
beta_inf <- snp_ldpred2_inf(corr, df_beta, h2 = h2_est)
saveRDS(beta_inf, "beta_inf_clozuk.RData")

ind.test <- 1:nrow(geno_clozuk)
pred_inf <- big_prodVec(    geno_clozuk,
                            beta_inf,
                            ind.row = ind.test,
                            ind.col = info_snp$`_NUM_ID_`)

PRS <- readRDS("pred_inf_clozuk.RData")
formula <- as.formula(paste0("case ~ PRS + sex + maxassessmentage +", paste0("PC", 1:5, collapse = "+")))
m <- rms::lrm(formula, data = pheno_clozuk)
print(m$stats["R2"])
# r2 =  0.1771377, difference = 0.0581976
# r2 on test data = 0.1812234, difference = 0.0544076



# LDpred-grid
#  p_seq
# 0.03200 0.05600 0.10000 0.18000 
message("start to run beta_grid")
grid.param <-
  expand.grid(p = 0.01,
              h2 = h2_est,
              sparse = c(FALSE, TRUE))
beta_grid <-
  snp_ldpred2_grid(corr, df_beta, grid.param, ncores = NCORES)

ind.test <- 1:nrow(geno_clozuk)
pred_grid <- big_prodMat(   geno_clozuk, 
                            beta_grid, 
                            ind.col = info_snp$`_NUM_ID_`)

reg.formula <- paste("PC", 1:5, sep = "", collapse = "+") %>%
  paste0("case ~ PRS + sex + maxassessmentage +", .) %>%
  as.formula
reg.dat <- pheno_clozuk
max.r2 <- 0
for(i in 1:ncol(pred_grid)){
  reg.dat$PRS <- pred_grid[,i]
  formula <- as.formula(paste0("case ~ PRS + sex + maxassessmentage +", paste0("PC", 1:5, collapse = "+")))
  m <- rms::lrm(formula, data = reg.dat)
  if(max.r2 < m$stats["R2"]){
    max.r2 <- m$stats["R2"]
  }
}
# max.r2 = 0.1904056 (p=0.056), difference = 0.0714655
# max.r2 on test set = 0.1910819 (p=0.056), difference = 0.0642661

# train-test split
n <- 962
set.seed(42)
ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
geno_train <- geno_clozuk[ind, ]
geno_test <- geno_clozuk[!ind, ]
big_write(geno_train, "geno_clozuk_train.RData", every_nrow=100, progress=TRUE)
big_write(geno_test, "geno_clozuk_test.RData", every_nrow=100, progress=TRUE)
pheno_train <- pheno[ind, ]
pheno_test <- pheno[!ind, ]
saveRDS(pheno_train, "pheno_clozuk_train.RData")
saveRDS(pheno_test, "pheno_clozuk_test.RData")


