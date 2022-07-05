library(bigsnpr)
options(bigstatsr.check.parallel.blas = FALSE)
options(default.nproc.blas = NULL)
library(data.table)
library(magrittr)
library("R.utils")

sumstats <- bigreadr::fread2("external/fluid_intelligence.neale.tsv.gz")
names(sumstats) <- c("variant","rsid","n_eff","AC","ytx","beta","beta_se","tstat","p","chr","pos","a0","a1","info","MAF")

info <- readRDS("hap_map3.RData")
sumstats <- sumstats[sumstats$rsid%in% info$rsid,]

NCORES <- 4
obj.bigSNP <- snp_attach("2022_01_25/22Qonly_INDM2_Sex_Mind001_SNPqc.good_snps.rds")
map <- obj.bigSNP$map[-3]
names(map) <- c("chr", "rsid", "pos", "a1", "a0")
info_snp <- snp_match(sumstats, map)
genotype <- obj.bigSNP$genotypes

cv_partition <- function(df, name, isTrain) {
   df['pre_site_id'] <- substr(df[, 'site_id'], 1, 3)
  train <- df %>% filter(pre_site_id != name)
  validate <- df %>% filter(pre_site_id == name)
  if(isTrain) {return(train)}
  else {return(validate)}
}

fam.order <- as.data.table(obj.bigSNP$fam)
setnames(fam.order, "sample.ID", "IID")

eigenvec_file <- "2022_01_25/pca.allsnps.eigenvec"
ibbc_analysis_prefix <- "2022_01_25/22Qonly_INDM2_Sex_Mind001_SNPqc.good_snps"

results_dir <- "2022_01_25"
external_dir <- "external/"
iBBC <- "external/22Qonly_INDM2_Sex_Mind001_SNPqc"
pheno_file_name_no_extension <- "iBBC_AIMIIdata_14June2018"

R_dir <- "code"
source(file.path(R_dir, "functions.R"))
source(file.path(R_dir, "pheno_info.R"))

pheno_csv_file <- file.path(external_dir, paste0(pheno_file_name_no_extension, ".csv"))
pheno <- read.csv(pheno_csv_file)
eigenvec <- read.table(eigenvec_file, header = TRUE)
pheno <- pheno[match_iid_to_affy_ids(eigenvec[, "IID"], pheno), ]
pheno <- cbind(pheno, eigenvec)


ld <- readRDS("ld_fluid.RData")
corr <- readRDS("corr_fluid.RData")
df_beta <- info_snp[,c("beta", "beta_se", "n_eff", "_NUM_ID_")]

ldsc <- snp_ldsc(   ld,
                    length(ld),
                    chi2 = (df_beta$beta / df_beta$beta_se)^2,
                    sample_size = df_beta$n_eff,
                    blocks = NULL)
h2_est <- ldsc[["h2"]]

null.model <- paste("PC", 1:5, sep = "", collapse = "+") %>%
  paste0("FSIQ_Z_First~sex+", .) %>%
  as.formula %>%
  lm(., data = pheno) %>%
  summary
null.r2 <- null.model$r.squared
# null.r2 = 0.01593768

message("start to run beta_grid")
p_seq <- signif(seq_log(1e-4, 1, length.out = 7), 2)
# h2_seq <- round(h2_est * c(0.7, 1, 1.4), 4)
grid.param <-
  expand.grid(p = p_seq,
              h2 = h2_est,
              sparse = c(FALSE, TRUE))
beta_grid <-
  snp_ldpred2_grid(corr, df_beta, grid.param, ncores = NCORES)

# calculate PRS for all samples
ind.test <- 1:nrow(genotype)
pred_grid <- big_prodMat(   genotype,
                            beta_grid,
                            ind.col = info_snp$`_NUM_ID_`)

reg.formula <- paste("PC", 1:5, sep = "", collapse = "+") %>%
  paste0("FSIQ_Z_First ~ PRS + sex + Age_FSIQ_First +", .) %>%
  as.formula
reg.dat <- pheno
max.r2 <- 0
for(i in 1:ncol(pred_grid)){
  reg.dat$PRS <- pred_grid[,i]
  grid.model <- lm(reg.formula, dat=reg.dat) %>%
    summary
  if(max.r2 < grid.model$r.squared){
    max.r2 <- grid.model$r.squared
  }
}
(result <- data.table(
  grid = max.r2 - null.r2,
  null = null.r2
))
saveRDS(result,"result_fluid_grid.RData")


