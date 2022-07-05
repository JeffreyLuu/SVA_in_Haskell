library(bigsnpr)

load_genotype <- function() {
    obj.bigSNP <- snp_attach("2022_01_25/22Qonly_INDM2_Sex_Mind001_SNPqc.good_snps.rds")
    map <- obj.bigSNP$map[-3]
    names(map) <- c("chr", "rsid", "pos", "a1", "a0")
    info_snp <- snp_match(sumstats, map)
    genotype <- obj.bigSNP$genotypes

    fam.order <- as.data.table(obj.bigSNP$fam)
    setnames(fam.order, "sample.ID", "IID")
    return(list(map, info_snp, genotype))
}

load_phenotype <- function() {
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
    return(pheno)
}

remove_bad_data <- function(unknown, filename) {
    overlap <- which((pheno$IID == "BM-SNP_6-13250_MV60705") | (pheno$IID == "BM-SNP_6-13222_MV-V03_03") | (pheno$IID == "BM-SNP_6-13241_MV-V28_04"))
    remove <- c(unknown, overlap)
    pheno_clean <- pheno[-remove, ]
    geno_clean <- genotype[-remove, ]
    big_write(geno_clean, filename, every_nrow=100,  progress=TRUE)
    return(pheno_clean)
}

load_genotype_clean <- function(filename) {
    if (file.exists(paste0(filename, ".bk"))) {file.remove(paste0(filename, ".bk"))}
    genotype_raw <- big_read(paste0(filename, ".RData"), c(1:4002885), type="unsigned char")
    code <- rep(NA_real_, 256)
    code[1:3] <- c(0:2)
    geno_clean <- add_code256(genotype_raw, code)
    return(geno_clean)
}


calc_ld_mat <- function(info_snp, map, geno_clean, ld_output, corr_output) {
    CHR <- map$chr
    POS <- map$pos
    POS2 <- snp_asGeneticPos(CHR, POS, dir = ".")

    tmp <- tempfile(tmpdir = "tmp-data")
    on.exit(file.remove(paste0(tmp, ".sbk")), add = TRUE)

    for (chr in 1:22) {
        message("chr: ", chr)  
        ind.chr <- which(info_snp$chr == chr)
        ind.chr <- sort(ind.chr)
        dim <- dimension(ind.chr)
        n_times <- ceiling((dim-1000)/10000)
        for (i in 1:n_times) {
            l <- (i-1)*10000+1
            r <- min(i*10000+1000, dim)
            ind.chr2 <- info_snp$`_NUM_ID_`[ind.chr[l:r]]
            corr0 <- snp_cor(
                geno_clean,
                ind.col = ind.chr2,
                ncores = NCORES,
                infos.pos = POS2[ind.chr2],
                size = 500
            )
            if(i==1) {l2 = 1}
            else {l2=501}
            if(i==n_times) {r2=r-(i-1)*10000}
            else {r2=r-500-(i-1)*10000}
            corr1 = corr0[l2:r2, l2:r2]
            if((chr==1) && (i==1)) {
                ld <- Matrix::colSums(corr1^2)
                corr <- as_SFBM(corr1, tmp)
            } else {
                ld_part <- Matrix::colSums(corr1^2)
                ld <- c(ld, ld_part)
                corr$add_columns(corr1, nrow(corr))
            }
        }
    }
    saveRDS(ld, ld_output)
    saveRDS(corr, corr_output)
}
