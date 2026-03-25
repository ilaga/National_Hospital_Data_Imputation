# Load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(mice)


########################################################################
## To generate combinations
# Generate all valid (col, imp_method) combinations
# all_combos_list <- lapply(names(dat), function(col) {
#   if (col %in% Missing_con) {
#     methods <- methods_num
#   } else if (col %in% Missing_binary) {
#     methods <- methods_bin
#   } else if (col %in% Missing_cate) {
#     methods <- methods_cate
#   } else {
#     methods <- NULL  # skip if not in any category
#   }
#
#   if (is.null(methods)) return(NULL)
#
#   data.frame(
#     col = col,
#     imp_method = methods,
#     stringsAsFactors = FALSE
#   )
# })
#
# # Combine into one data frame
# all_combos <- do.call(rbind, all_combos_list)
#
# sbatch_lines <- apply(all_combos, 1, function(x) {
#   paste("sbatch run_imp.sh", x[1], x[2])
# })
#
# # Save to text file
# writeLines(sbatch_lines, "submit_jobs.txt")
#

########################################################################


args <- commandArgs(trailingOnly = TRUE)
col <- args[1]
imp_method <- args[2]

print(paste0("col: ", col))
print(paste0("imp_method: ", imp_method))


# Import Excel files and add year column
AHA_22 <- read_excel("./Data/2024_12_FY22.xlsx", sheet = "FY22") %>%
  mutate(Year = 2022)

AHA_23 <- read_excel("./Data/2024_12_FY23.xlsx", sheet = "FY23") %>%
  mutate(HHEGTKFC = as.numeric(HHEGTKFC), Year = 2023)


# Define numeric variables
Keep_numeric <- c(
  "HOSPBD",
  "EXPTOT",
  "VEM",
  "FTMDTF",
  "FTRNTF",
  "ADC",
  "PLNTA",
  "GFEET",
  "CEAMT",
  "VIDVZ",
  "PRPM"
)
Remove_numeric <- c(
  "PSYBD",
  "ADMTOT",
  "IPDTOT",
  "MCRIPD",
  "MCDIPD",
  "SPTIP",
  "THRTIP",
  "FTPHR",
  "FTAPRN",
  "FTPHRN"
)
var_numeric <- c(Keep_numeric, Remove_numeric)

# remove the variable
remove_cate_cleaning <- c(
  "COMMTYC",
  "PSCBD",
  "COMMTY",
  "MEDADHOS",
  "MMCHOS",
  "OTHIMHOS",
  "HLINHOS",
  "OSMGOTH",
  "SCFOD",
  "SCTRN",
  "SCIOS",
  "SCOTH",
  "SCBH",
  "SCBH (not available in FY23)"
)

Keep_cate <- c(
  "ID",
  "CNTYNAME",
  "CBSANAME",
  "CBSACODE",
  "CHC",
  "MAPP1",
  "MAPP18",
  "MAPP20",
  "IINSPT",
  "CMRPAY",
  "FAMADV",
  "COUTRHOS",
  "FITCHOS",
  "HLTHSHOS",
  "HLTRHOS",
  "EMDEPHOS",
  "NUTRPHOS",
  "ONCOLHOS",
  "PALHOS",
  "SOCEHR",
  "OUTMTX",
  "WFAIPPD",
  "COLLCLI",
  "TRAUML90",
  "SCNED",
  "CLUSTER",
  "Year"
)
Missing_cate <- c(
  "IINSPT",
  "CMRPAY",
  "FAMADV",
  "COUTRHOS",
  "EMDEPHOS",
  "FITCHOS",
  "HLTHSHOS",
  "HLTRHOS",
  "NUTRPHOS",
  "ONCOLHOS",
  "PALHOS",
  "SOCEHR",
  "OUTMTX",
  "WFAIPPD",
  "COLLCLI",
  "TRAUML90",
  "SCNED",
  "CLUSTER"
)
Missing_con <- c("PLNTA", "GFEET", "CEAMT", "VIDVZ", "PRPM")

complete_con <- c("HOSPBD", "EXPTOT", "VEM", "FTMDTF", "FTRNTF", "ADC")
complete_cat <- c("CHC", "MAPP1", "MAPP18", "MAPP20")


# Define the binary list
cate_common_list <- c("ID", "CNTYNAME", "CBSANAME", "CBSACODE") # Common variables
cate_list <- c("TRAUML90", "SCNED", "CLUSTER")
binary_list <- setdiff(Keep_cate, c(cate_common_list, cate_list))
bianry_cate_list <- c(cate_list, binary_list)

# Get all column names
all_columns_22 <- setdiff(names(AHA_22), remove_cate_cleaning) # Get names and remove variables
all_columns_23 <- setdiff(names(AHA_23), remove_cate_cleaning) # Get names and remove variables

# Identify categorical variables
var_string_22 <- setdiff(all_columns_22, var_numeric)
var_string_23 <- setdiff(all_columns_23, var_numeric)

# Convert column types
AHA_22_cleaned <- AHA_22[all_columns_22] %>%
  mutate(across(all_of(var_string_22), as.character)) %>%
  mutate(across(all_of(var_numeric), ~ suppressWarnings(as.numeric(.))))

AHA_23_cleaned <- AHA_23[all_columns_23] %>%
  mutate(across(all_of(var_string_23), as.character)) %>%
  mutate(across(all_of(var_numeric), ~ suppressWarnings(as.numeric(.))))


# Merge datasets
merged_AHA <- bind_rows(AHA_22_cleaned, AHA_23_cleaned)

merged_AHA$TRAUML90 <- ifelse(merged_AHA$TRAUML90 == 5, 4, merged_AHA$TRAUML90)

merged_AHA$CHC[merged_AHA$CHC == 2] <- 0
merged_AHA$MAPP1[merged_AHA$MAPP1 == 2] <- 0
merged_AHA$MAPP18[merged_AHA$MAPP18 == 2] <- 0
merged_AHA$MAPP20[merged_AHA$MAPP20 == 2] <- 0

########################################################### 3
merged_AHA$Missing <- ifelse(is.na(merged_AHA$COUTRHOS), 1, 0)


############################# Imputation #########################################
# Convert the variable types
merged_AHA_clean <- merged_AHA %>%
  mutate(across(all_of(bianry_cate_list), as.factor)) %>%
  mutate(across(all_of(Keep_numeric), ~ suppressWarnings(as.numeric(.))))

Keep_numeric <- Keep_numeric[Keep_numeric != "EXPTOT"]

# Log-transformation ------------------------------------------------------
## Could compare imputation for before/after log transform


dat <- merged_AHA_clean[c(bianry_cate_list, Keep_numeric)]

tmp_dat_trans <- dat
for (v in Keep_numeric) {
  vals <- dat[[v]]
  if (all(vals >= 0, na.rm = TRUE)) {
    tmp_dat_trans[[v]] <- log1p(vals)
  } else {
    warning(paste("Variable", v, "contains negative values - skipped."))
  }
}


dat <- tmp_dat_trans


Missing_binary <- c(
  "IINSPT", "CMRPAY", "FAMADV",
  "COUTRHOS", "EMDEPHOS", "FITCHOS", "HLTHSHOS", "HLTRHOS",
  "NUTRPHOS", "ONCOLHOS", "PALHOS", "SOCEHR", "OUTMTX", "WFAIPPD", "COLLCLI"
)


# Cross-validation --------------------------------------------------------


methods_num <- c(
  "pmm", "midastouch", "sample", "cart", "rf", "mean", "norm",
  "norm.nob", "norm.boot", "norm.predict", "lasso.norm",
  "lasso.select.norm", "quadratic"
)

methods_bin <- c(
  "pmm", "midastouch", "sample", "cart", "rf", "logreg",
  "logreg.boot", "lasso.logreg", "lasso.select.logreg"
)


methods_cate <- c("pmm", "midastouch", "sample", "cart", "rf", "polyreg", "lda")


set.seed(1)


imp_method_vec <- make.method(dat, defaultMethod = c("rf")) # initialize (names same as columns)
imp_method_vec[col] <- imp_method

# Initialize result storage
imputation_results <- list()

# Identify observed (non-missing) indices
obs_ind <- which(!is.na(dat[[col]]))
n <- length(obs_ind)

if (n < 5) stop("Not enough observed cases for 5-fold CV.")

# 5-fold partition of observed indices
folds <- sample(rep(1:5, length.out = n))

for (f in 1:5) {
  cat("Fold", f, "for variable", col, "using method", imp_method, "\n")

  # Copy dataset
  dat_cv <- dat

  # Mask observed values in the current fold
  mask_idx <- obs_ind[folds == f]
  dat_cv[mask_idx, col] <- NA

  # Run multiple imputations (m = 5)
  imp_cv <- mice(
    dat_cv,
    method = imp_method_vec,
    m = 5,
    maxit = 10,
    printFlag = F
  )

  # Get all 5 completed datasets
  completed_list <- lapply(1:5, function(i) complete(imp_cv, i))

  # Extract imputed values for the masked observations across imputations
  imputed_values <- sapply(completed_list, function(x) x[mask_idx, col])

  # Store fold results
  imputation_results[[paste0("fold_", f)]] <- list(
    masked_indices = mask_idx,
    observed = dat[mask_idx, col],
    imputed = imputed_values
  )
}

# Save results
saveRDS(imputation_results, file = paste0("./Results/imp_res_", col, "_", imp_method, ".rds"))
