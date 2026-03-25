# Load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(skimr)
library(mice)
library(miceadds)
library(ggplot2)
library(naniar)
library(corrplot)
library(purrr)
library(broom)
library(VIM)
library(vcd)
library(reshape2)
library(caret)
library(car)
library(here)

# Import Excel files and add year column
AHA_22 <- read_excel("./Data/2024_12_FY22.xlsx", sheet = "FY22") %>%
  mutate(Year = 2022)

AHA_23 <- read_excel("./Data/2024_12_FY23.xlsx", sheet = "FY23") %>%
  mutate(HHEGTKFC = as.numeric(HHEGTKFC), Year = 2023)


# Define numeric variables
Keep_numeric <- c(
  "HOSPBD", "EXPTOT", "VEM", "FTMDTF", "FTRNTF", "ADC",
  "PLNTA", "GFEET", "CEAMT", "VIDVZ", "PRPM"
)
Remove_numeric <- c("PSYBD", "ADMTOT", "IPDTOT", "MCRIPD", "MCDIPD", "SPTIP", "THRTIP", "FTPHR", "FTAPRN", "FTPHRN")
var_numeric <- c(Keep_numeric, Remove_numeric)

# remove the variable
remove_cate_cleaning <- c(
  "COMMTYC", "PSCBD", # Delete them due to high missing volumne
  "COMMTY", # highly correlated with CHC
  "MEDADHOS", "MMCHOS", "OTHIMHOS", "HLINHOS", "OSMGOTH", # this is the subvar. for IINSPT
  "SCFOD", "SCTRN", "SCIOS", "SCOTH", "SCBH", "SCBH (not available in FY23)" # this is the subvar. for SCNED
)

Keep_cate <- c(
  "ID", "CNTYNAME", "CBSANAME", "CBSACODE",
  "CHC", "MAPP1", "MAPP18", "MAPP20", "IINSPT", "CMRPAY", "FAMADV",
  "COUTRHOS", "FITCHOS", "HLTHSHOS", "HLTRHOS", "EMDEPHOS",
  "NUTRPHOS", "ONCOLHOS", "PALHOS", "SOCEHR", "OUTMTX", "WFAIPPD", "COLLCLI",
  "TRAUML90", "SCNED", "CLUSTER",
  "Year"
)

Missing_cate <- c(
  "IINSPT", "CMRPAY", "FAMADV",
  "COUTRHOS", "EMDEPHOS", "FITCHOS", "HLTHSHOS", "HLTRHOS",
  "NUTRPHOS", "ONCOLHOS", "PALHOS", "SOCEHR", "OUTMTX", "WFAIPPD", "COLLCLI",
  "TRAUML90", "SCNED", "CLUSTER"
)

Missing_con <- c("PLNTA", "GFEET", "CEAMT", "VIDVZ", "PRPM")

complete_con <- c("HOSPBD", "EXPTOT", "VEM", "FTMDTF", "FTRNTF", "ADC")
complete_cat <- c("CHC", "MAPP1", "MAPP18", "MAPP20")


# Define the binary list
cate_common_list <- c("ID", "CNTYNAME", "CBSANAME", "CBSACODE") # Common variables
cate_list <- c("TRAUML90", "SCNED", "CLUSTER")
binary_list <- setdiff(Keep_cate, c(cate_common_list, cate_list))
binary_cate_list <- c(cate_list, binary_list)

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


merged_AHA$Missing <- ifelse(is.na(merged_AHA$COUTRHOS), 1, 0)


# Reshape to long format for faceted plotting
df_long <- merged_AHA %>%
  select(Missing, all_of(complete_con)) %>%
  pivot_longer(
    cols = all_of(complete_con),
    names_to = "Variable",
    values_to = "Value"
  )

# Create boxplots for each variable, grouped by Missing
ggplot(df_long, aes(x = factor(Missing), y = Value, fill = factor(Missing))) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~Variable, scales = "free_y") +
  labs(
    x = "Missing (0 = Not Missing, 1 = Missing)",
    y = "Value",
    title = "Distribution of Variables by Missing Status"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Reshape to long format
df_long_cat <- merged_AHA %>%
  select(Missing, all_of(complete_cat)) %>%
  pivot_longer(
    cols = all_of(complete_cat),
    names_to = "Variable",
    values_to = "Value"
  )

# Bar plot: distribution of binary variable values grouped by Missing
ggplot(df_long_cat, aes(x = factor(Value), fill = factor(Missing))) +
  geom_bar(position = "dodge") +
  facet_wrap(~Variable, scales = "free_y") +
  labs(
    x = "Value of Binary Variable",
    y = "Count",
    fill = "Missing",
    title = "Distribution of Binary Variables by Missing Status"
  ) +
  theme_bw()


# Look at missingness structure -------------------------------------------

# View structure
# glimpse(merged_AHA)
skim(merged_AHA[Keep_cate])
skim(merged_AHA[Keep_numeric])

all_missing_vars <- c(Missing_cate, Missing_con)

# 1. Create missingness indicator variables
for (var in all_missing_vars) {
  merged_AHA[[paste0(var, "_miss")]] <- ifelse(is.na(merged_AHA[[var]]), 1, 0)
}

# 2. Run ANOVA tests and store results
results <- data.frame(
  variable = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in all_missing_vars) {
  miss_var <- paste0(var, "_miss")

  # ANOVA: EXPTOT ~ missing indicator
  fit <- aov(EXPTOT ~ as.factor(merged_AHA[[miss_var]]), data = merged_AHA)
  pval <- summary(fit)[[1]][["Pr(>F)"]][1]

  results <- rbind(results, data.frame(variable = var, p_value = pval))
}

# 3. Check significant associations (e.g., p < 0.05)
sig_vars <- results %>% filter(p_value < 0.05)
print(sig_vars)

# 4. Plot EXPTOT distribution for significant variables
for (var in sig_vars$variable) {
  miss_var <- paste0(var, "_miss")

  # Compute group means and SE
  summary_df <- merged_AHA %>%
    group_by(!!sym(miss_var)) %>%
    summarise(
      mean_exptot = mean(EXPTOT, na.rm = TRUE),
      se_exptot = sd(EXPTOT, na.rm = TRUE) / sqrt(n())
    )

  p <- ggplot(summary_df, aes(x = as.factor(!!sym(miss_var)), y = mean_exptot, fill = as.factor(!!sym(miss_var)))) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = mean_exptot - se_exptot, ymax = mean_exptot + se_exptot),
      width = 0.2, color = "black"
    ) +
    labs(
      title = paste("Mean EXPTOT by Missingness in", var),
      x = "Missing Indicator (0 = observed, 1 = missing)",
      y = "Mean EXPTOT"
    ) +
    scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
    theme_minimal()

  print(p)
}


# Create complete case df -------------------------------------------------

# Convert the variable types
merged_AHA_clean <- merged_AHA %>%
  mutate(across(all_of(binary_cate_list), as.factor)) %>%
  mutate(across(all_of(Keep_numeric), ~ suppressWarnings(as.numeric(.))))

features2 <- c(
  "ADC", "FTMDTF", "CEAMT", "VIDVZ", "PRPM", "FTRNTF", "GFEET", "PLNTA",
  "CHC", "MAPP1", "MAPP18", "MAPP20", "IINSPT", "CMRPAY", "FAMADV",
  "COUTRHOS", "FITCHOS", "HLTHSHOS", "HLTRHOS",
  "NUTRPHOS", "ONCOLHOS", "PALHOS", "SOCEHR", "OUTMTX", "WFAIPPD", "COLLCLI",
  "TRAUML90", "SCNED", "CLUSTER", "Year"
) # Only know this from non-centered results, but not centering for comparison

Keep_numeric_ml <- c(
  "HOSPBD", "VEM", "FTMDTF", "FTRNTF", "ADC",
  "PLNTA", "GFEET", "CEAMT", "VIDVZ", "PRPM"
)

Keep_numeric <- Keep_numeric[Keep_numeric != "EXPTOT"]

complete_df <- merged_AHA_clean[c("EXPTOT", features2)]
complete_df <- complete_df[complete.cases(complete_df[, c("EXPTOT", features2)]), ]
# Apply log1p to numeric features

complete_df[intersect(Keep_numeric_ml, features2)] <- lapply(complete_df[intersect(Keep_numeric_ml, features2)], log1p)

complete_df_std <- complete_df

# Calculate complete case mean and SD for each variable in Keep_numeric
standardization_params <- data.frame(
  variable = intersect(Keep_numeric, names(complete_df)),
  mean_cc = NA,
  sd_cc = NA
)

for (var in intersect(Keep_numeric, names(complete_df))) {
  standardization_params$mean_cc[standardization_params$variable == var] <- mean(complete_df[[var]], na.rm = TRUE)
  standardization_params$sd_cc[standardization_params$variable == var] <- sd(complete_df[[var]], na.rm = TRUE)

  complete_df_std[[var]] <- (complete_df[[var]] -
    standardization_params$mean_cc[standardization_params$variable == var]) /
    standardization_params$sd_cc[standardization_params$variable == var]
}


# Perform imputation ------------------------------------------------------


# Variables with missing data to adjust (continuous variables)
variables_to_adjust <- Missing_con # c('PLNTA', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM')

# Run multiple imputation using random forest
imp <- mice(merged_AHA_clean[c(binary_cate_list, Keep_numeric)],
  method = "rf", m = 5, maxit = 25, seed = 500
)

# Quick look at imputations
summary(imp)

# Get datasets
EXPTOT_set <- merged_AHA[c("EXPTOT")]

## imputed datasets with those common variables
common_df <- merged_AHA_clean[cate_common_list]
complete_data_1 <- cbind(common_df, complete(imp, 1), EXPTOT_set)
complete_data_2 <- cbind(common_df, complete(imp, 2), EXPTOT_set)
complete_data_3 <- cbind(common_df, complete(imp, 3), EXPTOT_set)
complete_data_4 <- cbind(common_df, complete(imp, 4), EXPTOT_set)
complete_data_5 <- cbind(common_df, complete(imp, 5), EXPTOT_set)

complete_data_1_std <- complete_data_1
complete_data_2_std <- complete_data_2
complete_data_3_std <- complete_data_3
complete_data_4_std <- complete_data_4
complete_data_5_std <- complete_data_5

for (var in intersect(Keep_numeric, names(complete_df))) {
  mean_cc <- standardization_params$mean_cc[standardization_params$variable == var]
  sd_cc <- standardization_params$sd_cc[standardization_params$variable == var]

  complete_data_1_std[[var]] <- (complete_data_1[[var]] - mean_cc) / sd_cc
  complete_data_2_std[[var]] <- (complete_data_2[[var]] - mean_cc) / sd_cc
  complete_data_3_std[[var]] <- (complete_data_3[[var]] - mean_cc) / sd_cc
  complete_data_4_std[[var]] <- (complete_data_4[[var]] - mean_cc) / sd_cc
  complete_data_5_std[[var]] <- (complete_data_5[[var]] - mean_cc) / sd_cc
}

complete_data_list <- lapply(1:5, function(i) {
  df <- bind_cols(common_df, complete(imp, i), EXPTOT_set) # stays as data.frame

  # Apply log1p to numeric features
  df[Keep_numeric_ml] <- lapply(df[Keep_numeric_ml], log1p)

  # Standardize using complete case parameters
  for (var in intersect(Keep_numeric, names(complete_df))) {
    mean_cc <- standardization_params$mean_cc[standardization_params$variable == var]
    sd_cc <- standardization_params$sd_cc[standardization_params$variable == var]

    # Standardize the log-transformed values
    df[[var]] <- (df[[var]] - mean_cc) / sd_cc
  }

  df
})


saveRDS(complete_data_list, "./Results/complete_data_list.rds")


# Calculate standard deviations of observed data for each continuous variable
observed_sds <- sapply(variables_to_adjust, function(var) {
  tmp <- complete_data_list[[1]] # Doesn't matter which one we are grabbing, all the same
  tmp <- tmp[which(!is.na(merged_AHA_clean[, var])), var]
  sd(data.frame(tmp)[, 1])
})


# Create initial model with all covariates --------------------------------


# Define your features
features <- c(
  "HOSPBD", "VEM", "FTMDTF", "FTRNTF", "ADC", "PLNTA", "GFEET", "CEAMT", "VIDVZ", "PRPM",
  "CHC", "MAPP1", "MAPP18", "MAPP20", "IINSPT", "CMRPAY", "FAMADV",
  "COUTRHOS", "FITCHOS", "HLTHSHOS", "HLTRHOS", "EMDEPHOS",
  "NUTRPHOS", "ONCOLHOS", "PALHOS", "SOCEHR", "OUTMTX", "WFAIPPD", "COLLCLI",
  "TRAUML90", "SCNED", "CLUSTER", "Year"
)

# Create the formula
formula <- as.formula(paste("log(EXPTOT) ~", paste(features, collapse = " + ")))

# Fit linear models on each dataset in the list
lm_models <- lapply(complete_data_list, function(df) {
  lm(formula, data = df)
})

# Check VIF
model_vif <- lapply(lm_models, vif)
model_vif[[1]]
model_vif[[2]]
model_vif[[3]]
model_vif[[4]]
model_vif[[5]]


# Refine model based on VIF -----------------------------------------------

# Define your features
features2 <- c(
  "ADC", "FTMDTF", "CEAMT", "VIDVZ", "PRPM", "FTRNTF", "GFEET", "PLNTA",
  "CHC", "MAPP1", "MAPP18", "MAPP20", "IINSPT", "CMRPAY", "FAMADV",
  "COUTRHOS", "FITCHOS", "HLTHSHOS", "HLTRHOS",
  "NUTRPHOS", "ONCOLHOS", "PALHOS", "SOCEHR", "OUTMTX", "WFAIPPD", "COLLCLI",
  "TRAUML90", "SCNED", "CLUSTER", "Year"
)

# Create the formula
formula2 <- as.formula(paste("log(EXPTOT) ~", paste(features2, collapse = " + ")))

# 1. Fit linear models on each dataset in the list
lm_models2 <- lapply(complete_data_list, function(df) {
  lm(formula2, data = df)
})

complete_data_1$resid <- resid(lm_models2[[1]])
complete_data_1$resid <- resid(lm_models2[[2]])
complete_data_1$resid <- resid(lm_models2[[3]])
complete_data_1$resid <- resid(lm_models2[[4]])
complete_data_1$resid <- resid(lm_models2[[5]])

saveRDS(lm_models2, "./Results/Imputed_models.rds")

# 2. Extract coefficients and covariance matrices using lapply
betas_list <- lapply(lm_models2, FUN = coef)
cov_matrices_list <- lapply(lm_models2, FUN = vcov)

# 3. Combine the results
pooled_results <- miceadds::pool_mi(qhat = betas_list, u = cov_matrices_list)

summary(pooled_results)

# Loop through the models and generate 4-in-1 diagnostic plots
# Loop over each imputed regression model
for (i in 1:5) {
  model <- lm_models2[[i]]
  df <- complete_data_list[[i]]

  # Extract fitted values and residuals
  fitted_vals <- fitted(model)
  residuals_vals <- resid(model)

  # Open PNG device
  png(
    filename = paste0("./Figures/Residual_Plots_LM2_", i, ".png"),
    width = 1000, height = 900
  )

  # Set layout and margins
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2), oma = c(0, 0, 3, 0), cex = 1.4)

  # Plot 1: Residuals vs Fitted
  plot(fitted_vals, residuals_vals,
    main = "Residuals vs Fitted",
    xlab = "Fitted values", ylab = "Residuals"
  )
  abline(h = 0, col = "red", lty = 2)

  # Plot 2: Normal Q-Q
  qqnorm(residuals_vals, main = "Normal Q-Q")
  qqline(residuals_vals, col = "red")

  # Plot 3: Scale-Location
  sqrt_abs_res <- sqrt(abs(residuals_vals))
  plot(fitted_vals, sqrt_abs_res,
    main = "Scale-Location",
    xlab = "Fitted values", ylab = expression(sqrt("|Residuals|"))
  )
  abline(h = 0, col = "red", lty = 2)

  # Plot 4: Histogram of Residuals
  hist(residuals_vals,
    main = "Residuals Histogram",
    xlab = "Residuals", breaks = 30, col = "gray"
  )

  # Add overall title
  mtext(paste("Residual Diagnostics - Linear Model - Dataset", i),
    outer = TRUE, cex = 1.5, font = 2
  )

  # Close PNG device
  dev.off()
}


#### Calculate the metrics
# Initialize lists to store results
mse_list <- c()
mae_list <- c()
r2_list <- c()

# Loop through each model
for (i in 1:5) {
  model <- lm_models2[[i]]
  df <- complete_data_list[[i]]

  print(df)
  # True values and predictions
  actual <- log(df$EXPTOT)
  predicted <- predict(model, newdata = df)

  # Calculate metrics
  mse_val <- mean((actual - predicted)^2)
  mae_val <- mean(abs(actual - predicted))
  r2_val <- summary(model)$r.squared

  # Store metrics
  mse_list[i] <- mse_val
  mae_list[i] <- mae_val
  r2_list[i] <- r2_val
}

# Create a summary data frame
metrics_df <- data.frame(
  Dataset = 1:5,
  MSE = round(mse_list, 4),
  MAE = round(mae_list, 4),
  R2 = round(r2_list, 4)
)

# Print the result
print(metrics_df)

# Output the data to csv
results_path <- "./Results"
write.csv(complete_data_1, file = file.path(results_path, "complete_data_NoEXP_1.csv"), row.names = FALSE)
write.csv(complete_data_2, file = file.path(results_path, "complete_data_NoEXP_2.csv"), row.names = FALSE)
write.csv(complete_data_3, file = file.path(results_path, "complete_data_NoEXP_3.csv"), row.names = FALSE)
write.csv(complete_data_4, file = file.path(results_path, "complete_data_NoEXP_4.csv"), row.names = FALSE)
write.csv(complete_data_5, file = file.path(results_path, "complete_data_NoEXP_5.csv"), row.names = FALSE)

# Test on the imputated data
# plot(imp) # visualize the trace lines for the means and standard deviations of the imputed values across iterations for each variable. You want to see these lines mixing well and converging to a stable range, indicating that the imputation algorithm has stabilized
# dev.off()
# densityplot(imp) # compare the distribution of the observed data with the imputed data for each variable. For continuous variables, the imputed data's density should broadly follow the observed data's density.
# dev.off()
# stripplot(imp) # imputed values for each variable across different imputations, allowing you to spot outliers or unusual patterns.
# dev.off()


# Compare to complete case analysis ---------------------------------------
complete_fit <- lm(formula2, data = complete_df_std)


# Get CBSA codes (need to match residuals to spatial locations)
complete_cbsa <- merged_AHA_clean[c("EXPTOT", features2)]
complete_cbsa <- complete_cbsa[complete.cases(complete_cbsa[, c("EXPTOT", features2)]), ]
complete_cbsa_codes <- merged_AHA_clean[complete.cases(merged_AHA_clean[c("EXPTOT", features2)]), "CBSACODE"]

# Combine into a dataframe for Moran's I analysis
complete_case_morans_data <- data.frame(
  CBSACODE = complete_cbsa_codes,
  residuals = resid(complete_fit)
)

saveRDS(complete_fit, "./Results/Complete_model.rds")
saveRDS(complete_case_morans_data, "./Results/Complete_resid_spatial.rds")


# Extract coefficients from the complete model
complete_tidy <- broom::tidy(complete_fit, conf.int = TRUE) %>%
  mutate(source = "Complete Data")

# Extract coefficients from the pooled MI results
tmp <- as.data.frame(summary(pooled_results))
tmp$term <- rownames(tmp)
pooled_tidy <- tmp %>%
  rename(
    term = term,
    estimate = results,
    std.error = se,
    conf.low = `(lower`,
    conf.high = `upper)`
  ) %>%
  mutate(source = "Imputed Data")

shared_terms <- pooled_tidy$term
coef_compare <- bind_rows(complete_tidy, pooled_tidy) %>%
  filter(term %in% shared_terms, term != "(Intercept)")

# Order terms by complete-model estimates
term_order <- coef_compare %>%
  filter(source == "Imputed Data") %>%
  arrange(estimate) %>%
  pull(term)

coef_compare$term <- factor(coef_compare$term, levels = term_order)

# Plot in alphabetical order
coef_compare$term <- factor(coef_compare$term, levels = sort(levels(coef_compare$term), decreasing = TRUE))

# Extract coefficients for comparison
complete_coefs <- coef_compare %>%
  filter(source == "Complete Data") %>%
  select(term, estimate) %>%
  rename(estimate_complete = estimate)

imputed_coefs <- coef_compare %>%
  filter(source == "Imputed Data") %>%
  select(term, estimate) %>%
  rename(estimate_imputed = estimate)

# Calculate percent and absolute differences (only for variables in both)
diff_data <- complete_coefs %>%
  inner_join(imputed_coefs, by = "term") %>%
  mutate(
    absolute_diff = estimate_imputed - estimate_complete,
    percent_diff = ifelse(
      estimate_complete != 0,
      (absolute_diff / abs(estimate_complete)) * 100,
      NA
    ),
    term_label = paste0(
      term,
      " (",
      sprintf("%.0f%%", percent_diff),
      ", ",
      sprintf("%.3f", absolute_diff),
      ")"
    )
  )

# Add variables only in imputed data
imputed_only <- coef_compare %>%
  filter(source == "Imputed Data") %>%
  filter(!term %in% diff_data$term) %>%
  mutate(term_label = paste0(term, " (NA, NA)")) %>%
  select(term, term_label) %>%
  distinct()

coef_compare_with_labels <- coef_compare %>%
  left_join(bind_rows(diff_data %>% select(term, term_label), imputed_only), by = "term") %>%
  mutate(term_display = ifelse(!is.na(term_label), term_label, as.character(term)))

# Reorder levels alphabetically by the original term name
coef_compare_with_labels$term_display <- factor(coef_compare_with_labels$term_display,
  levels = sort(unique(coef_compare_with_labels$term_display), decreasing = TRUE)
)

gg_coef <- ggplot(coef_compare_with_labels, aes(x = estimate, y = term_display, color = source)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.2), width = 0.8, linewidth = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 13) +
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    color = NULL,
    title = "Comparison of Coefficient Estimates: Complete vs Imputed Data"
  ) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(gg_coef)
ggsave("./Figures/complete_coef_comp.png", gg_coef, width = 10, height = 10)

## Interesting things
tmp <- merged_AHA_clean[complete.cases(merged_AHA_clean[features2]), ]
summary(tmp$CHC)
summary(tmp$SCNED)

# Sensitivity Analysis ----------------------------------------------------
# Using delta method for continuous covariates

# Define delta values to test
# delta = 0: MAR assumption (baseline)
# delta > 0: missing values systematically HIGHER
# delta < 0: missing values systematically LOWER
delta_values <- c(-2, -1.5, -1, -0.5, -0.3, 0, 0.3, 0.5, 1)


# Store all sensitivity analysis results
sensitivity_results <- list()

# For each delta value, apply SD-scaled adjustment and re-fit models
# For each variable with missingness, do sensitivity analysis separately
for (var_adjust in variables_to_adjust) {
  sensitivity_results_var <- list()

  # For each delta value, apply SD-scaled adjustment and re-fit models
  for (delta in delta_values) {
    # Get all 5 completed datasets and apply delta adjustment
    adjusted_complete_data_list <- lapply(1:5, function(i) {
      # Get the i-th completed dataset
      dat_imputed <- complete_data_list[[i]]

      dat_raw <- dat_imputed

      # Apply delta adjustment scaled by SD to ONLY the current variable
      orig_missing_idx <- is.na(merged_AHA_clean[[var_adjust]])
      if (sum(orig_missing_idx) > 0) {
        # Scale delta by observed SD: delta * SD
        adjustment <- delta * observed_sds[var_adjust]
        dat_raw[orig_missing_idx, var_adjust] <- dat_raw[orig_missing_idx, var_adjust] + adjustment
      }

      return(dat_raw)
    })

    # Fit models to delta-adjusted datasets
    lm_models_delta <- lapply(adjusted_complete_data_list, function(df) {
      lm(formula2, data = df)
    })

    # Extract coefficients and covariance matrices
    betas_list_delta <- lapply(lm_models_delta, FUN = coef)
    cov_matrices_list_delta <- lapply(lm_models_delta, FUN = vcov)

    # Pool results
    pooled_results_delta <- miceadds::pool_mi(
      qhat = betas_list_delta,
      u = cov_matrices_list_delta
    )

    # Extract results into dataframe
    delta_results_df <- as.data.frame(summary(pooled_results_delta)) %>%
      rownames_to_column("term") %>%
      select(term, results, se) %>%
      rename(estimate = results) %>%
      mutate(
        delta = delta,
        adjusted_variable = var_adjust,
        scenario = paste0(var_adjust, " (delta = ", delta, ")")
      )

    sensitivity_results_var[[as.character(delta)]] <- delta_results_df
  }

  # Combine results for this variable
  assign(
    paste0("sensitivity_results_", var_adjust),
    do.call(rbind, sensitivity_results_var)
  )
}

# Combine all sensitivity results into one dataframe
all_sensitivity_results <- do.call(rbind, mget(paste0("sensitivity_results_", variables_to_adjust))) %>%
  as_tibble() %>%
  filter(term != "(Intercept)")


# Create tipping point plot -----------------------------------------------

# For each variable, identify delta range where significance is maintained
# Calculate p-values for all results
tipping_analysis <- all_sensitivity_results %>%
  mutate(
    t_stat = estimate / se,
    p_value = 2 * (1 - pt(abs(t_stat), df = nrow(merged_AHA_clean) - 30)),
    significant = ifelse(p_value < 0.05, "Yes", "No")
  )

# Find tipping points for key variables (across all adjusted variables)
tipping_summary <- tipping_analysis %>%
  filter(term == adjusted_variable) %>% # Only keep matching variables
  group_by(term, adjusted_variable) %>%
  summarize(
    estimate_at_delta_0 = estimate[delta == 0],
    min_delta = min(delta),
    max_delta = max(delta),
    min_p_value = min(p_value),
    max_p_value = max(p_value),
    significant_across_all = all(p_value < 0.05),
    significant_range = {
      sig_deltas <- delta[p_value < 0.05]
      if (length(sig_deltas) > 0) {
        paste0("[", round(min(sig_deltas), 2), ", ", round(max(sig_deltas), 2), "]")
      } else {
        "Never significant"
      }
    },
    .groups = "drop"
  )

# Tipping point plot (faceted by adjusted variable)
tipping_plot <- tipping_analysis %>%
  filter(term == adjusted_variable) %>%
  mutate(
    significant = ifelse(p_value < 0.05, "Significant", "Not Significant"),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se
  ) %>%
  ggplot(aes(x = delta, y = estimate, color = significant)) +
  geom_point(size = 2.5) +
  geom_line(color = "gray50", linewidth = 0.4, alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1, linewidth = 0.6, alpha = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5, alpha = 0.5) +
  facet_wrap(~adjusted_variable, ncol = 3, scales = "free_y") +
  scale_color_manual(values = c("Significant" = "steelblue", "Not Significant" = "coral")) +
  labs(
    title = "Tipping Point Analysis: Coefficient Estimates Across MNAR Scenarios",
    # subtitle = "Red dashed line indicates MAR assumption (delta = 0)",
    x = "Delta (MNAR Bias Offset)",
    y = "Coefficient Estimate ± 95% CI",
    color = "p < 0.05"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "gray60"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    axis.text = element_text(size = 11)
  )

print(tipping_plot)

# Save tipping point plot
ggsave("./Figures/tipping_point_analysis.png", tipping_plot, width = 16, height = 12, dpi = 300)

# Save all results
save.image("./Results/Final_Analysis.RData")
