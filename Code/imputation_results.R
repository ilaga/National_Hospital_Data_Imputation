# Load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(mice)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggridges)
library(viridis)
library(here)


result_files <- list.files("./Results", pattern = "^imp_res_.*\\.rds$", full.names = TRUE)

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

Missing_binary <- c(
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
  "COLLCLI"
)

Missing_cate <- Missing_cate[-which(Missing_cate %in% Missing_binary)]


# List all .rds files in the Results folder
all_files <- list.files("./Results", pattern = "\\.rds$", full.names = TRUE)


# Numeric variables only --------------------------------------------------

# Keep only files where the variable part matches one of the Missing_con
continuous_files <- all_files[
  str_detect(all_files, paste0("imp_res_(", paste(Missing_con, collapse = "|"), ")_"))
]

# Summarize result file
summarize_imputation_result <- function(file_path) {
  res <- readRDS(file_path)

  # Extract variable and method from filename
  info <- str_match(basename(file_path), "imp_res_(.*?)_(.*?)\\.rds$")
  variable <- info[2]
  method <- info[3]

  # Initialize storage for all folds
  fold_summaries <- map_dfr(names(res), function(fold_name) {
    fold <- res[[fold_name]]

    observed <- unlist(fold$observed)
    imputed_mat <- fold$imputed # matrix of imputed values (rows = masked obs, cols = imputations)

    # Mean across imputations
    imputed_mean <- rowMeans(imputed_mat, na.rm = TRUE)

    # Compute performance metrics
    tibble(
      fold = fold_name,
      variable = variable,
      method = method,
      n = length(observed),
      bias = mean(imputed_mean - observed, na.rm = TRUE),
      rmse = sqrt(mean((imputed_mean - observed)^2, na.rm = TRUE)),
      cor = suppressWarnings(cor(observed, imputed_mean, use = "complete.obs"))
    )
  })

  # Average metrics across folds
  fold_summaries %>%
    summarise(
      variable = first(variable),
      method = first(method),
      n = mean(n),
      bias = mean(bias, na.rm = TRUE),
      mean_rmse = mean(rmse, na.rm = TRUE),
      sd_rmse = sd(rmse, na.rm = TRUE),
      cor = mean(cor, na.rm = TRUE)
    )
}


summary_continuous <- map_dfr(continuous_files, summarize_imputation_result)


summary_continuous %>%
  arrange(variable, mean_rmse) %>%
  print(n = Inf)

summary_continuous <- summary_continuous %>%
  group_by(variable) %>%
  mutate(method = reorder(method, mean_rmse))

gg_cont <- ggplot(summary_continuous, aes(x = method, y = mean_rmse)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 13) +
  labs(
    title = "RMSE across Imputation Methods for Continuous Variables",
    x = "Imputation Method",
    y = "RMSE"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  )


# Binary variables only ---------------------------------------------------

binary_files <- all_files[
  str_detect(all_files, paste0("imp_res_(", paste(Missing_binary, collapse = "|"), ")_"))
]


summarize_binary_result <- function(file_path) {
  res <- readRDS(file_path)

  # Extract variable and method from filename
  info <- str_match(basename(file_path), "imp_res_(.*?)_(.*?)\\.rds$")
  variable <- info[2]
  method <- info[3]

  # Aggregate across folds
  fold_summaries <- map_dfr(names(res), function(fold_name) {
    fold <- res[[fold_name]]

    observed <- as.numeric(as.character(unlist(fold$observed)))
    imputed_mat <- apply(fold$imputed, 2, as.character)
    imputed_mat <- apply(imputed_mat, 2, as.numeric) # rows = masked obs, cols = imputations

    # Mean across imputations
    imputed_mean <- rowMeans(imputed_mat, na.rm = TRUE)

    # Metrics for binary variables
    correct <- mean(round(imputed_mean) == observed, na.rm = TRUE)

    tibble(
      fold = fold_name,
      variable = variable,
      method = method,
      n = length(observed),
      bias = mean(imputed_mean - observed, na.rm = TRUE),
      rmse = sqrt(mean((imputed_mean - observed)^2, na.rm = TRUE)),
      cor = suppressWarnings(cor(observed, imputed_mean, use = "complete.obs")),
      accuracy = correct
    )
  })

  # Average across folds
  fold_summaries %>%
    summarise(
      variable = first(variable),
      method = first(method),
      n = mean(n),
      bias = mean(bias, na.rm = TRUE),
      mean_rmse = mean(rmse, na.rm = TRUE),
      sd_rmse = sd(rmse, na.rm = TRUE),
      cor = mean(cor, na.rm = TRUE),
      accuracy = mean(accuracy, na.rm = TRUE)
    )
}


summary_binary <- map_dfr(binary_files, summarize_binary_result)


summary_binary %>%
  arrange(variable, mean_rmse) %>%
  print(n = Inf)

summary_binary <- summary_binary %>%
  group_by(variable) %>%
  mutate(method = reorder(method, accuracy, decreasing = TRUE))


gg_bin <- ggplot(summary_binary, aes(x = method, y = accuracy)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 13) +
  labs(
    title = "Accuracy of Imputation Methods for Binary Variables",
    x = "Imputation Method",
    y = "Accuracy (proportion correctly imputed)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


summary_binary <- summary_binary %>%
  group_by(variable) %>%
  mutate(method = reorder(method, mean_rmse))


ggplot(summary_binary, aes(x = method, y = mean_rmse)) +
  geom_col(position = position_dodge(width = 0.7)) +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 13) +
  labs(
    title = "RMSE of Imputation Methods for Binary Variables",
    x = "Imputation Method",
    y = "RMSE"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()


ggplot(summary_binary, aes(x = mean_rmse, y = accuracy, color = method)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_bw(base_size = 13) +
  labs(
    title = "RMSE vs. Accuracy for Binary Imputation Methods",
    x = "Mean RMSE",
    y = "Accuracy"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed")


# Categorical variables only ----------------------------------------------

categorical_files <- all_files[
  str_detect(all_files, paste0("imp_res_(", paste(Missing_cate, collapse = "|"), ")_"))
]


summarize_categorical_result <- function(file_path) {
  res <- readRDS(file_path)

  # Extract variable and method from filename
  info <- str_match(basename(file_path), "imp_res_(.*?)_(.*?)\\.rds$")
  variable <- info[2]
  method <- info[3]

  fold_summaries <- map_dfr(names(res), function(fold_name) {
    fold <- res[[fold_name]]

    observed <- as.character(unlist(fold$observed))
    imputed_mat <- apply(fold$imputed, 2, as.character)

    # For each row (masked observation), compute proportion of imputations that match observed
    prop_correct <- rowMeans(imputed_mat == observed, na.rm = TRUE)

    tibble(
      fold = fold_name,
      variable = variable,
      method = method,
      n = length(observed),
      percent_correct = mean(prop_correct, na.rm = TRUE)
    )
  })

  # Average across folds
  fold_summaries %>%
    summarise(
      variable = first(variable),
      method = first(method),
      n = mean(n),
      percent_correct = mean(percent_correct, na.rm = TRUE)
    )
}


summary_categorical <- map_dfr(categorical_files, summarize_categorical_result)


summary_categorical %>%
  arrange(variable, desc(percent_correct)) %>%
  print(n = Inf)


summary_categorical <- summary_categorical %>%
  group_by(variable) %>%
  mutate(method = reorder(method, percent_correct, decreasing = TRUE))


# Bar plot of % correct by method per variable
gg_cat <- ggplot(summary_categorical, aes(x = method, y = percent_correct)) +
  geom_col(position = position_dodge(width = 0.7)) +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw(base_size = 13) +
  labs(
    title = "Accuracy of Imputation Methods for Categorical Variables",
    x = "Imputation Method",
    y = "Percent Correct"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Figures/imp_comp_cont.png", gg_cont, width = 12, height = 8)
ggsave("Figures/imp_comp_bin.png", gg_bin, width = 12, height = 8)
ggsave("Figures/imp_comp_cat.png", gg_cat, width = 12, height = 8)


# Summarize in table ------------------------------------------------------


library(dplyr)
library(tidyr)
library(flextable)
library(officer)


agg_table <- bind_rows(
  summary_continuous %>%
    group_by(method) %>%
    summarise(value = mean(mean_rmse, na.rm = TRUE)) %>%
    mutate(metric = "Continuous RMSE"),
  summary_binary %>%
    group_by(method) %>%
    summarise(value = mean(accuracy, na.rm = TRUE)) %>%
    mutate(metric = "Binary Accuracy"),
  summary_categorical %>%
    group_by(method) %>%
    summarise(value = mean(percent_correct, na.rm = TRUE)) %>%
    mutate(metric = "Categorical % Correct")
) %>%
  ungroup() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(method) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


bold_best <- function(ft, df, cols, lower_is_better = c(TRUE, FALSE, FALSE)) {
  for (i in seq_along(cols)) {
    col <- cols[i]
    if (lower_is_better[i]) {
      best_row <- which.min(df[[col]])
    } else {
      best_row <- which.max(df[[col]])
    }
    ft <- bold(ft, i = best_row, j = col, bold = TRUE, part = "body")
  }
  ft
}


ft <- flextable(agg_table) %>%
  set_caption("Aggregated Imputation Performance") %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 10)


ft <- bold_best(ft, agg_table, cols = 2:4, lower_is_better = c(TRUE, FALSE, FALSE))

doc <- read_docx() %>%
  body_add_par("Aggregated Imputation Performance Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Results/imputation_summary_compact.docx")
