# ====================================================================
# General

# --------------------------------------------------------------------
# Installs necessary packages

source("GeneralUtils/setup_packages.R")
install_packages_from_file()

# --------------------------------------------------------------------
# Necessary libaries

library("tidyverse")
library("tsibble")
library("tseries")
library("ggplot2")
library("feasts")

# --------------------------------------------------------------------
# Data Preparation 

source("GeneralUtils/load_data.R")
source("GeneralUtils/structure_analysis.R")

# Read Data
ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  mutate(level = vapply(industry_code, get_level, integer(1)))

# Create tsibble
ifo_tsbl <- ifo_tbl %>% as_tsibble(key = industry_code, index = date)

# Create 1st Diff tsibble
ifo_tsbl_d1 <- ifo_tsbl %>%
  group_by(industry_code) %>%
  transmute(
    date = date,
    across(where(is.numeric) & !any_of("date"), ~ difference(.x, lag = 1))
  ) %>%
  ungroup()

# Set Main Index
main_index <- "C0000000"

# ====================================================================
# Setup

# --------------------------------------------------------------------
# Level 2

# Filter Level 0 and 2 for sector aggregates
ifo_tsbl_l2 <- ifo_tsbl %>%
  filter(level %in% c(0, 2)) %>%
  select(date, industry_code, KLD)

# Transform to wide format
ifo_tsbl_l2_wide <- ifo_tsbl_l2 %>%
  pivot_wider(names_from = industry_code, values_from = KLD)


# --------------------------------------------------------------------
# Level 2 - 1st Diff

# Filter Level 0 and 2 for sector aggregates
ifo_tsbl_d1_l2 <- ifo_tsbl_d1 %>%
  filter(level %in% c(0, 2)) %>%
  select(date, industry_code, KLD)

# Transform to wide format
ifo_tsbl_d1_l2_wide <- ifo_tsbl_d1_l2 %>%
  pivot_wider(names_from = industry_code, values_from = KLD)


# ====================================================================
# Test Stationarity and Cointegration

source("LagAnalysis/stationarity_cointegration.R")

# --------------------------------------------------------------------
# Level 2

# Test for unit root: ADF test for all columns (excluding date)
adf_results <- as_tibble(ifo_tsbl_l2_wide) %>% run_adf_tests()

# Test cointegration: Johansen Cointegration Test (VECM) with Main Index
cointegration_results <- as_tibble(ifo_tsbl_l2_wide) %>% 
  run_cointegration_tests(main_index = main_index)

# Merge both tables
diagnostic_tbl <- adf_results %>%
  inner_join(cointegration_results, by = "industry_code") %>%
  mutate(
    model_type = case_when(
      adf_is_stationary ~ "VAR",
      !adf_is_stationary & coint_cointegrated ~ "VECM",
      !adf_is_stationary & !coint_cointegrated ~ "DIFF+VAR",
      TRUE ~ NA_character_
    )
  )


# --------------------------------------------------------------------
# Level 2 - 1st Diff

# Test for unit root: ADF test for all columns (excluding date)
adf_results_d1 <- as_tibble(ifo_tsbl_d1_l2_wide) %>% run_adf_tests()

# Test cointegration: Johansen Cointegration Test (VECM) with Main Index
cointegration_results_d1 <- as_tibble(ifo_tsbl_d1_l2_wide) %>% 
  run_cointegration_tests(main_index = main_index)

# Merge both tables
diagnostic_tbl_d1 <- adf_results_d1 %>%
  inner_join(cointegration_results_d1, by = "industry_code") %>%
  mutate(
    model_type = case_when(
      adf_is_stationary ~ "VAR",
      !adf_is_stationary & coint_cointegrated ~ "VECM",
      !adf_is_stationary & !coint_cointegrated ~ "DIFF+VAR",
      TRUE ~ NA_character_
    )
  )


# ====================================================================
# Cross-Correlation Analysis

# --------------------------------------------------------------------
# Setup 

# Set target list
l2_targets <- setdiff(colnames(ifo_tsbl_l2_wide), c("date", main_index))

# Max lag for tests
max_lag <- 12

# Function to compute full lag correlation for one sector
get_ccf_full <- function(tsbl, main_index, target_code) {
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Remove NAs
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Compute CCF
  ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
  
  tibble(
    lag = ccf_obj$lag,
    correlation = ccf_obj$acf,
    industry_code = target_code
  )
}


# --------------------------------------------------------------------
# Level 2

# Build full tibble
ccf_full_tbl <- map_dfr(
  l2_targets,
  ~get_ccf_full(tsbl = ifo_tsbl_l2_wide, main_index = main_index, target_code = .x)
)

# Extract peak lead/lag per Industry
ccf_peak_tbl <- ccf_full_tbl %>%
  group_by(industry_code) %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(peak_lag = lag, peak_corr = correlation)

## Visualization

# Sort full ccf tbl
ccf_full_sorted <- ccf_full_tbl %>%
  left_join(ccf_peak_tbl, by = "industry_code") %>%
  arrange(peak_lag, industry_code) %>%
  mutate(industry_code = factor(industry_code, levels = unique(industry_code)))

# Create markers for heatmap
ccf_peak_markers <- ccf_peak_tbl %>%
  mutate(industry_code = factor(industry_code, levels = levels(ccf_full_sorted$industry_code)))

# Plot heatmap
ggplot(ccf_full_sorted, aes(x = industry_code, y = lag, fill = correlation)) +
  geom_tile() +  # Base heatmap
  geom_tile(data = ccf_peak_markers, aes(x = industry_code, y = peak_lag),
            color = "yellow", fill = NA, linewidth = 0.8, width = 0.95, height = 0.95, inherit.aes = FALSE) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "CCF Heatmap: Highlighted Peak Correlations",
       x = "Industry Code (sorted by peak lag)", y = "Lag (months)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# --------------------------------------------------------------------
# Level 2 - 1st Diff

# Build full tibble
ccf_full_tbl <- map_dfr(
  l2_targets,
  ~get_ccf_full(tsbl = ifo_tsbl_d1_l2_wide, main_index = main_index, target_code = .x)
)

# Extract peak lead/lag per Industry
ccf_peak_tbl <- ccf_full_tbl %>%
  group_by(industry_code) %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(peak_lag = lag, peak_corr = correlation)

## Visualization

# Sort full ccf tbl
ccf_full_sorted <- ccf_full_tbl %>%
  left_join(ccf_peak_tbl, by = "industry_code") %>%
  arrange(peak_lag, industry_code) %>%
  mutate(industry_code = factor(industry_code, levels = unique(industry_code)))

# Create markers for heatmap
ccf_peak_markers <- ccf_peak_tbl %>%
  mutate(industry_code = factor(industry_code, levels = levels(ccf_full_sorted$industry_code)))

# Plot heatmap
ggplot(ccf_full_sorted, aes(x = industry_code, y = lag, fill = correlation)) +
  geom_tile() +  # Base heatmap
  geom_tile(data = ccf_peak_markers, aes(x = industry_code, y = peak_lag),
            color = "yellow", fill = NA, linewidth = 0.8, width = 0.95, height = 0.95, inherit.aes = FALSE) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "CCF Heatmap: Highlighted Peak Correlations",
       x = "Industry Code (sorted by peak lag)", y = "Lag (months)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
