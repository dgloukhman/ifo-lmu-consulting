# ====================================================================
# General

# --------------------------------------------------------------------
# Installs necessary packages
source("utils/setup_packages.R")
install_packages_from_file()

# --------------------------------------------------------------------
# Source Utility Function
source("utils/load_data.R")
source("LagAnalysis/lag_utils.R")
source("LagAnalysis/lag_functions.R")
source("LagAnalysis/stationarity_cointegration.R")



# --------------------------------------------------------------------
# Necessary libaries
library("tidyverse")
library("tsibble")
library("future")

# --------------------------------------------------------------------
# Enable parallel processing
plan(multisession, workers = 8)

# --------------------------------------------------------------------
# Data Preparation

# Read Data
ifo_tsbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  mutate(date = yearmonth(date)) %>%      # Maybe push to read_ifo_data()
  as_tsibble(key = industry_code, index = date)

# Read Data Alternative
# ifo_tsbl <- read_csv("Data/ifo_tsbl.csv") %>%
#   as_tsibble(key = industry_code, index = date)

# --------------------------------------------------------------------
# Setup 

# Create full tsibble (with 1st Differences)
ifo_tsbl_full <- ifo_tsbl %>%
  group_by(industry_code) %>%
  mutate(
    across(where(is.numeric) & !any_of(c("date", "level")),
           ~ difference(.x, lag = 1), .names = "{.col}-diff1")
  ) %>%
  ungroup()

# Pivot Wider
ifo_tsbl_full_wide <- ifo_tsbl_full %>%
  select(-level) %>%
  pivot_wider(
    id_cols = date,
    names_from = industry_code,
    values_from = -c(date, industry_code)
  )

# ====================================================================
# Test Stationarity

# --------------------------------------------------------------------
# Full Stationarity Test

# Test for unit root: ADF test for all columns (excluding date)
adf_results_full <- ifo_tsbl_full_wide %>% 
  as_tibble() %>% 
  run_adf_tests() 

# Postprocessing of adf Results
adf_results_full <- adf_results_full %>%
  adf_postprocess()

# Save Output as csv file
write_csv(adf_results_full, here("Data/corr_results/adf_results_full.csv"))


# ====================================================================
# Correlation Analysis

# --------------------------------------------------------------------
# Setup

# Main Index
main_index = "KLD_C0000000"

# Max lag for tests
max_lag <- 12

# Set target indices for ccf calclation
target_codes <- ifo_tsbl_full_wide %>%
  names() %>%
  setdiff(c("date", "date_window_end", "window_id", main_index))

# --------------------------------------------------------------------
# Cross Correlation

# Build full tibble
ccf_results_full <- future_map_dfr(
  target_codes,
  ~ get_ccf_full(
    tsbl = ifo_tsbl_full_wide,
    main_index = main_index,
    target_code = .x,
    max_lag = max_lag
  ),
  .options = furrr_options(seed = TRUE))

# Postprocessing of ccf Results
ccf_results_full <- ccf_results_full %>%
  ccf_postprocess()

# Extract peak lead/lag per Industry
ccf_results_full_peak <- ccf_results_full %>%
  filter(lag %in% c(-6:6)) %>% 
  group_by(ID) %>%
  slice_max(order_by = abs(corr), n = 1, with_ties = FALSE) %>%
  ungroup()

# Save Output as csv file
write_csv(ccf_results_full, here("Data/corr_results/ccf_results_full.csv"))
write_csv(ccf_results_full_peak, here("Data/corr_results/ccf_results_full_peak.csv"))

# --------------------------------------------------------------------
# Distance Correlation Analysis

# Build full tibble
dcor_results_full <- future_map_dfr(
  target_codes,
  ~ get_dcor_full(
    tsbl = ifo_tsbl_full_wide,
    main_index = main_index,
    target_code = .x,
    max_lag = max_lag
  ),
  .options = furrr_options(seed = TRUE))

# Postprocessing of ccf Results
dcor_results_full <- dcor_results_full %>%
  dcor_postprocess()

# Extract peak lead/lag per Industry
dcor_results_full_peak <- dcor_results_full %>%
  filter(lag %in% c(-6:6)) %>% 
  group_by(ID) %>%
  slice_max(order_by = abs(corr), n = 1, with_ties = FALSE) %>%
  ungroup()

# Save Output as csv file
write_csv(dcor_results_full, here("Data/corr_results/dcor_results_full.csv"))
write_csv(dcor_results_full_peak, here("Data/corr_results/dcor_results_full_peak.csv"))
