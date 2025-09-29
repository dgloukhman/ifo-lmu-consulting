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
source("LagAnalysis/stationarity_cointegration.R")
source("LagAnalysis/lag_functions.R")


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

# --------------------------------------------------------------------
# Rolling Window Setup

# Create Rolling Window tsbl
ifo_tsbl_roll <- ifo_tsbl_full_wide %>%
  tsbl_roll_wide(window_size = 120, step = 1)

# Pivot wider Rolling Window
ifo_tsbl_roll_wide <- ifo_tsbl_roll %>% 
  select(-date_window_end) %>%
  pivot_wider(
    id_cols = date,
    names_from = window_id,
    values_from = -c(date, window_id)
  )


# ====================================================================
# Test Stationarity

# --------------------------------------------------------------------
# Rolling Window Stationarity Test

# Test for unit root: rolling ADF test for all columns (excluding date)
adf_results_roll <- ifo_tsbl_roll %>%
  as_tibble() %>%
  group_by(window_id) %>%
  group_split() %>%
  .[1:5] %>%                      # Option to limit input/run time for testing
  future_map_dfr(~ {
    df <- .x
    run_adf_tests(df) %>%
      mutate(date_window_end = df$date_window_end[1],
             window_id = df$window_id[1])
    },
  .progress = TRUE
  )

# Postprocessing of adf Results
adf_results_roll <- adf_results_roll %>%
  adf_postprocess() %>%
  mutate(ID = str_c(ID, date_window_end, sep = "_"))

# Save Output as temp data file
write_csv(adf_results_roll, here("Data/corr_results/adf_results_roll.csv"))
# adf_results_roll <- read_csv(here("Data/corr_results/adf_results_roll.csv"))


# ====================================================================
# Correlation Analysis

# --------------------------------------------------------------------
# Setup

# Main Index
main_index = "KLD_C0000000"

# Max lag for tests
max_lag <- 12

# Set target indices for ccf calclation
target_codes <- ifo_tsbl_roll %>%
  names() %>%
  setdiff(c("date", "date_window_end", "window_id", main_index))


# --------------------------------------------------------------------
# Compute Rolling CCF

# Compute rolling ccf tibble
ccf_tbl_roll <- ifo_tsbl_roll %>%
  as_tibble() %>%
  group_by(window_id) %>%
  group_split() %>%
  .[1:5] %>%                      # Option to limit input/run time for testing
  future_map_dfr(function(df) {
    end_date <- df$date_window_end[1]
    map_dfr(
      target_codes,
      function(ind) {
        x <- df[[ind]]
        y <- df[[main_index]]
        if (any(is.na(x)) || any(is.na(y))) return(tibble())
        ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
        tibble(
          lag = as.numeric(ccf_obj$lag),
          corr = as.numeric(ccf_obj$acf),
          indicator = ind,
          date_window_end = end_date,
          window_id = df$window_id[1]
        )
      }
    )
  }, 
  .options = furrr_options(seed = TRUE),
  .progress = TRUE)

# Postprocessing of ccf Results
ccf_tbl_roll <- ccf_tbl_roll %>%
  ccf_postprocess()

# Extract peak lead/lag per Industry
ccf_tbl_roll_peak <- ccf_tbl_roll %>%
  filter(lag %in% c(-6:6)) %>% 
  group_by(ID, window_id) %>%
  slice_max(order_by = abs(corr), n = 1, with_ties = FALSE) %>%
  ungroup()

# Save Output as temp data file
write_csv(ccf_tbl_roll, here("Data/corr_results/ccf_results_roll.csv"))
write_csv(ccf_tbl_roll_peak, here("Data/corr_results/ccf_results_roll_peak.csv"))
# ccf_tbl_roll <- read_csv(here("Data/corr_results/ccf_results_roll.csv"))


# --------------------------------------------------------------------
# Compute Rolling dCor

# Compute rolling ccf tibble
dcor_tbl_roll <- ifo_tsbl_roll %>%
  as_tibble() %>%
  group_by(window_id) %>%
  group_split() %>%
  .[1:5] %>%                      # Option to limit input/run time for testing 
  future_map_dfr(function(df) {
    end_date <- df$date_window_end[1]
    map_dfr(
      target_codes,
      function(ind) {
        x <- df[[ind]]
        y <- df[[main_index]]
        if (any(is.na(x)) || any(is.na(y))) return(tibble())
        get_dcor_pair(x, y, max_lag = max_lag) %>%
          mutate(
            indicator = ind,
            date_window_end = end_date,
            window_id = df$window_id[1]
          )
        }
      )
    }, 
    .options = furrr_options(seed = TRUE),
    .progress = TRUE)

# Postprocessing of ccf Results
dcor_tbl_roll <- dcor_tbl_roll %>%
  dcor_postprocess()

# Extract peak lead/lag per Industry
dcor_tbl_roll_peak <- dcor_tbl_roll %>%
  filter(lag %in% c(-6:6)) %>%
  group_by(ID, window_id) %>%
  slice_max(order_by = abs(corr), n = 1, with_ties = FALSE) %>%
  ungroup()

# Save Output as temp data file
write_csv(dcor_tbl_roll, here("Data/corr_results/dcor_results_roll.csv"))
write_csv(dcor_tbl_roll_peak, here("Data/corr_results/dcor_results_roll_peak.csv"))
# dcor_tbl_roll <- read_csv("LagAnalysis/results/dcor_results_roll.csv")
