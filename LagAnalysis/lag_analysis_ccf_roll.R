# ====================================================================
# General

# --------------------------------------------------------------------
# Installs necessary packages

source("utils/setup_packages.R")
install_packages_from_file()

# --------------------------------------------------------------------
# Necessary libaries

library("tidyverse")
library("tsibble")
library("tseries")
library("ggplot2")
library("feasts")
library("furrr")

# --------------------------------------------------------------------
# Data Preparation

source("utils/load_data.R")


# Read Data
ifo_tsbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  as_tsibble(key = industry_code, index = date)

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

# --------------------------------------------------------------------
# Filter option for performance
ifo_tsbl_full <- ifo_tsbl_full %>%
  filter(level %in% c(0,2))
# --------------------------------------------------------------------

# Pivot Wider
ifo_tsbl_full_wide <- ifo_tsbl_full %>%
  select(-level) %>%
  pivot_wider(
    id_cols = date,
    names_from = industry_code,
    values_from = -c(date, industry_code)
  )

# --------------------------------------------------------------------
# Create Rolling Window Tsibble

source("LagAnalysis/lag_utils.R")

ifo_tsbl_roll <- ifo_tsbl_full_wide %>%
  tsbl_roll_wide(window_size = 24, step = 1)

# ====================================================================
# Test Stationarity

source("LagAnalysis/stationarity_cointegration.R")

# --------------------------------------------------------------------
# Rolling Window Stationarity Test
plan(multisession)  # Or use multisession, cluster, etc.

# Test for unit root: rolling ADF test for all columns (excluding date)
adf_results_roll <- ifo_tsbl_roll %>%
  as_tibble() %>%
  group_by(window_id) %>%
  group_split() %>%
  future_map_dfr(~ {
    df <- .x
    run_adf_tests(df) %>%
      mutate(date_window_end = df$date_window_end[1],
             window_id = df$window_id[1])
  })

# Postprocessing of adf Results
adf_results_roll <- adf_results_roll %>%
  # Step 1: Rename original industry_code to preserve it
  rename(ID = industry_code) %>%
  # Step 2: Separate the original ID into indicator and industry_code
  separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
  # Step 3: Split indicator into base + diff components
  separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
  # Step 4: Compute difference column and level
  mutate(
    difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
    level = sapply(industry_code, get_level)
  ) %>%
  # Remove diff_part column (no longer needed)
  select(-diff_part) %>%
  # Add the date_window_end to ID (enforces unique ID)
  mutate(ID = str_c(ID, date_window_end, sep = "_"))


# ====================================================================
# Cross-Correlation Analysis

# --------------------------------------------------------------------
# Setup

# Main Index
main_index = "KLD_C0000000"

# Max lag for tests
max_lag <- 12

# Set target list
stationary_targets_roll <- adf_results_roll %>%
  # Step 1: keep only stationary
  filter(adf_is_stationary) %>%
  # Step 2: group per series
  group_by(indicator, industry_code, date_window_end) %>%
  # Step 3: pick least-differenced
  slice_min(difference, with_ties = FALSE) %>%    
  ungroup() %>%
  # Step 4: exclude main index
  filter(!str_starts(ID, "main_index")) %>% 
  pull(ID)

# --------------------------------------------------------------------
# Compute Rolling CCF
source("LagAnalysis/ccf_function.R")

plan(multisession)  # # Or use multisession, cluster, etc.

# Compute rolling ccf tibble
ccf_results_roll <- ifo_tsbl_roll %>%
  as_tibble() %>%
  group_by(window_id) %>%
  group_split() %>%
  future_map_dfr(~ {
    df <- .x
    end_date <- df$date_window_end[1]
    
    # Get all target codes in this window (excluding main index + metadata)
    target_codes <- setdiff(
      names(df),
      c("date", "date_window_end", "window_id", main_index)
    )
    
    # Compute CCFs for all eligible target codes
    map_dfr(target_codes, function(target_code) {
      current_ID <- str_c(target_code, end_date, sep = "_")
      
      if (current_ID %in% stationary_targets_roll) {
        get_ccf_full(df, main_index, target_code, max_lag = 12) %>%
          mutate(
            date_window_end = end_date,
            window_id = df$window_id[1]
          )
      } else {
        tibble()  # Skip if not in stationary list
      }
    })
  })


# ====================================================================
# Visualization

