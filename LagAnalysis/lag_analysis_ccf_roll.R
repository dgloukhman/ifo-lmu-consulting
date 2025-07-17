# ====================================================================
# General

# --------------------------------------------------------------------
# Source Utility Function
source("utils/setup_packages.R")
source("utils/load_data.R")
source("LagAnalysis/lag_utils.R")


# --------------------------------------------------------------------
# Installs necessary packages

install_packages_from_file()


# --------------------------------------------------------------------
# Necessary libaries

library("tidyverse")
library("tsibble")
library("tseries")
library("ggplot2")
library("furrr")
library("future")
library("MSwM")


# --------------------------------------------------------------------
# Enable parallel processing

plan(multisession)  # Or use multisession, cluster, etc.


# --------------------------------------------------------------------
# Data Preparation

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

# Pivot Wider
ifo_tsbl_full_wide <- ifo_tsbl_full %>%
  select(-level) %>%
  pivot_wider(
    id_cols = date,
    names_from = industry_code,
    values_from = -c(date, industry_code)
  )

# Main Index tsbl
ifo_tsbl_main <- ifo_tsbl %>% 
  filter(industry_code == "C0000000") %>% 
  select(date, industry_code, KLD)


# --------------------------------------------------------------------
# Create Rolling Window tsbl

ifo_tsbl_roll <- ifo_tsbl_full_wide %>%
  tsbl_roll_wide(window_size = 120, step = 1)


# ====================================================================
# Test Stationarity

source("LagAnalysis/stationarity_cointegration.R")

# --------------------------------------------------------------------
# Main Index Stationarity Test
adf_results_main <- ifo_tsbl_main %>% 
  as_tibble() %>% 
  run_adf_tests() 

# --------------------------------------------------------------------
# Rolling Window Stationarity Test

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
  adf_postprocess() %>%
  # Add the date_window_end to ID (enforces unique ID)
  mutate(ID = str_c(ID, date_window_end, sep = "_"))

# Save Output as temp data file
write_csv(adf_results_roll, "LagAnalysis/temp_data/adf_results_roll.csv")


# ====================================================================
# Cross-Correlation Analysis

# --------------------------------------------------------------------
# Setup

# Main Index
main_index = "KLD-diff1_C0000000"

# Max lag for tests
max_lag <- 12

# Set target list
stationary_targets_roll <- adf_results_roll %>%
  # Increase stability of output by selecting first differences
  filter(difference == 1) %>%
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

# Compute rolling ccf tibble
ccf_tbl_roll <- ifo_tsbl_roll %>%
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

# Postprocessing of ccf Results
ccf_tbl_roll <- ccf_tbl_roll %>%
  ccf_postprocess()

# Save Output as temp data file
write_csv(ccf_tbl_roll, "LagAnalysis/temp_data/ccf_results_roll.csv")


# ====================================================================
# Markov Switching Model 

# --------------------------------------------------------------------
# Fit Markov Switching Model

# Fit lm Intercept Model as Baseline
msm_model <- msmFit(
  object = lm(KLD ~ 1, data = ifo_tsbl_main),
  p = 0,                      # number of lags
  k = 2,                      # number of regimes                     
  sw = c(TRUE, TRUE) 
)

plotProb(msm_model)    #Plots the Regimes

 # Get the matrix of smoothed probabilities
msm_probs_tbl <- msm_model %>%
  extract_msm_probs_tbl(ifo_tsbl_main$date)

# Extract most likely regime
msm_regime_tbl <- msm_probs_tbl %>%
  pivot_longer(cols = starts_with("r"), names_to = "regime", values_to = "prob") %>%
  group_by(date) %>%
  slice_max(order_by = prob, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(regime = parse_number(regime))


# ====================================================================
# Visualization

# --------------------------------------------------------------------
# Data Preprocessing


# Regime classification to rolling window tsbl
ccf_tbl_roll <- ccf_tbl_roll %>%
  left_join(msm_regime_tbl %>% 
              select(date, regime), 
            by = c("date_window_end" = "date"))


ccf_tbl_roll %>%
  group_by(regime, indicator, lag) %>%
  summarise(avg_corr = mean(correlation, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = lag, y = indicator, fill = avg_corr)) +
  geom_tile() +
  facet_wrap(~ regime) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(
    title = "Average Cross-Correlation per Regime",
    x = "Lag (months)",
    y = "Indicator",
    fill = "Avg Corr"
  )

ccf_tbl_roll %>%
  ggplot(aes(x = date_window_end, y = lag, fill = correlation)) +
  geom_tile() +
  facet_wrap(~ indicator, scales = "free_y") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(
    title = "Rolling CCF Heatmap per Indicator",
    x = "Window End Date",
    y = "Lag (months)",
    fill = "Correlation"
  )

ccf_tbl_roll %>%
  filter(indicator == "KLD") %>%
  ggplot(aes(x = date_window_end, y = lag, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_y_continuous(breaks = seq(-12, 12, by = 3)) +
  theme_minimal() +
  labs(
    title = "Rolling CCF Heatmap for KLD (with KLD Time Series)",
    x = "Window End Date",
    y = "Lag (months)",
    fill = "Correlation"
  )

