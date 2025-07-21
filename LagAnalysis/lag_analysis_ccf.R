# ====================================================================
# General

# --------------------------------------------------------------------
# Source Utility Function
source("utils/setup_packages.R")
source("utils/load_data.R")
source("LagAnalysis/lag_utils.R")
source("LagAnalysis/ccf_function.R")
source("LagAnalysis/stationarity_cointegration.R")

# --------------------------------------------------------------------
# Installs necessary packages

install_packages_from_file()

# --------------------------------------------------------------------
# Necessary libaries

library("tidyverse")
library("tsibble")
library("tseries")
library("ggplot2")


# --------------------------------------------------------------------
# Data Preparation

# Read Data
ifo_tsbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  mutate(date = yearmonth(date)) %>%      # Maybe push to read_ifo_data()
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

# # --------------------------------------------------------------------
# # Filter option for performance
# ifo_tsbl_full <- ifo_tsbl_full %>%
#   filter(level %in% c(0,2))
# # --------------------------------------------------------------------

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


# ====================================================================
# Cross-Correlation Analysis

# --------------------------------------------------------------------
# Setup

# Main Index
main_index = "KLD_C0000000"

# Max lag for tests
max_lag <- 12

# --------------------------------------------------------------------
# Compute correlation

# Set target list
stationary_targets <- adf_results_full %>%
  # Step 1: keep only stationary
  filter(adf_is_stationary) %>%
  # Step 2: group per series
  group_by(indicator, industry_code) %>%
  # Step 3: pick least-differenced
  slice_min(difference, with_ties = FALSE) %>%    
  ungroup() %>%
  pull(ID) %>%
  # Step 4: exclude main index
  setdiff(main_index)                             

# Build full tibble
ccf_results_full <- map_dfr(
  stationary_targets,
  ~ get_ccf_full(
    tsbl = ifo_tsbl_full_wide,
    main_index = main_index,
    target_code = .x,
    max_lag = max_lag
  ))

# Postprocessing of ccf Results
ccf_results_full <- ccf_results_full %>%
  ccf_postprocess()

# Extract peak lead/lag per Industry
ccf_results_full_peak <- ccf_results_full %>%
  group_by(ID) %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(peak_lag = lag, peak_corr = correlation)

# ====================================================================
# Visualization

# --------------------------------------------------------------------
# Full Heatmap (L2-3)

# Filter ccf Results
heatmap_data <- ccf_results_full %>%
  filter(level %in% c(2, 3)) %>%
  group_by(lag, indicator) %>%
  summarise(mean_corr = mean(correlation, na.rm = TRUE), .groups = "drop")

# Identify max correlation per indicator
highlight_points <- heatmap_data %>%
  group_by(indicator) %>%
  slice_max(abs(mean_corr), n = 1, with_ties = FALSE) %>%  # absolute value now
  ungroup()

# Plot ccf REsults with yellow borders on peak correlation
ggplot(heatmap_data, aes(x = lag, y = indicator, fill = mean_corr)) +
  geom_tile() +
  geom_tile(data = highlight_points,
            color = "yellow", size = 1.2, fill = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Average Cross-Correlation per Lag and Indicator",
       x = "Lag",
       y = "Indicator",
       fill = "Mean Corr")


# Filter ccf Peak Results
peak_heatmap_data <- ccf_results_full_peak %>%
  filter(level %in% c(2, 3)) %>%
  count(peak_lag, indicator, name = "peak_count")

# Plot ccf Peak Results
ggplot(peak_heatmap_data, aes(x = peak_lag, y = indicator, fill = peak_count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme_minimal() +
  labs(title = "Peak Lead/Lag Count per Indicator",
       x = "Peak Lag",
       y = "Indicator",
       fill = "Count")

