# ====================================================================
# General

# --------------------------------------------------------------------
# Source Utility Function
source("utils/setup_packages.R")
source("utils/load_data.R")
source("LagAnalysis/lag_utils.R")
source("LagAnalysis/msm_function.R")
source("LagAnalysis/ccf_function.R")

# --------------------------------------------------------------------
# Installs necessary packages

install_packages_from_file()

# --------------------------------------------------------------------
# Necessary libaries

library("tidyverse")
library("tsibble")
library("tseries")
library("ggplot2")
library("MSwM")

# --------------------------------------------------------------------
# Data Preparation

# Read Data
ifo_tsbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  mutate(date = yearmonth(date)) %>%      # Maybe push to read_ifo_data()
  as_tsibble(key = industry_code, index = date)

# Alternative: load Data from .csv


# --------------------------------------------------------------------
# Setup 

# Pivot Wider
ifo_tsbl_wide <- ifo_tsbl %>%
  select(-level) %>%
  pivot_wider(
    id_cols = date,
    names_from = industry_code,
    values_from = -c(date, industry_code)
  )

# ====================================================================
# Markov Switching Model 

# --------------------------------------------------------------------
# Fit Markov Switching Model
msm_probs_long <- fit_msm_models_all(ifo_tsbl_wide)

# Postprocessing of MSM Results
msm_probs_wide_r1 <- msm_probs_long %>%
  mutate(regime_1_prob = regime_1_prob-0.5) %>%
  pivot_wider(
    id_cols = date,
    names_from = indicator,
    values_from = -c(date, indicator)
  )

# ====================================================================
# Cross-Correlation Analysis

# --------------------------------------------------------------------
# Setup 

# Main Index
main_index = "KLD_C0000000"

# --------------------------------------------------------------------
# CCF Analysis of MSM Probabilities
ccf_results_msm <- run_ccf_msm_parallel(
  msm_wide = msm_probs_wide_r1,
  main_index = main_index,
  max_lag = 12
)

# Postprocessing CCF Results
ccf_results_msm <- ccf_results_msm %>% 
  rename(industry_code = indicator) %>% 
  ccf_postprocess() %>% 
  select(-difference)

# --------------------------------------------------------------------
# Visualization (L2-3)

# Plot MSM Probs
target_indicator = "AVS_C0000000"

msm_probs_long %>%
  filter(indicator == target_indicator) %>%
  ggplot(aes(x = date, y = regime_1_prob)) +
  geom_line(color = "steelblue") +
  labs(
    title = paste("Regime 1 Probability -", target_indicator),
    x = "Date",
    y = "Regime 1 Probability"
  ) +
  theme_minimal()

# Filter ccf Results
heatmap_data <- ccf_results_msm %>%
  filter(level %in% c(2, 3)) %>%
  group_by(lag, indicator) %>%
  summarise(mean_corr = mean(correlation, na.rm = TRUE), .groups = "drop")

# Plot ccf Results with yellow borders on peak correlation
ggplot(heatmap_data, aes(x = lag, y = indicator, fill = mean_corr)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Average Cross-Correlation per Lag and Indicator",
       x = "Lag",
       y = "Indicator",
       fill = "Mean Corr")

