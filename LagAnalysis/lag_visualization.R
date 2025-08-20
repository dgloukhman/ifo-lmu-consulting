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
library("ggplot2")

# --------------------------------------------------------------------
# Read Data

# Full Analysis Data
ccf_tbl_full <- read_csv("LagAnalysis/results/ccf_results_full.csv")
ccf_tbl_full_peak <- read_csv("LagAnalysis/results/ccf_results_full_peak.csv")
dcor_tbl_full <- read_csv("LagAnalysis/results/dcor_results_full.csv")
dcor_tbl_full_peak <- read_csv("LagAnalysis/results/dcor_results_full_peak.csv")
adf_tbl_full <- read_csv("LagAnalysis/results/adf_results_full.csv")

# Rolling Window Analysis Data
ccf_tbl_roll <- read_csv("LagAnalysis/results/ccf_results_roll.csv")
ccf_tbl_roll_peak <- read_csv("LagAnalysis/results/ccf_results_roll_peak.csv")
dcor_tbl_roll <- read_csv("LagAnalysis/results/dcor_results_roll.csv")
dcor_tbl_roll_peak <- read_csv("LagAnalysis/results/dcor_results_roll_peak.csv")
adf_tbl_roll <- read_csv("LagAnalysis/results/adf_results_roll.csv")


# ====================================================================
# Full Time Series Visualization

# --------------------------------------------------------------------
# Setup


# --------------------------------------------------------------------
# L1 - Corr Overview per Industry

# CCF Overview

# Filter ccf Results
heatmap_data <- ccf_tbl_full %>%
  filter(level %in% c(1), 
         difference == 0,
         lag %in% c(-6:6), )%>%
         # industry_code %in% top_scorer) %>%
  group_by(lag, indicator) %>%
  summarise(mean_corr = median(correlation, na.rm = TRUE), .groups = "drop")

# Identify max correlation per indicator
highlight_points <- heatmap_data %>%
  group_by(indicator) %>%
  slice_max(abs(mean_corr), n = 1, with_ties = FALSE) %>%  # absolute value now
  ungroup()

# Plot ccf Results with yellow borders on peak correlation
ggplot(heatmap_data, aes(x = lag, y = indicator, fill = mean_corr)) +
  geom_tile() +
  geom_tile(data = highlight_points,
            color = "yellow", size = 1.2, fill = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Median Cross-Correlation per Lag and Indicator",
       x = "Lag",
       y = "Indicator",
       fill = "Median Corr")


# Dcor Overview

heatmap_data <- dcor_tbl_full %>%
  filter(level %in% c(1:3), difference == 0) %>%
  group_by(lag, indicator) %>%
  summarise(mean_corr = mean(dcor, na.rm = TRUE), .groups = "drop")

# Identify max correlation per indicator
highlight_points <- heatmap_data %>%
  group_by(indicator) %>%
  slice_max(abs(mean_corr), n = 1, with_ties = FALSE) %>%  # absolute value now
  ungroup()

# Plot ccf Results with yellow borders on peak correlation
ggplot(heatmap_data, aes(x = lag, y = indicator, fill = mean_corr)) +
  geom_tile() +
  geom_tile(data = highlight_points,
            color = "yellow", size = 1.2, fill = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Average Cross-Correlation per Lag and Indicator",
       x = "Lag",
       y = "Indicator",
       fill = "Mean Corr")

# --------------------------------------------------------------------
# Corr Overview per Question

# L(1-L3)


# --------------------------------------------------------------------
# Corr Variance Overview (Boxplot)

# Total Variance Boxplot per Question (L1-L3)
ccf_tbl_full %>% 
  filter(level %in% c(1:3), 
         difference == 0,
         lag %in% c(-6:6)) %>%
  ggplot(aes(x = factor(lag), y = correlation)) +
  facet_wrap(~ indicator) +
  geom_boxplot(outlier.size = 1, outlier.colour = "red") +
  theme_minimal() +
  labs(
    title = "Correlation distribution by Question",
    x = "Indicator (Questions)",
    y = "Correlation"
  )

# Peak Corr Variance Boxplot per Question (L1-L3)
ccf_tbl_full_peak %>% 
  filter(level %in% c(1:3),
         difference == 0,
         peak_lag %in% c(-6:6), #)%>%
         industry_code %in% top_scorer) %>%
  ggplot(aes(x = indicator, y = peak_corr)) +
  facet_wrap(~ level) +
  geom_boxplot(outlier.size = 1, outlier.colour = "red") +
  theme_minimal() +
  labs(
    title = "Peak Correlation distribution by Question",
    x = "Indicator (Questions)",
    y = "Correlation"
  )

# 
top_scorer <- ccf_tbl_full_peak %>%
  filter(level %in% c(1:3),
         difference == 0,
         peak_lag %in% c(-6:6)) %>%
  group_by(indicator) %>%
  slice_max(abs(peak_corr), n=30) %>% 
  pull(industry_code) %>%
  unique()

top <- ccf_tbl_full_peak %>%
  filter(level %in% c(1:3),
         difference == 0,
         peak_lag %in% c(-6:6)) %>%
  group_by(indicator) %>%
  slice_max(abs(peak_corr), n=30) %>%
  ungroup() %>%
  select(industry_code, level) %>%
  unique() %>% 
  count(level)

top_ind <- ccf_tbl_full_peak %>%
  filter(level %in% c(1:3),
         difference == 0,
         peak_lag %in% c(-6:6)) %>%
  group_by(indicator) %>%
  slice_max(abs(peak_corr), n=10) %>%
  ungroup() %>%
  select(industry_code, level) %>%
  unique() %>% 
  mutate(group = str_sub(industry_code, 1,3)) %>%
  count(group) %>%
  rename(top = n)

ind <- ccf_tbl_full_peak %>%
  filter(level %in% c(1:3),
         difference == 0,
         peak_lag %in% c(-6:6)) %>%
  select(industry_code, level) %>%
  unique() %>% 
  mutate(group = str_sub(industry_code, 1,3)) %>%
  count(group) %>%
  left_join(top_ind)


