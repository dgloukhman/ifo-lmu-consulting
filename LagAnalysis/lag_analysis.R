# ====================================================================
# General Setup

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

# --------------------------------------------------------------------

#### 0. Data Preparation ####
source("GeneralUtils/load_data.R")
source("GeneralUtils/structure_analysis.R")

ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  mutate(level = vapply(industry_code, get_level, integer(1)))

# ====================================================================
# Level 2 Analysis

# --------------------------------------------------------------------
# Setup 

# Filter Level 0 and 2 for sector aggregates
ifo_tbl_l2 <- ifo_tbl %>%
  filter(level %in% c(0, 2)) %>%
  select(date, industry_code, KLD)

# Transform to wide format
ifo_tbl_l2_wide <- ifo_tbl_l2 %>%
  pivot_wider(names_from = industry_code, values_from = KLD)

# Convert to tsibble: requires explicit time index
ifo_tsbl_l2 <- as_tsibble(ifo_tbl_l2_wide, index = date)

# Create 1st Diff tsibble

# Set target list
main_index <- "C0000000"
l2_targets <- setdiff(colnames(ifo_tsbl_l2), c("date", main_index))

# Max lag for tests
max_lag <- 12

# --------------------------------------------------------------------
# Test Stationarity and Cointegration

source("LagAnalysis/stationarity_cointegration.R")

# Test stationarity: ADF test for all columns (excluding date)
adf_results <- as_tibble(ifo_tsbl_l2) %>% run_adf_tests()

# Test cointegration: Johansen Cointegration Test (VECM) with Main Index
cointegration_results <- as_tibble(ifo_tsbl_l2) %>% run_cointegration_tests(main_index = main_index)

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

### Cross-Correlation 
## Function to compute full lag correlation for one sector
get_ccf_full <- function(target_code) {
  x <- ifo_tbl_l2_wide[[target_code]]
  y <- ifo_tbl_l2_wide[[main_index]]
  
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

# Build full tibble
ccf_full_tbl <- map_dfr(l2_targets, get_ccf_full)

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

### Granger Causality

# ====================================================================
# Level 3 Analysis



# ====================================================================
#### 1. Analyze Lag Structure #####
# - Anylaze Lag Structure between main index and level 2 compound indicies
# - Analyze General Lag Structure and Turning Point Lag Structure
# - Short-term and long-term lag analysis
#### 2. Model main index with compound indicies ####
# - General model 
# - Model based on Leading Indices only 
# - Model based on Leading + Coinciding Indices (Regression on Residuals)
# - Full Prediction and only Turning Point Prediction



#### 3. Refining Model ####
# - Select most important indices (LASSO/Elastic-Net) 
# - Allow reweighting of compound indices (Grouped Loss)
# - Model Turning Point Prediction and Normal Prediction separatly 
#   (Variance based Band Filter or Markov-Switching based Filter)

# ====================================================================