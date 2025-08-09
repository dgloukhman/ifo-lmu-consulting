# ====================================================================
# Diagnostic Measure: Cross-Correlation Function (CCF) with Main Index
# Description:
#   This diagnostic computes the cross-correlation function (CCF) between 
#   a target sector's time series and a reference (main) index. It is 
#   particularly useful for identifying potential lead-lag relationships 
#   and synchronization patterns between industry-level indicators and 
#   the broader economy.
#   Applies to wide-format tibbles with one 'date' column and multiple
#   time series columns representing different industries or indices.
# ====================================================================

library(tidyverse)   # For tibble, select, map_dfr, mutate, etc.
library(energy)      # For distance correlation
library(mpmi)        # For mutual information estimator

# --------------------------------------------------------------------
# Function: get_ccf_full
# Purpose: Compute the full cross-correlation function between a target 
#          industry and a main reference index over a specified lag range.
# Arguments:
#   - tsbl: A tibble or data frame with time series columns in wide format,
#           including a 'date' column and multiple numeric series.
#   - main_index: The column name (string) of the main reference index.
#   - target_code: The column name (string) of the target industry.
#   - max_lag: Maximum lag (positive integer) to evaluate in both directions.
# Returns:
#   - A tibble with columns:
#       lag              (lags from -max_lag to +max_lag)
#       correlation      (cross-correlation value at each lag)
#       industry_code    (repeated target_code for tracking)
# Notes:
#   - NA values are removed pairwise before computing the correlation.
#   - The output is suitable for lead-lag visualizations and dynamic analyses.
# --------------------------------------------------------------------
get_ccf_full <- function(tsbl, main_index, target_code, max_lag = 12) {
  # Extract time series for main index and target industry
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Remove any rows with missing values in either series
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Compute the full cross-correlation function (no plot)
  ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
  
  # Format output as tidy tibble
  tibble(
    lag = as.numeric(ccf_obj$lag),
    correlation = as.numeric(ccf_obj$acf),
    industry_code = target_code
  )
}

# --------------------------------------------------------------------
# Function: get_mi_full_tidy
# Purpose: Compute mutual information (MI) at each lag between a target 
#          industry time series and a main reference index.
# Arguments:
#   - tsbl: A tibble or data frame with time series columns in wide format,
#           including a 'date' column and multiple numeric series.
#   - main_index: The column name (string) of the main reference index.
#   - target_code: The column name (string) of the target industry.
#   - max_lag: Maximum lag (positive integer) to evaluate in both directions.
# Returns:
#   - A tibble with columns:
#       lag              (lags from -max_lag to +max_lag)
#       mi               (mutual information at each lag)
#       industry_code    (repeated target_code for tracking)
# Notes:
#   - NA values are removed pairwise before MI calculation.
#   - Output is suitable for lead-lag visualization and nonlinear validation.
#   - For each lag k: x is shifted by -k steps relative to y (like ccf()).
# --------------------------------------------------------------------
get_mi_full <- function(tsbl, main_index, target_code, max_lag = 12) {
  # Extract time series for main index (y) and target industry (x)
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Remove any rows with missing values in either series
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Set sequence of lags (from -max_lag to +max_lag)
  lags <- seq(-max_lag, max_lag)
  
  # For each lag, shift x by -k and compute MI on aligned (non-NA) values
  mi_vals <- map_dbl(lags, function(k) {
    # Shift x: lag(x, n = k) for k > 0; lead(x, n = -k) for k < 0; else no shift
    x_shifted <- if (k > 0) lag(x, n = k) else if (k < 0) lead(x, n = -k) else x
    # Only use complete cases (pairwise complete)
    idx <- complete.cases(x_shifted, y)
    if (sum(idx) < 5) return(NA_real_) # Require minimum overlap
    cmi.pw(x_shifted[idx], y[idx])$bcmi
  })
  
  # Return tidy tibble with lag, MI, and indicator name
  tibble(
    lag = lags,
    mi = mi_vals,
    industry_code = target_code
  )
}

# --------------------------------------------------------------------
# Function: get_mi_full_tidy
# Purpose: Compute mutual information (MI) at each lag between a target 
#          industry time series and a main reference index.
# Arguments:
#   - tsbl: A tibble or data frame with time series columns in wide format,
#           including a 'date' column and multiple numeric series.
#   - main_index: The column name (string) of the main reference index.
#   - target_code: The column name (string) of the target industry.
#   - max_lag: Maximum lag (positive integer) to evaluate in both directions.
# Returns:
#   - A tibble with columns:
#       lag              (lags from -max_lag to +max_lag)
#       mi               (mutual information at each lag)
#       industry_code    (repeated target_code for tracking)
# Notes:
#   - NA values are removed pairwise before MI calculation.
#   - Output is suitable for lead-lag visualization and nonlinear validation.
#   - For each lag k: x is shifted by -k steps relative to y (like ccf()).
# --------------------------------------------------------------------
get_dcor_full <- function(tsbl, main_index, target_code, max_lag = 12) {
  # Extract time series for main index (y) and target industry (x)
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Remove any rows with missing values in either series
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Set sequence of lags (from -max_lag to +max_lag)
  lags <- seq(-max_lag, max_lag)
  
  # For each lag, shift x by -k and compute MI on aligned (non-NA) values
  dcor_vals <- map_dbl(lags, function(k) {
    # Shift x: lag(x, n = k) for k > 0; lead(x, n = -k) for k < 0; else no shift
    x_shifted <- if (k > 0) lag(x, n = k) else if (k < 0) lead(x, n = -k) else x
    # Only use complete cases (pairwise complete)
    idx <- complete.cases(x_shifted, y)
    if (sum(idx) < 5) return(NA_real_) # Require minimum overlap
    dcor(x_shifted[idx], y[idx])
  })
  
  # Return tidy tibble with lag, MI, and indicator name
  tibble(
    lag = lags,
    dcor = dcor_vals,
    industry_code = target_code
  )
}

# --------------------------------------------------------------------
# Function: get_mi_full_tidy
# Purpose: Compute mutual information (MI) at each lag between a target 
#          industry time series and a main reference index.
# Arguments:
#   - tsbl: A tibble or data frame with time series columns in wide format,
#           including a 'date' column and multiple numeric series.
#   - main_index: The column name (string) of the main reference index.
#   - target_code: The column name (string) of the target industry.
#   - max_lag: Maximum lag (positive integer) to evaluate in both directions.
# Returns:
#   - A tibble with columns:
#       lag              (lags from -max_lag to +max_lag)
#       mi               (mutual information at each lag)
#       industry_code    (repeated target_code for tracking)
# Notes:
#   - NA values are removed pairwise before MI calculation.
#   - Output is suitable for lead-lag visualization and nonlinear validation.
#   - For each lag k: x is shifted by -k steps relative to y (like ccf()).
# --------------------------------------------------------------------
get_dcor_pair <- function(x, y, max_lag = 12) {
  # Remove any rows with missing values in either series
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Set sequence of lags (from -max_lag to +max_lag)
  lags <- seq(-max_lag, max_lag)
  
  # For each lag, shift x by -k and compute MI on aligned (non-NA) values
  dcor_vals <- map_dbl(lags, function(k) {
    # Shift x: lag(x, n = k) for k > 0; lead(x, n = -k) for k < 0; else no shift
    x_shifted <- if (k > 0) lag(x, n = k) else if (k < 0) lead(x, n = -k) else x
    # Only use complete cases (pairwise complete)
    idx <- complete.cases(x_shifted, y)
    if (sum(idx) < 5) return(NA_real_) # Require minimum overlap
    dcor(x_shifted[idx], y[idx])
  })
  
  # Return tidy tibble with lag, MI, and indicator name
  tibble(
    lag = lags,
    dcor = dcor_vals,
  )
}
