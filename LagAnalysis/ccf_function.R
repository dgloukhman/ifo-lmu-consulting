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
library(slider)      # For rolling window functionality

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
# Function: run_rolling_ccf
# Purpose: Compute the cross-correlation function (CCF) over rolling windows
# Arguments:
#   - tsbl: A tibble or tsibble with a 'date' column and time series columns
#   - main_index: Name (string) of the reference index column
#   - target_code: Name (string) of the target industry column
#   - window_size: Number of observations in each rolling window (default = 24)
#   - step: Step size to move the window forward (default = 1)
#   - max_lag: Maximum lag to compute CCF over
# Returns:
#   - A tibble with columns:
#       date_window_end    (last date of each rolling window)
#       lag                (lags from -max_lag to +max_lag)
#       correlation        (cross-correlation at each lag)
#       industry_code      (the target_code repeated)
# --------------------------------------------------------------------
run_rolling_ccf <- function(tsbl, main_index, target_code,
                            window_size = 24, step = 1, max_lag = 12,
                            target_IDs) {
  
  tsbl <- tsbl %>% arrange(date)
  
  slider::slide_index_dfr(
    .x = tsbl,
    .i = tsbl$date,
    .f = ~{
      # Get the window's end date
      end_date <- max(.x$date)
      
      # Build the current window's target ID (same logic as in your stationary_targets_roll)
      current_ID <- str_c(target_code, end_date, sep = "_")
      
      # Only compute if it's in the target list
      if (current_ID %in% target_IDs) {
        ccf_tbl <- get_ccf_full(.x, main_index, target_code, max_lag)
        ccf_tbl %>% mutate(date_window_end = end_date)
      } else {
        tibble()  # return empty tibble if not in target
      }
    },
    .before = months(window_size - 1),
    .complete = TRUE,
    .every = step
  )
}

# --------------------------------------------------------------------
# Function: get_ccf_msm_dual
# Purpose: Compute cross-correlation between MSM regime probs of a
#          target series and a main reference series, handling regime
#          label switching by comparing both regimes.
# Arguments:
#   - msm_wide: Wide-format tibble with regime 1 probabilities
#   - main_index: Name of the reference column (character)
#   - target_code: Name of the target column (character)
#   - max_lag: Maximum lag to consider in both directions (default: 12)
# Returns:
#   - A tibble with: lag, correlation, regime_used, indicator
# --------------------------------------------------------------------
get_ccf_msm_dual <- function(msm_wide, main_index, target_code, max_lag = 12) {
  # Extract regime probabilities
  x_main <- msm_wide[[main_index]]
  x_r1 <- msm_wide[[target_code]]
  x_r2 <- 1 - x_r1  # Counter regime
  
  # Drop NAs
  valid_idx <- complete.cases(x_main, x_r1)
  x_main <- x_main[valid_idx]
  x_r1 <- x_r1[valid_idx]
  x_r2 <- x_r2[valid_idx]
  
  # Return empty if no overlap
  if (length(x_main) == 0 || length(x_r1) == 0) return(tibble())
  
  # Compute both CCFs
  ccf_r1 <- ccf(x_r1, x_main, lag.max = max_lag, plot = FALSE)
  ccf_r2 <- ccf(x_r2, x_main, lag.max = max_lag, plot = FALSE)
  
  # Select best by maximum absolute correlation
  max_r1 <- mean(abs(ccf_r1$acf), na.rm = TRUE)
  max_r2 <- mean(abs(ccf_r2$acf), na.rm = TRUE)
  
  if (max_r1 >= max_r2) {
    tibble(
      lag = as.numeric(ccf_r1$lag),
      correlation = as.numeric(ccf_r1$acf),
      regime_used = "r1",
      indicator = target_code
    )
  } else {
    tibble(
      lag = as.numeric(ccf_r2$lag),
      correlation = as.numeric(ccf_r2$acf),
      regime_used = "r2",
      indicator = target_code
    )
  }
}
