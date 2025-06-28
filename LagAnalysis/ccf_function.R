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
  # Convert to Tibble
  tbl <- as_tibble(tsbl) 
  
  # Extract time series for main index and target industry
  x <- tbl[[target_code]]
  y <- tbl[[main_index]]
  
  # Remove any rows with missing values in either series
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Compute the full cross-correlation function (no plot)
  ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
  
  # Format output as tidy tibble
  tibble(
    lag = ccf_obj$lag,
    correlation = ccf_obj$acf,
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
                            window_size = 24, step = 1, max_lag = 12) {
  tbl <- as_tibble(tsbl)
  tbl <- tbl %>% arrange(date)
  
  slider::slide_index_dfr(
    .x = tbl,
    .i = tbl$date,
    .f = ~{
      ccf_tbl <- get_ccf_full(.x, main_index, target_code, max_lag)
      ccf_tbl %>% mutate(date_window_end = max(.x$date))
    },
    .before = window_size - 1,
    .complete = TRUE,
    .every = step
  )
}