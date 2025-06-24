# ====================================================================
# Diagnostic Measure: Cross-Correlation Function (CCF) with Main Index
# Description:
#   Computes the full cross-correlation function between a target sector's
#   time series and a reference (main) index across a defined lag window.
#   This is useful for identifying lead/lag relationships and synchronization
#   between sectoral indicators and the overall economy.
# Inputs:
#   - tsbl: A tibble or data frame with time series columns (wide format),
#           where each column represents a different industry or index
#   - main_index: The column name (string) of the main reference index
#   - target_code: The column name (string) of the target industry
#   - max_lag: (assumed as global) the maximum lag to evaluate CCF over
# Output:
#   - A tibble with columns:
#       lag              (integer lags from -max_lag to +max_lag)
#       correlation      (cross-correlation value at each lag)
#       industry_code    (the target_code repeated for tracking)
# Dependencies:
#   - Requires `stats::ccf()` and `tibble::tibble()`
# ====================================================================

get_ccf_full <- function(tsbl, main_index, target_code, max_lag) {
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
    lag = ccf_obj$lag,
    correlation = ccf_obj$acf,
    industry_code = target_code
  )
}