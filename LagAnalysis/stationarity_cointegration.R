# ====================================================================
# Diagnostic Tests: ADF for Stationarity and Johansen for Cointegration
# Description:
#   These functions are designed to test time series data for stationarity
#   and cointegration using the ADF and Johansen tests respectively.
#   Input: Wide-format data frame (columns = time series, "date" column)
# ====================================================================

library(tidyverse)   # For tibble, select, map_dfr, mutate, etc.
library(tseries)     # For adf.test()
library(urca)        # For ca.jo() Johansen cointegration test
library(slider)      # For rolling window functionality
library(lubridate)   # For sliding over months intead of days

# --------------------------------------------------------------------
# Function: run_adf_tests
# Purpose: Apply Augmented Dickey-Fuller test to each time series column
# Arguments:
#   - data_tbl: A wide-format tibble/data.frame with a 'date' column
#               and multiple time series columns
#   - significance_level: Threshold for determining stationarity (default = 0.05)
# Returns:
#   - A tibble with columns:
#       industry_code        (name of the time series column)
#       adf_statistic        (ADF test statistic)
#       adf_p_value          (p-value of the ADF test)
#       adf_is_stationary    (TRUE if p < significance_level)
# Notes:
#   - If the ADF test fails (e.g., due to short or constant input),
#     NA values are returned and an informative message is printed.
# --------------------------------------------------------------------
run_adf_tests <- function(data_tbl, significance_level = 0.05) {
  data_tbl %>%
    as_tibble() %>%
    select(where(is.numeric)) %>%  # Remove 'date' column etc., keep only numeric
    purrr::imap_dfr(~{
      ts_clean <- .x[complete.cases(.x)]  # Drop any NA values in the series
      
      tryCatch({
        # Run ADF test with warnings suppressed (e.g., for p-value near 0)
        test <- suppressWarnings(tseries::adf.test(ts_clean))
        # Return test results as tibble
        tibble(
          industry_code = .y,                   # Column name (e.g., sector ID)
          adf_statistic = test$statistic,       # Test statistic
          adf_p_value = test$p.value,           # p-value
          adf_is_stationary = test$p.value < significance_level  # Logical result
        )
      }, error = function(e) {
        # If the test fails (e.g., due to constant input or too few points), return NAs
        message(sprintf("ADF error in series '%s': %s", .y, e$message))
        tibble(
          industry_code = .y,
          adf_statistic = NA_real_,
          adf_p_value = NA_real_,
          adf_is_stationary = NA
        )
      })
    })
}

# --------------------------------------------------------------------
# Function: run_rolling_adf
# Purpose: Apply the ADF test over rolling windows of fixed length
# Arguments:
#   - data_tsbl: A wide-format tsibble or tibble with a 'date' column
#   - window_size: Number of observations per rolling window (default = 24)
#   - step: Number of observations to move forward per iteration (default = 1)
#   - significance_level: Threshold for determining stationarity (default = 0.05)
# Returns:
#   - A tibble with one row per industry and per window,
#     including:
#       industry_code        (name of the time series column)
#       adf_statistic        (ADF test statistic)
#       adf_p_value          (p-value of the ADF test)
#       adf_is_stationary    (TRUE if p < significance_level)
#       date_window_end      (last date in the current rolling window)
# Notes:
#   - Only complete windows are used (controlled by .complete = TRUE)
#   - Results are aligned to the right edge of the window (i.e., date_window_end)
# --------------------------------------------------------------------
run_rolling_adf <- function(data_tsbl, window_size = 24, step = 1, significance_level = 0.05) {
  # Ensure data is ordered by date to ensure correct rolling behavior
  data_tsbl <- data_tsbl %>% arrange(date)
  
  # Apply sliding window ADF tests using slider::slide_index_dfr
  slide_index_dfr(
    .x = data_tsbl,              # The data
    .i = data_tsbl$date,         # The index column (rolling is done by date)
    .f = ~{
      # Apply ADF tests within each rolling window, return results + window end date
      run_adf_tests(.x, significance_level) %>%
        mutate(date_window_end = max(.x$date))
    },
    .before = months(window_size - 1),   # Look-back size (window length)
    .complete = TRUE,            # Drop incomplete windows (e.g., at the beginning)
    .every = step                # Step size for sliding window
  )
}

# --------------------------------------------------------------------
# Function: run_cointegration_tests
# Purpose: Perform Johansen cointegration test for each sector vs main index
# Arguments:
#   - data_tbl: A wide-format tibble/data.frame with a 'date' column
#   - main_index: Name (string) of the column to use as the main reference series
#   - lag: Number of lags to include in the Johansen test (default = 2)
# Returns:
#   - A tibble with columns:
#       industry_code         (name of the sector index column)
#       coint_statistic       (Johansen test statistic for r = 0)
#       coint_critical_value  (5% critical value for the test)
#       coint_cointegrated    (TRUE if test_stat > critical value)
# Notes:
#   - The test is performed for each sector index relative to the main index
#   - Constant deterministic trend ("const") is included in the test specification
# --------------------------------------------------------------------
run_cointegration_tests <- function(data_tbl, main_index, lag = 2) {
  other_codes <- setdiff(names(data_tbl), c("date", main_index))  # exclude main and date
  
  purrr::map_dfr(other_codes, function(code) {
    df <- data_tbl %>%
      select(main_index = !!main_index, sector = !!code) %>%
      drop_na()  # ensure no missing values for cointegration test
    
    tryCatch({
      # Johansen trace test with constant deterministic term
      johansen <- urca::ca.jo(df, type = "trace", ecdet = "const", K = lag)
      test_stat <- johansen@teststat[1]  # test for r = 0 (no cointegration)
      crit_val <- johansen@cval[1, "5pct"]
      
      tibble(
        industry_code = code,
        coint_statistic = test_stat,
        coint_critical_value = crit_val,
        coint_cointegrated = test_stat > crit_val
      )
    }, error = function(e) {
      # Handle errors (e.g., rank deficiency or data issues)
      tibble(
        industry_code = code,
        coint_statistic = NA_real_,
        coint_critical_value = NA_real_,
        coint_cointegrated = NA
      )
    })
  })
}