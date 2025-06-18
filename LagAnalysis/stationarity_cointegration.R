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

# --------------------------------------------------------------------
# Function: run_adf_tests
# Purpose: Apply Augmented Dickey-Fuller test to each time series column
# Arguments:
#   - data_tbl: A wide-format tibble/data.frame with a 'date' column
#               and multiple time series columns
#   - significance_level: threshold for determining stationarity (default = 0.05)
# Returns:
#   - A tibble with columns:
#       industry_code        (name of the time series column)
#       adf_statistic        (ADF test statistic)
#       adf_p_value          (p-value of the ADF test)
#       adf_is_stationary    (TRUE if p < significance_level)
# --------------------------------------------------------------------
run_adf_tests <- function(data_tbl, significance_level = 0.05) {
  data_tbl %>%
    select(-date) %>%
    purrr::map_dfr(~{
      ts_clean <- .x[complete.cases(.x)]  # remove NA values
      result <- tryCatch({
        # Perform ADF test and suppress common warning about small p-values
        test <- suppressWarnings(tseries::adf.test(ts_clean))
        tibble(
          adf_statistic = test$statistic,
          adf_p_value = test$p.value,
          adf_is_stationary = test$p.value < significance_level
        )
      }, error = function(e) {
        # Handle errors (e.g., not enough data)
        tibble(adf_statistic = NA_real_, adf_p_value = NA_real_, adf_is_stationary = NA)
      })
    }, .id = "industry_code")  # use column names as IDs
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
#       coint_statistic       (Johansen test statistic)
#       coint_critical_value  (5% critical value)
#       coint_cointegrated    (TRUE if test_stat > critical value)
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