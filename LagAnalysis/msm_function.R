library(tidyverse)   # For tibble, dplyr, purrr-style mapping
library(furrr)       # For parallelized future_map functions
library(MSwM)        # For Markov-switching regression models

# ====================================================================
# Structural Regime Detection: MSM Fit Across Multiple Indicators
# Description:
#   This function fits two-regime Markov Switching Models (MSM) to 
#   each time series column in a wide-format tibble (excluding 'date'),
#   using a constant-intercept baseline model (no autoregressive lags).
#   It returns the smoothed probability of being in Regime 1 for each
#   indicator and time point, enabling identification of regime shifts
#   across the economic panel.
# ====================================================================

# --------------------------------------------------------------------
# Function: fit_msm_models_all
# Purpose: Fit two-regime MSMs for each time series in a wide-format
#          tibble (excluding the 'date' column), extract Regime 1
#          smoothed probabilities, and return a tidy long-format tibble.
# Arguments:
#   - df_tsbl: A tibble or tsibble with a 'date' column and multiple
#              time series columns (e.g. survey indicators)
# Returns:
#   - A tidy tibble with columns:
#       date             (date from the input tibble)
#       indicator        (name of the variable being modeled)
#       regime_1_prob    (smoothed probability of Regime 1 at each time)
# Notes:
#   - Uses msmFit() with 2 regimes, no lags, switching mean and variance.
#   - Assumes rows are in time order and shared across all indicators.
#   - Useful for visualizing regime shifts and structural change.
# --------------------------------------------------------------------
fit_msm_models_all <- function(df_tsbl) {
  # Identify time series columns (exclude 'date')
  target_vars <- setdiff(names(df_tsbl), "date")
  
  # Set up parallel processing strategy
  plan(multisession)
  
  # Fit MSM models to all target series in parallel
  msm_probs_all <- future_map_dfr(
    .x = target_vars,
    .f = ~ {
      # Subset current variable and rename to 'value' for modeling
      df <- df_tsbl %>% select(date, value = all_of(.x))
      
      # Skip if all values are missing
      if (all(is.na(df$value))) return(tibble())
      
      # Fit baseline linear model (intercept-only)
      base_lm <- lm(value ~ 1, data = df)
      
      # Fit MSM with 2 regimes, no lags, switching mean and variance
      msm_model <- msmFit(
        object = base_lm,
        p = 0,
        k = 2,
        sw = c(TRUE, FALSE)
      )
      
      # Extract smoothed probabilities for Regime 1 only
      r1_probs <- msm_model@Fit@smoProb[, 1]
      
      # Return tibble with row index for later date alignment
      tibble(
        row_id = seq_along(r1_probs),
        regime_1_prob = r1_probs,
        indicator = .x
      )
    },
    .progress = TRUE
  )
  
  # Add date by joining with original row indices
  date_tbl <- df_tsbl %>%
    mutate(row_id = row_number()) %>%
    select(row_id, date)
  
  # Merge regime probabilities with dates and return final output
  msm_probs_all %>%
    left_join(date_tbl, by = "row_id") %>%
    select(date, indicator, regime_1_prob)
}


# --------------------------------------------------------------------
# Function: run_ccf_msm_parallel
# Purpose: Run get_ccf_msm_dual() across multiple indicators in parallel
# Arguments:
#   - msm_wide: Wide-format tibble with 'date' and regime 1 probabilities
#   - main_index: Name of column to serve as reference series
#   - max_lag: Max lag to evaluate CCF in both directions
# Returns:
#   - A tidy tibble with: lag, correlation, regime_used, indicator
# --------------------------------------------------------------------
run_ccf_msm_parallel <- function(msm_wide, main_index, max_lag = 12) {
  library(furrr)
  plan(multisession)  # or plan(multicore) on macOS/Linux
  
  # Define which columns to apply to
  target_vars <- setdiff(names(msm_wide), c("date", main_index))
  
  # Parallel map over all target variables
  future_map_dfr(
    .x = target_vars,
    .f = ~ get_ccf_msm_dual(
      msm_wide = msm_wide,
      main_index = main_index,
      target_code = .x,
      max_lag = max_lag
    ),
    .progress = TRUE
  )
}