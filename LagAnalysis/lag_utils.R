# ====================================================================
# Utility: Rolling Window Expansion of Wide-Format Time Series
# Description:
#   This utility function expands a wide-format time series tibble 
#   into a long-form tibble where each row is duplicated across 
#   rolling windows defined by a fixed window size and step.
#   Each row in the result is tagged with the ending date of the 
#   rolling window it belongs to, and optionally a window ID.
#   This format is useful for downstream grouped operations such as 
#   computing rolling CCFs, regressions, or diagnostics.
#   Applies to wide-format tibbles with one 'date' column and 
#   multiple time series columns representing industries or indicators.
# ====================================================================

library(slider)       # For efficient rolling window iteration
library(tidyverse)    # For data manipulation

# --------------------------------------------------------------------
# Function: tsbl_roll_wide
# Purpose: Generate a row-duplicated rolling window tibble from a 
#          wide-format time series dataset.
# Arguments:
#   - tsbl_wide: A wide-format tibble with a 'date' column and 
#                multiple numeric columns for time series data.
#   - window_size: Integer, number of months in each rolling window.
#   - step: Integer, step size (in number of rows) to move the window forward.
# Returns:
#   - A tibble in wide format where each row is tagged with:
#       - window_id: Optional unique identifier for each window
#       - date_window_end: The last date of the corresponding window
# Notes:
#   - This function performs one `slide_index()` over the full matrix,
#     so it scales well for many time series.
#   - The result can be pivoted to long format for tidy workflows.
# --------------------------------------------------------------------
tsbl_roll_wide <- function(tsbl_wide, window_size = 24, step = 1) {
  
  # Ensure rows are in chronological order
  tsbl_wide <- tsbl_wide %>% arrange(date)
  
  # Perform rolling operation across all columns using date as index
  slider::slide_index_dfr(
    .x = tsbl_wide,
    .i = tsbl_wide$date,
    .f = ~ {
      end_date <- max(.x$date)  # Identify the end of the current window
      
      .x %>%
        mutate(
          window_id = paste0("win_", end_date),  # Optional unique window label
          date_window_end = end_date             # Track window scope
        )
    },
    .before = months(window_size - 1),  # Define window size in months
    .complete = TRUE,                   # Require full window (no partials)
    .every = step                       # Advance by this many rows per window
  )
}