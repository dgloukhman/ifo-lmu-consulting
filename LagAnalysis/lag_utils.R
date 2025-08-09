# ====================================================================
# Lag Utilities Script
# Description:
#   A collection of utility functions for time series analysis on the
#   Ifo Business Survey dataset. Includes tools for rolling window
#   expansion, MSM probability extraction, and postprocessing of ADF
#   and CCF results. These functions enable preprocessing, regime
#   identification, and downstream diagnostics like lead-lag analysis.
# ====================================================================

library(slider)       # For efficient rolling window iteration
library(tidyverse)    # For data manipulation

source("utils/load_data.R")    # For get_level function

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
#   - This function performs one slide_index() over the full matrix,
#     so it scales well for many time series.
#   - The result can be pivoted to long format for tidy workflows.
# --------------------------------------------------------------------
tsbl_roll_wide <- function(tsbl_wide, window_size = 24, step = 1) {
  tsbl_wide <- tsbl_wide %>% arrange(date)
  slider::slide_index_dfr(
    .x = tsbl_wide,
    .i = tsbl_wide$date,
    .f = ~ {
      end_date <- max(.x$date)
      .x %>% mutate(
        window_id = paste0("win_", end_date),
        date_window_end = end_date
      )
    },
    .before = months(window_size - 1),
    .complete = TRUE,
    .every = step
  )
}

# --------------------------------------------------------------------
# Function: extract_msm_probs_tbl
# Purpose: Convert MSM model probabilities into a tidy tibble with 
#          aligned date and duration rectangles.
# Arguments:
#   - msm_model: A fitted msmFit object from the MSwM package
#   - tsbl_dates: Vector of dates used to align probabilities
# Returns:
#   - A tibble of smoothed probabilities with regime columns, date,
#     and lead-date pairs (start/end) for rect plotting.
# --------------------------------------------------------------------
extract_msm_probs_tbl <- function(msm_model, tsbl_dates) {
  msm_probs <- msm_model@Fit@smoProb[1:length(tsbl_dates), ]
  colnames(msm_probs) <- paste0("r", seq_len(ncol(msm_probs)))
  msm_probs_tbl <- msm_probs %>%
    as_tibble() %>%
    mutate(date = tail(tsbl_dates, nrow(msm_probs))) %>%
    relocate(date) %>%
    mutate(
      start = date,
      end = lead(date)
    )
  return(msm_probs_tbl)
}

# --------------------------------------------------------------------
# Postprocessing: ADF Test Results
# Description:
#   Cleans and annotates the output of ADF test results.
#   Adds separate fields for indicator name, industry code,
#   number of differences, and hierarchy level.
# --------------------------------------------------------------------
adf_postprocess <- function(adf_results_full) {
  adf_results_full %>%
    rename(ID = industry_code) %>%
    separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
    separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
    mutate(
      difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
      level = sapply(industry_code, get_level)
    ) %>%
    select(-diff_part)
}

# --------------------------------------------------------------------
# Postprocessing: CCF Analysis Results
# Description:
#   Cleans and annotates the output of cross-correlation results.
#   Adds separate fields for indicator name, industry code,
#   number of differences, and hierarchy level.
# --------------------------------------------------------------------
ccf_postprocess <- function(ccf_results_full) {
  ccf_results_full %>%
    rename(ID = industry_code) %>%
    separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
    separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
    mutate(
      difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
      level = sapply(industry_code, get_level)
    ) %>%
    select(-diff_part)
}

# --------------------------------------------------------------------
# Postprocessing: MI Analysis Results
# Description:
#   Cleans and annotates the output of mutual information results.
#   Adds separate fields for indicator name, industry code,
#   number of differences, and hierarchy level.
# --------------------------------------------------------------------
mi_postprocess <- function(mi_results_full) {
  mi_results_full %>%
    rename(ID = industry_code) %>%
    separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
    separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
    mutate(
      difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
      level = sapply(industry_code, get_level)
    ) %>%
    select(-diff_part)
}

# --------------------------------------------------------------------
# Postprocessing: MI Analysis Results
# Description:
#   Cleans and annotates the output of mutual information results.
#   Adds separate fields for indicator name, industry code,
#   number of differences, and hierarchy level.
# --------------------------------------------------------------------
dcor_postprocess <- function(mi_results_full) {
  dcor_results_full %>%
    rename(ID = industry_code) %>%
    separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
    separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
    mutate(
      difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
      level = sapply(industry_code, get_level)
    ) %>%
    select(-diff_part)
}
