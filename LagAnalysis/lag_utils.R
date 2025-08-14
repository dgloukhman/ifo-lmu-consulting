# ====================================================================
# Lag Utilities (IBS)
# Description:
#   Utilities for sliding-window expansion and postprocessing of model/
#   diagnostic outputs (MSwM regime probs, ADF, CCF/MI/DCorr). Designed
#   for *wide* time-series tibbles: one 'date' column + multiple numeric
#   series (e.g., industry codes). Aims: reproducible preprocessing,
#   clean metadata (indicator/industry/level/diffs), and tidy outputs
#   ready for plots (heatmaps, rectangles) and rolling analyses.
# ====================================================================

library(slider)       # slide_index_* for index-aware rolling windows
library(tidyverse)    # dplyr/tidyr/purrr/tibble
# If you use months() in tsbl_roll_wide(), ensure lubridate is available:
# library(lubridate)

source("utils/load_data.R")    # exposes get_level(industry_code)

# --------------------------------------------------------------------
# Function: tsbl_roll_wide
# Purpose:
#   Create a row-duplicated rolling-window view over a *wide* time-series
#   tibble, tagging each row with the window's end date and a window id.
#   One call to slide_index_dfr() spans all columns → efficient at scale.
# Arguments:
#   - tsbl_wide   : wide tibble with a Date/YearMonth 'date' column and
#                   multiple numeric series (industries/indices).
#   - window_size : integer; window length in *months* when using
#                   lubridate::months(window_size - 1) with a monthly index.
#                   For other index frequencies, adjust accordingly.
#   - step        : integer; stride in *rows* between window starts
#                   (slide_index_dfr .every counts elements, not time).
# Returns:
#   tibble in wide format where each original row within each window
#   is annotated with:
#     - window_id       : "win_<YYYY-MM-..>" derived from date_window_end
#     - date_window_end : last index value (max(.x$date)) in the window
# Notes:
#   - Assumes tsbl_wide$date is ordered ascending; this function enforces
#     arrange(date) defensively.
#   - For monthly data, .before = months(window_size - 1) uses
#     lubridate::months(); add `library(lubridate)` or prefix call.
#   - Output can be pivoted longer for tidy workflows, or grouped by
#     (window_id/date_window_end) for per-window computations.
#   - .complete = TRUE drops partial leading windows.
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
# Purpose:
#   Tidy smoothed regime probabilities from an MSwM fit and align them
#   to an external date index; also compute [start, end) rectangles for
#   plotting regime spans.
# Arguments:
#   - msm_model : fitted MSwM model (e.g., class "MSwMfit"); must expose
#                 slot @Fit@smoProb with T×R smoothed probabilities.
#   - tsbl_dates: vector of dates used to align probabilities (length ≥ T).
# Returns:
#   tibble with columns:
#     - date  : aligned timestamp per row
#     - r1..rR: smoothed regime probabilities per regime
#     - start : = date (rectangle left)
#     - end   : lead(date) (rectangle right; last row typically NA)
# Notes:
#   - Only the first length(tsbl_dates) rows of smoProb are used; if
#     msm_model has fewer rows than tsbl_dates, results are truncated.
#   - For rectangle plots, you often drop the last row where end is NA.
#   - Ensure the timing of msm_model input matches tsbl_dates (same
#     sampling and alignment) to avoid off-by-one artifacts.
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
# Purpose:
#   Normalize ADF results’ metadata by splitting composite IDs of the form
#   "<indicator[-diffK]>_<industry_code>" into separate fields and adding
#   hierarchy level via get_level().
# Input expectations:
#   - Columns include industry_code (composite id before split).
# Output columns added/standardized:
#   - ID          : original composite id (preserved)
#   - indicator   : base indicator name (e.g., "KLD")
#   - industry_code: industry code part (e.g., "C1105000")
#   - difference  : integer K extracted from "-diffK" suffix (0 if absent)
#   - level       : hierarchy level from get_level(industry_code)
# Notes:
#   - Does not modify numeric test stats/decisions; only augments metadata.
#   - Assumes "-diff" suffix formatting (e.g., "KLD-diff1"); nonconforming
#     names will yield NA for difference.
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
# Purpose:
#   Add clean metadata to CCF outputs whose 'indicator' field contains a
#   composite id "<indicator[-diffK]>_<industry_code>".
# Input expectations:
#   - Columns include indicator (composite id before split).
# Output columns added/standardized:
#   - ID, indicator, industry_code, difference, level (as above).
# Notes:
#   - Keeps original CCF metrics (lag, correlation) unchanged.
# --------------------------------------------------------------------
ccf_postprocess <- function(ccf_results_full) {
  ccf_results_full %>%
    rename(ID = indicator) %>%
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
# Purpose:
#   Same metadata normalization for mutual information outputs where
#   'indicator' holds "<indicator[-diffK]>_<industry_code>".
# Input expectations / Output columns:
#   - Same as ccf_postprocess(); preserves MI metrics (lag, mi).
# --------------------------------------------------------------------
mi_postprocess <- function(mi_results_full) {
  mi_results_full %>%
    rename(ID = indicator) %>%
    separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
    separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
    mutate(
      difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
      level = sapply(industry_code, get_level)
    ) %>%
    select(-diff_part)
}

# --------------------------------------------------------------------
# Postprocessing: DCorr Analysis Results
# Purpose:
#   Same metadata normalization for distance-correlation outputs where
#   'indicator' holds "<indicator[-diffK]>_<industry_code>".
# Input expectations / Output columns:
#   - Same as ccf_postprocess(); preserves DCorr metrics (lag, dcor).
# --------------------------------------------------------------------
dcor_postprocess <- function(dcor_results_full) {
  dcor_results_full %>%
    rename(ID = indicator) %>%
    separate(ID, into = c("indicator", "industry_code"), sep = "_", remove = FALSE) %>%
    separate(indicator, into = c("indicator", "diff_part"), sep = "-diff", fill = "right") %>%
    mutate(
      difference = if_else(is.na(diff_part), 0L, as.integer(diff_part)),
      level = sapply(industry_code, get_level)
    ) %>%
    select(-diff_part)
}