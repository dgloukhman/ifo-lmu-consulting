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
#   - sector      : first 3 digits of the industry code (level 1)
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
      level = sapply(industry_code, get_level),
      sector = str_sub(industry_code, 1, 3)
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
#   - ID, indicator, industry_code, difference, level, sector (as above).
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
      level = sapply(industry_code, get_level),
      sector = str_sub(industry_code, 1, 3)
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
      level = sapply(industry_code, get_level),
      sector = str_sub(industry_code, 1, 3)
    ) %>%
    select(-diff_part)
}

# --------------------------------------------------------------------
# Function: compute_topk_jaccard
# Purpose:
#   For each k in k_vec, compute the mean pairwise Jaccard similarity
#   of the top-k ranked industry sets across all indicators.
# Inputs:
#   - df    : tibble with columns 'indicator', 'industry_code', 'rank'
#   - k_vec : integer vector of k values (e.g., 1:50)
# Output:
#   - tibble with columns:
#       k         : the evaluated cutoff
#       mean_jacc : average pairwise Jaccard across all indicator pairs
# Notes:
#   - Ranks must be 1 = best (ties handled by input order after arrange()).
#   - Top-k sets are derived once per indicator and reused for all k.
#   - Requires: library(tidyverse)
# --------------------------------------------------------------------
compute_topk_jaccard <- function(df, k_vec) {
  # Precompute ordered items per indicator once
  top_base <- df %>%
    arrange(indicator, rank, industry_code) %>%
    group_by(indicator) %>%
    summarise(items = list(industry_code), .groups = "drop")
  
  ind <- top_base$indicator
  nind <- length(ind)
  if (nind < 2) {
    return(tibble(k = k_vec, mean_jacc = NA_real_))
  }
  pairs <- combn(seq_len(nind), 2)
  
  # For each k, compute mean pairwise Jaccard on top-k sets
  tibble(k = k_vec) %>%
    mutate(mean_jacc = map_dbl(k, function(k_) {
      jac <- map_dbl(seq_len(ncol(pairs)), function(col) {
        i <- pairs[1, col]; j <- pairs[2, col]
        a <- head(top_base$items[[i]], k_)
        b <- head(top_base$items[[j]], k_)
        length(intersect(a, b)) / length(union(a, b))
      })
      mean(jac)
    }))
}


# --------------------------------------------------------------------
# Function: compute_topk_jaccard_roll
# Purpose:
#   Similar to compute_topk_jaccard_roll, but comparison over windows.
#   For each k in k_vec, compute the mean pairwise Jaccard similarity
#   of the top-k ranked industry sets across all windows.
# Inputs:
#   - df    : tibble with columns 'window_id', 'industry_code', 'rank'
#   - k_vec : integer vector of k values (e.g., 1:50)
# Output:
#   - tibble with columns:
#       k         : the evaluated cutoff
#       mean_jacc : average pairwise Jaccard across all indicator pairs
# Notes:
#   - Ranks must be 1 = best (ties handled by input order after arrange()).
#   - Top-k sets are derived once per window_id and reused for all k.
#   - Requires: library(tidyverse)
# --------------------------------------------------------------------
compute_topk_jaccard_win <- function(df, k_vec) {
  # Precompute ordered items per indicator once
  top_base <- df %>%
    arrange(window_id, rank, industry_code) %>%
    group_by(window_id) %>%
    summarise(items = list(industry_code), .groups = "drop")
  
  ind <- top_base$window_id
  nind <- length(ind)
  if (nind < 2) {
    return(tibble(k = k_vec, mean_jacc = NA_real_))
  }
  pairs <- combn(seq_len(nind), 2)
  
  # For each k, compute mean pairwise Jaccard on top-k sets
  tibble(k = k_vec) %>%
    mutate(mean_jacc = map_dbl(k, function(k_) {
      jac <- map_dbl(seq_len(ncol(pairs)), function(col) {
        i <- pairs[1, col]; j <- pairs[2, col]
        a <- head(top_base$items[[i]], k_)
        b <- head(top_base$items[[j]], k_)
        length(intersect(a, b)) / length(union(a, b))
      })
      mean(jac)
    }))
}

# --------------------------------------------------------------------
# Function: compute_topk_overlap
# Purpose:
#   For each k in k_vec, compute the size of the union of the top-k
#   industry codes across all indicators.
# Inputs:
#   - df    : tibble with columns 'indicator', 'industry_code', 'rank'
#   - k_vec : integer vector of k values (e.g., 1:50)
# Output:
#   - tibble with columns:
#       k       : cutoff
#       overlap : number of unique industries across all top-k sets
# Notes:
#   - Ranks must be 1 = best.
# --------------------------------------------------------------------
compute_topk_overlap <- function(df, k_vec) {
  tibble(k = k_vec) %>%
    mutate(overlap = map_int(k, function(k_) {
      df %>%
        filter(rank <= k_) %>%
        pull(industry_code) %>%
        unique() %>%
        length()
    }))
}
