# ====================================================================
# Diagnostics for Lead–Lag Structure vs. Main Index
# Description:
#   Helpers to quantify linear and nonlinear dependence between a target
#   sector series and a reference (main) index across a symmetric lag window.
#   Inputs are *wide* tibbles/data.frames with one 'date' column and
#   multiple numeric series columns (industries/indices).
#   Conventions follow base::ccf(): for ccf(x, y), a positive lag k
#   corresponds to corr(x_{t+k}, y_t), i.e., x **leads** y by k periods.
#   In the MI/DCorr helpers we reproduce this by shifting x backward by k.
# ====================================================================

library(tidyverse)   # tibble, dplyr::lag/lead, purrr::map_dbl, etc.
library(energy)      # distance correlation: dcor()
library(mpmi)        # mutual information estimator: cmi.pw()

# --------------------------------------------------------------------
# Function: get_ccf_full
# Purpose:
#   Compute the full cross-correlation function (linear dependence) between
#   a target industry series and the main reference index over lags
#   -max_lag ... +max_lag (no plotting).
# Arguments:
#   - tsbl        : wide tibble/data.frame with 'date' and numeric series.
#   - main_index  : string; column name of the main reference index (y).
#   - target_code : string; column name of the target industry (x).
#   - max_lag     : positive integer; maximum lag size in each direction.
# Returns:
#   tibble with columns:
#     - lag         : numeric lags in [-max_lag, ..., +max_lag].
#     - correlation : CCF value at each lag.
#     - indicator   : character; repeated target_code for tracking.
# Notes:
#   - Pairwise NA removal before computation.
#   - Positive lag means x leads y by that many periods (R ccf convention).
#   - Suitable for heatmaps/argmax-per-indicator plots and rolling analyses.
# --------------------------------------------------------------------
get_ccf_full <- function(tsbl, main_index, target_code, max_lag = 12) {
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Pairwise complete cases
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  
  ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
  
  tibble(
    lag = as.numeric(ccf_obj$lag),
    correlation = as.numeric(ccf_obj$acf),
    indicator = target_code
  )
}

# --------------------------------------------------------------------
# Function: get_mi_full
# Purpose:
#   Compute (nonlinear) mutual information between a target series (x) and
#   the main index (y) at each lag k in [-max_lag, ..., +max_lag], adhering
#   to ccf()’s sign convention: k>0 → x leads y (we shift x backward by k).
# Arguments:
#   - tsbl        : wide tibble/data.frame with 'date' and numeric series.
#   - main_index  : string; column name of the main reference index (y).
#   - target_code : string; column name of the target industry (x).
#   - max_lag     : positive integer; maximum lag size in each direction.
# Returns:
#   tibble with columns:
#     - lag       : numeric lags in [-max_lag, ..., +max_lag].
#     - mi        : mutual information at each lag (bias-corrected).
#     - indicator : character; repeated target_code for tracking.
# Notes:
#   - Uses mpmi::cmi.pw() and reports $bcmi (bias-corrected MI).
#   - Pairwise NA removal per-lag; requires ≥5 overlapping points (else NA).
#   - Positive lag → x leads y; implemented as x_shifted = lag(x, k).
# --------------------------------------------------------------------
get_mi_full <- function(tsbl, main_index, target_code, max_lag = 12) {
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Pairwise complete cases
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  
  lags <- seq(-max_lag, max_lag)
  
  mi_vals <- map_dbl(lags, function(k) {
    x_shifted <- if (k > 0) dplyr::lag(x, n = k) else if (k < 0) dplyr::lead(x, n = -k) else x
    jj <- complete.cases(x_shifted, y)
    if (sum(jj) < 5) return(NA_real_)
    cmi.pw(x_shifted[jj], y[jj])$bcmi
  })
  
  tibble(lag = lags, 
         mi = mi_vals, 
         indicator = target_code)
}

# --------------------------------------------------------------------
# Function: get_dcor_full
# Purpose:
#   Compute distance correlation (nonparametric dependence; 0 iff indep for
#   Euclidean metrics) between x and y at each lag k in [-max_lag, ..., +max_lag],
#   matching ccf()’s sign convention.
# Arguments:
#   - tsbl        : wide tibble/data.frame with 'date' and numeric series.
#   - main_index  : string; column name of the main reference index (y).
#   - target_code : string; column name of the target industry (x).
#   - max_lag     : positive integer; maximum lag size in each direction.
# Returns:
#   tibble with columns:
#     - lag       : numeric lags in [-max_lag, ..., +max_lag].
#     - dcor      : distance correlation at each lag.
#     - indicator : character; repeated target_code for tracking.
# Notes:
#   - Pairwise NA removal per-lag; requires ≥5 overlapping points (else NA).
#   - Positive lag → x leads y (shift x backward by k).
# --------------------------------------------------------------------
get_dcor_full <- function(tsbl, main_index, target_code, max_lag = 12) {
  x <- tsbl[[target_code]]
  y <- tsbl[[main_index]]
  
  # Pairwise complete cases
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  
  lags <- seq(-max_lag, max_lag)
  
  dcor_vals <- map_dbl(lags, function(k) {
    x_shifted <- if (k > 0) dplyr::lag(x, n = k) else if (k < 0) dplyr::lead(x, n = -k) else x
    jj <- complete.cases(x_shifted, y)
    if (sum(jj) < 5) return(NA_real_)
    dcor(x_shifted[jj], y[jj])
  })
  
  tibble(lag = lags, dcor = dcor_vals, indicator = target_code)
}

# --------------------------------------------------------------------
# Function: get_dcor_pair
# Purpose:
#   Same as get_dcor_full(), but operates directly on two numeric vectors
#   x (target) and y (main), e.g., when pre-extracted or simulated.
# Arguments:
#   - x, y     : numeric vectors of equal length (not required, but both
#                are truncated to pairwise complete cases before analysis).
#   - max_lag  : positive integer; maximum lag size in each direction.
# Returns:
#   tibble with columns:
#     - lag   : numeric lags in [-max_lag, ..., +max_lag].
#     - dcor  : distance correlation at each lag.
# Notes:
#   - Pairwise NA removal per-lag; requires ≥5 overlapping points (else NA).
#   - Positive lag → x leads y; implemented as x_shifted = lag(x, k).
# --------------------------------------------------------------------
get_dcor_pair <- function(x, y, max_lag = 12) {
  idx <- complete.cases(x, y)
  x <- x[idx]; y <- y[idx]
  
  lags <- seq(-max_lag, max_lag)
  
  dcor_vals <- map_dbl(lags, function(k) {
    x_shifted <- if (k > 0) dplyr::lag(x, n = k) else if (k < 0) dplyr::lead(x, n = -k) else x
    jj <- complete.cases(x_shifted, y)
    if (sum(jj) < 5) return(NA_real_)
    dcor(x_shifted[jj], y[jj])
  })
  
  tibble(lag = lags, 
         dcor = dcor_vals)
}