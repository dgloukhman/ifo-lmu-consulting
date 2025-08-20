# collecting all sorts of relevant functions for turning points here, 
# check if second script necessary
# ------------------------------------------------------------------------------
# Function: df_codes_to_titles
# Purpose: 
#.  Receives df and changes entries of codes to actual titles, using 
#   'question_df' and 'industries_df' as a dictionary to translate (need to implement)
# Arguments:
#. - df                     : df to change codes of 
#. - data_path.             : data_path to 'question_df' and 'industries_df'
#. - question_code_col_name : name of column containing question codes
#. - industry_code_col_name : name of column containing industry codes
# ------------------------------------------------------------------------------
df_codes_to_titles <- function(df, data_path, question_code_col_name, industry_code_col_name) {
  df <- df %>%
    rename(question_code = all_of(question_code_col_name)) %>%
    left_join(question_df, by = "question_code") %>%
    select(-question_code) %>%
    relocate(question_title, .before = everything())
  
  df <- df %>% 
    rename(industry_code = all_of(industry_code_col_name) ) %>%
    left_join(industries_df, by = "industry_code") %>%
    select(-industry_code) %>%
    relocate(industry_title, .before = everything())
}

# ------------------------------------------------------------------------------
# Function: get_bb_turning_points
# Purpose:
#   Get Bry-Boschan Turning Points, identifying peaks/throughs of time series in 
#   hindsight. A through corresponds to an expansion turning points and a
#   peak corresponds contraction turning point -> 'We've reached
#   the peak of our expansion phase, it will go down for now.' 
#   Needs BCDating package
# Arguments:
#  - values   : vector containing values of the series  
#  - dates    : date vector, containing all dates of the series
#  - mincycle : Minimum duration to for a whole business cycle to happen
#  - minphase : Minimum duration for a phase (eg. expansion phase)
# Returns:
# - list containing two vectors filled with dates:
#  - expansion turning points
#  - contraction turning points
# ------------------------------------------------------------------------------
get_bb_turning_points <- function(values, dates, mincycle = 24, minphase = 6) {
  library(BCDating)
  
  # get start and end date to turn into ts object (requirement for BCDating package)
  start_year <- as.numeric(format(min(dates), "%Y"))
  start_month <- as.numeric(format(min(dates), "%m"))
  end_year <- as.numeric(format(max(dates), "%Y"))
  end_month <- as.numeric(format(max(dates), "%m"))
  
  ts_data <- ts(
    values,
    start = c(start_year, start_month),
    frequency = 12,
    end = c(end_year, end_month)
  )
  
  # run Bry-Boschan
  bb <- BCDating::BBQ(ts_data, mincycle = mincycle, minphase = minphase)
  
  # extract months of identified peaks and throughs
  tp_exp <- C00_subset$date[bb@troughs]
  tp_contr <- C00_subset$date[bb@peaks]
  
  return(list(tp_expansion = tp_exp, tp_contraction = tp_contr))
}

# ------------------------------------------------------------------------------
# Function : get_markov_probabilities
# Purpose : 
#  Runs simple intercept only linear model with two regimes and return the Markov
#   Switching probabilities for the upper regime. 
# Arguments:
#  - values       : vector containing values of the series  
#  - dates        : date vector, containing all dates of the series
#  - smooth_probs : True if wanting to return smoother probabilities
# Returns:
# - probs         : vector containing probabilities 
# Notes:
# - estimation not always stable, sometimes the model does not detect two proper regimes 
#   with meaningfully differing intercepts. Thus estimating again if intercept 
#   difference too small. 
# ------------------------------------------------------------------------------
get_markov_probabilities <- function(values, dates, smooth_probs = FALSE) {
  # simple model with intercept only
  lm <- lm(values ~ 1)
  
  sw <- c(TRUE, FALSE) # vector for msm, allowing mean to change but constant variance
  
  # run 3 times due to unstable estimation sometimes, making sure that differences are big enough
  msm_model <- NULL
  
  for(i in 1:3) {
    temp_model <- MSwM::msmFit(lm, k =2, sw =sw)
    
    # compare difference of the two intercepts to threshold value of 5 (arbitrary)
    if (abs(diff(temp_model@Coef[["(Intercept)"]])) < 5 & !is.null(msm_model)) next 
    
    # keep model if differences are big enough to actually trace regime changes
    msm_model <- temp_model    
  }
  # extract index of bigger regime (expansion)
  exp_regime_index <- which.max(msm_model@Coef[["(Intercept)"]])
  
  # extract smooth or filtered probabilities 
  if(smooth_probs){
    probs <- msm_model@Fit@smoProb[,exp_regime_index]
    probs <- probs[-1] #drop first line (MSwM adds first prob for some reason)
  } else {
    probs <-  msm_model@Fit@filtProb[,exp_regime_index]
  }
  return(probs)
}

# ------------------------------------------------------------------------------
# Function : mark_turningpoints
# Purpose : 
#  Runs through markov probabilities and marks turning points:
#.  - if crossing upper threshold for first time -> expansion TP
#.  - if crossing lower thresholf for first time -> contraction TP
#.  - if in between -> uncertain, no TP 
# Arguments:
#  - probs_df : df with columns: date,industry_code, questions_code, value, probs 
#  - upper    : upper threshold for TP
#  - lower    : lower threshold for TP
# Returns:
# - Two new columns when applied to probs_df:
#    - phase : marking which regime currently (according to last TP)
#    - turning_point : TRUE if this month is turning points
# ------------------------------------------------------------------------------
mark_turningpoints <- function(probs_df, upper = 0.66, lower = 0.33) {
  probs_df %>%
    arrange(date) %>%
    mutate(
      phase = case_when(
        prob > upper~ "expansion",
        prob < lower ~ "contraction",
        TRUE ~ "uncertain"
      ),
      prev_phase = lag(phase, default = first(phase)),
      turning_point = case_when(
        (phase == "expansion" & prev_phase != "expansion") ~ TRUE,
        (phase == "contraction" & prev_phase != "contraction") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(-prev_phase)
}

# ------------------------------------------------------------------------------
# Function : evaluate_indicator
# Purpose : 
#   Runs through main_dates given and checks for each main date, if any
#   indicator date is precessing the main_date in given valid_range.
#   Negative values in valid_range allow for lagging indicators to count. 
#   counting of 
# Arguments:
#  - main_dates      : vector of dates to be predicted
#  - indicator_dates : vector of dates potentially indicating 
#  - valid_rage      : vector containing tolerated range to count as indicator 
# Returns:
# - tibble containing:
#.   - hits
#.   - total_pred
#.   - total_actual
#.   - precision
#.   - recall
#.   - f1_score
#.   - lead_time_avg
#.   - lead_time_sd
# ------------------------------------------------------------------------------
evaluate_indicator <- function(main_dates, indicator_dates, valid_range = c(0,6)) {
  hits <- 0
  lead_times <- c()
  
  for (main_date in main_dates) {
    main_date <- as.Date(main_date)
    diffs <- -((year(indicator_dates) - year(main_date)) * 12 + 
      (month(indicator_dates) - month(main_date)))
    
    valid_diffs <- diffs[diffs <= max(valid_range) & diffs >= min(valid_range)]
    
    if (length(valid_diffs) > 0) {
      # count only once per main_date
      hits <- hits + 1
      lead_times <- c(lead_times, min(valid_diffs) )
    }
  } 
  
  total_pred <- length(indicator_dates)
  total_actual <- length(main_dates)
  
  precision <- if (total_pred > 0) hits / total_pred else 0
  recall    <- if (total_actual > 0) hits / total_actual else 0
  f1_score  <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0
  
  tibble(
    hits = hits,
    total_pred = total_pred,
    total_actual = total_actual,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    lead_time_avg = if (length(lead_times) > 0) mean(lead_times) else NA_real_,
    lead_time_sd = if (length(lead_times) > 0) sd(lead_times) else NA_real_
  )
}

# ------------------------------------------------------------------------------
# Function : evaluate_tp_df
# Purpose : 
#   Receives tp_df and evaluates with standard evaluation metrics
# Arguments:
#  - tp_df          : tibble containing columns:
#                       - industry_code
#                       - question_code
#                       - expansion_tps
#                       - contraction_tps
#  - main_tp_exp    : vector of main expansion dates to validate against
#  - main_tp_contr  : vector of main expansion dates to validate against
#  - valid_rage     : vector containing tolerated range to count as indicator 
#  - phase          : Either "all", "expansion" or "contraction", indicating
#                     which type of turning points to evaluate 
# Returns:
# - tibble containing:
#.   - hits
#.   - total_pred
#.   - total_actual
#.   - precision
#.   - recall
#.   - f1_score
#.   - lead_time_avg
#.   - lead_time_sd
# ------------------------------------------------------------------------------
evaluate_tp_df <- function(tp_df, main_tp_exp, main_tp_contr, valid_range = c(0,6), phase = "all") {
  
  expansion_eval <- tp_df %>% 
    rowwise() %>% 
    mutate(
      eval = list(evaluate_indicator(main_tp_exp, expansion_tps, valid_range))
    ) %>% 
    unnest(eval)
  
  if(phase == "expansion"){
    return(expansion_eval)
  }
  
  contraction_eval <- tp_df %>% 
    rowwise() %>% 
    mutate(
      eval = list(evaluate_indicator(main_tp_contr, contraction_tps, valid_range))    ) %>% 
    unnest(eval)
  
  if(phase == "contraction"){
    return(contraction_eval)
  }
  
  summary_eval <- expansion_eval %>%
    select(industry_code, question_code, expansion_tps, contraction_tps,
           hits, total_pred, total_actual, precision, recall, f1_score, lead_time_avg, lead_time_sd) %>%
    left_join(
      contraction_eval %>%
        select(industry_code, question_code, hits, total_pred, total_actual, precision, recall, f1_score, lead_time_avg, lead_time_sd),
      by = c("industry_code", "question_code"),
      suffix = c("_exp", "_contr")
    ) %>%
    rowwise() %>%
    mutate(
      hits          = sum(hits_exp, hits_contr, na.rm = TRUE),
      total_pred    = sum(total_pred_exp, total_pred_contr, na.rm = TRUE),
      total_actual  = sum(total_actual_exp, total_actual_contr, na.rm = TRUE),
      precision     = mean(c(precision_exp, precision_contr), na.rm = TRUE),
      recall        = mean(c(recall_exp, recall_contr), na.rm = TRUE),
      f1_score      = mean(c(f1_score_exp, f1_score_contr), na.rm = TRUE),
      lead_time_avg = mean(c(lead_time_avg_exp, lead_time_avg_contr), na.rm = TRUE),
      lead_time_sd  = mean(c(lead_time_sd_exp, lead_time_sd_contr), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(industry_code, question_code, expansion_tps, contraction_tps,
           hits, total_pred, total_actual,
           precision, recall, f1_score, lead_time_avg, lead_time_sd)
  
}

#===============================================================================
# Grouping/Clustering functions

# function to return lag value with highest correlation for each ts and corresponding strength
get_max_lag_matrix <- function(tbl, question_vars, max_lag, only_negative_lag = TRUE) {
  # returns matrix containing industry_code, question, data, max_lag, max_corr
  # (data: column containing dfs with date, main_value, value) (not sure if needing this)
  
  get_max_lag <- function(x, y, max_lag = 6, only_negative_lag = TRUE) {
    ccf_result <- ccf(x, y, plot = FALSE, na.action = na.pass, lag.max = max_lag)
    lags <- as.numeric(ccf_result$lag)
    acf_vals <- as.numeric(ccf_result$acf)
    
    if (only_negative_lag) {
      valid_idx <- which(lags <= 0)
      lags <- lags[valid_idx]
      acf_vals <- acf_vals[valid_idx]
    }
    
    # Find index of strongest correlation
    max_idx <- which.max(abs(acf_vals))
    list(
      max_lag = lags[max_idx],
      max_corr = acf_vals[max_idx]
    )
  }
  
  tbl %>%
    left_join(C00_KLD, by = "date") %>%
    pivot_longer(cols = all_of(question_vars), names_to = "question_code", values_to = "value") %>%
    group_by(industry_code, question_code) %>%
    nest() %>%
    mutate(
      max_info = map(data, ~ get_max_lag(.x$value, .x$main_kld, max_lag, only_negative_lag))
    ) %>%
    unnest_wider(max_info) %>%
    ungroup()
}

# function receiving df full of markov probs, cluster_df containing industry_code, question_code
# and date column (can be taken from C00 eg)
average_cluster_probabilities <- function(probs_df, cluster_df, date) {
  cluster_prob_df <- data.frame(date = date)
  cluster_prob_df$prob <- 0
  
  for (i in 1:nrow(cluster_df)) {
    prob <- probs_df %>% filter(
      question_code == cluster_df$question_code[i],
      industry_code == cluster_df$industry_code[i]
    ) %>% pull(prob)
    cluster_prob_df$prob <- cluster_prob_df$prob + prob
  }
  
  cluster_prob_df$prob <- cluster_prob_df$prob / nrow(cluster_df)
  return(cluster_prob_df)
}
