#===============================================================================
# Rolling Window Approach 
#-------------------------------------------------------------------------------
# Idea: dont look at states but rather at turning point yes/no 
# if eg more than 50% Turning points in 3 months time frame -> Predict turning point 
# lets first just collect all turning point TRUE columns for expansion tps of cluster
cluster_df

# extract all expansion_tps from the cluster (accessing tp_df for this)
dates_list_exp <- map2(cluster_df$industry_code,
                       cluster_df$question_code,
                       ~ tp_df %>% 
                         filter(aggregate == .x, question == .y) %>% 
                         pull(expansion_tps))

# combine into giant vector 
tp_vec_exp <- dates_list_exp %>% 
  flatten() %>% 
  unlist() %>% 
  as.Date()

# extract all expansion_tps from the cluster (accessing tp_df for this)
dates_list_contr <- map2(cluster_df$industry_code,
                         cluster_df$question_code,
                         ~ tp_df %>% 
                           filter(aggregate == .x, question == .y) %>% 
                           pull(contraction_tps))

# combine into giant vector 
tp_vec_contr <- dates_list_contr %>% 
  flatten() %>% 
  unlist() %>% 
  as.Date()

# function receives all turning points given as date vector and returns selection
# according to rolling window
get_rolling_tp <- function(tp_vec, window_size, sum_threshold,return_df_filled = FALSE){
  # tp_vec - Date Vector containing all turning points
  # window_size - Size of rolling window (should be uneven for symmetry)
  # sum_threshold - How many individual tp in window to become tp 
  # return_df_filled - True if wanting to look at counts occuring
  
  window_range <- min(1, (window_size-1)/2)
  
  # create df with counts 
  df_month <- tibble(month = floor_date(tp_vec, "month")) %>%
    count(month, name = "n_turning_points") 
  
  # now apply rolling window
  df_filled <- tibble(month = C00_KLD$date) %>%
    left_join(df_month, by = "month") %>%
    mutate(n_turning_points = replace_na(n_turning_points, 0))
  
  if(return_df_filled){
    return(df_filled)
  }
  
  # rolling sum, to fill up with sums of neighbouring months
  rolling_sum <- numeric(nrow(df_filled))
  
  # loop through df and fill in rolling sum
  for (i in 1:nrow(df_filled)) {
    start_idx <- max(1, i - window_range)
    end_idx   <- min(nrow(df_filled), i + window_range)
    rolling_sum[i] <- sum(df_filled$n_turning_points[start_idx:end_idx])
  }
  
  # filter dates according to sum_threshold
  cluster_exp_tp <- df_filled %>%
    mutate(rolling_sum = rolling_sum) %>%
    filter(rolling_sum > sum_threshold) %>% 
    pull(month)
}  

# function to only keep turning points with large enough distance
filter_close_tp <- function(date_vec, min_distance_month = 6) {
  min_days <- min_distance_month * 30 #not exact but doesnt matter in our case
  
  # initialise with first date
  last_kept <- date_vec[1]
  filtered_dates <- last_kept
  
  # loop through dates and keep only if difference to last kept big enough
  for (i in 2:length(date_vec)) {
    diff <- as.numeric(date_vec[i] - last_kept)
    
    if(diff > min_days) {
      last_kept <- date_vec[i]    
      filtered_dates <- cbind(filtered_dates, date_vec[i])
    }
  }
  filtered_dates <- as.Date(filtered_dates)
}

cluster_exp_tp <- get_rolling_tp(tp_vec_exp, 
                                 window_size = 7,
                                 sum_threshold = 4)

cluster_contr_tp <- get_rolling_tp(tp_vec_contr, 
                                   window_size = 7,
                                   sum_threshold = 4)

cluster_exp_tp <- filter_close_tp(cluster_exp_tp, 6)
cluster_contr_tp <- filter_close_tp(cluster_contr_tp, 6)

# evaluate
evaluate_indicator(main_dates = exp_main_bb,
                   indicator_dates = cluster_contr_tp,
                   valid_lead_time = 6)


evaluate_indicator(main_dates = contr_main_bb,
                   indicator_dates = cluster_exp_tp,
                   valid_lead_time = 6)

