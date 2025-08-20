# Grouping simply by lag value with highest correlation, then somehow rank by correlation strength

# function to install and load missing packages for project in general 
library(here)
source(here("utils","setup_packages.R"))

install_packages_from_file()

library(patchwork) # to stack ggplots, add to .txt later

source(here("utils","load_data.R"))

# load in data and preprocess
data_path <- here("Data")
data_path_dict <- here("Data")
ifo_tbl <- read_ifo_data(data_path)
ifo_tbl <- preprocess_ifo_data(ifo_tbl)

question_df <- read.csv(paste0(data_path, "/questions_codes_titles.csv"))
industries_df <- read.csv(paste0(data_path, "/industries_codes_titles.csv"))

#-------------------------------------------------------------------------------
# Group TS according to same lag values with max correlation 
#-------------------------------------------------------------------------------

# df with ts of all two_digits
ts_two_digits_df <- ifo_tbl %>% 
  filter(level == 1) %>% 
  select(-level)

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
    pivot_longer(cols = all_of(question_vars), names_to = "question", values_to = "value") %>%
    group_by(industry_code, question) %>%
    nest() %>%
    mutate(
      max_info = map(data, ~ get_max_lag(.x$value, .x$main_kld, max_lag, only_negative_lag))
    ) %>%
    unnest_wider(max_info) %>%
    ungroup()
}

# apply to two digits 
ts_max_lag_matrix  <- get_max_lag_matrix(
  tbl = ts_two_digits_df,
  question_vars = question_vars,
  max_lag = 6,
  only_negative_lag = FALSE
)

# look at a small summary
ts_max_lag_matrix %>% 
  group_by(max_lag) %>% 
  summarise(
    n = n(),
    mean_max_corr = mean(max_corr, na.rm = TRUE)
  )

# -> decent mean correlation at all ts with lag = -6 and further n = 8 seems reasonable,
# -> continue to work with this, by looking at ways to combine their signals 

# (if wanting tp use multiple lags)
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag %in% c(-4,-5,-6)) %>% 
  rename(question_code = question)

# first make cluster_df ready again for aggregate_tp function
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag == -3) %>% 
  rename(question_code = question)

#-------------------------------------------------------------------------------
# Averaging binary states of single groups
#------------------------------------------------------------------------------
# empty dataframe to store binary
cluster_states_df <- data.frame(date = C00_KLD$date) 
cluster_states_df$state <- 0

# Loop through industry/question pairs of cluster and aggregate binary states
for (i in 1:nrow(cluster_df)) {
  # using aggregate_tp function to simply convert into binary states
  temp_df <- aggregate_tp(ts_df, 
                          agg_vals = cluster_df$industry_code[i],
                          question_vals = cluster_df$question_code[i])
  # adding to total count
  cluster_states_df$state <- cluster_states_df$state + temp_df$state
}

# averaging with cluster size
cluster_states_df$state <- cluster_states_df$state / nrow(cluster_df)

# df marking turning points and adding back date column
cluster_tp_df <- mark_turningpoints(cluster_states_df$state)
cluster_tp_df$date <- cluster_states_df$date

# extract turning points
cluster_exp_tp <- cluster_tp_df %>% filter(turning_point == TRUE & phase == 'expansion') %>% pull(date)
cluster_contr_tp <- cluster_tp_df %>% filter(turning_point == TRUE & phase == 'contraction') %>% pull(date)


cluster_exp_tp
exp_main_bb
evaluate_indicator(main_dates = exp_main_bb,
                   indicator_dates = cluster_exp_tp,
                   valid_lead_time = 6)

cluster_contr_tp
contr_main_bb
evaluate_indicator(main_dates = contr_main_bb,
                   indicator_dates = cluster_contr_tp,
                   valid_lead_time = 6)

# lets take a quick exemplary look at the individual expansion TPs of the lag -3 group
group_3 <- cluster_df %>% filter (max_lag == -3)
group_tps

group_tps <- map2(group_3$industry_code, group_3$question_code, ~ {
  tp_df %>%
    filter(aggregate == .x, question == .y) %>%
    pull(expansion_tps)
})
print(group_tps)

# -> They do look quite similar roughly 
# (n ~ 10, and roughly same years/months)


#-------------------------------------------------------------------------------
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

cluster_exp_tp
contr_main_bb
exp_main_bb

# Taking group with highest correlation at lag 0 and using a valid_lead_time of 12
# leads to good preditive performance -> However mostly due to nature of Markov points
#-------------------------------------------------------------------------------
# Comparing to Markov switching of C00 KLD 
#-------------------------------------------------------------------------------
# comparing them to using filtered probs of simply C00 KLD 

markov_tp_main <- ms_turning_points(ts = C00_subset$KLD, 
                                    dates = C00_subset$date, 
                                    smooth_probs = FALSE) #smooth to use all data

main_exp_tp <- markov_tp_main %>% filter(turning_point, phase == "expansion") %>% pull(date)
main_contr_tp <- markov_tp_main %>% filter(turning_point, phase == "contraction") %>% pull(date)

# evaluate
evaluate_indicator(main_dates = exp_main_bb,
                   indicator_dates = main_contr_tp,
                   valid_lead_time = 12)

evaluate_indicator(main_dates = contr_main_bb,
                   indicator_dates = main_exp_tp,
                   valid_lead_time = 12)

# -> Using markov of simply main series has also predictive power, whereas with a 
# little lower lead_time and less sd of this leadtime 