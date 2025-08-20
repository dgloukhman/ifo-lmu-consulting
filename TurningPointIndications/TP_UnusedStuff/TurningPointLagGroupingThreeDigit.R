# Grouping simply by lag value with highest correlation, then somehow rank by correlation strength

# function to install and load missing packages for project in general 
library(here)
source(here("utils","setup_packages.R"))

install_packages_from_file()

library(patchwork) # to stack ggplots, add to .txt later

source(here("utils","load_data.R"))

# also source functions from two-digit case
source(here("TurningPointIndications","TurningPointLagGrouping.R"))

# load in data and preprocess
data_path <- here("Data")
data_path_dict <- here("Data")
ifo_tbl <- read_ifo_data(data_path)
ifo_tbl <- preprocess_ifo_data(ifo_tbl)

question_df <- read.csv(paste0(data_path, "/questions_codes_titles.csv"))
industries_df <- read.csv(paste0(data_path, "/industries_codes_titles.csv"))



# extract turning points (running takes a while)
#tp_three_digit_df <- extract_markov_tp(ifo_tbl, 
#                           industry_codes = two_digit_aggregates, 
#                           question_codes = question_vars)


# df with ts of all two_digits
ts_three_digits_df <- ifo_tbl %>% 
  filter(level == 2) %>% 
  select(-level)


# apply to two digits 
ts_max_lag_matrix_three_digits  <- get_max_lag_matrix(
  tbl = ts_three_digits_df,
  question_vars = question_vars,
  max_lag = 6,
  only_negative_lag = FALSE
)

# look at a small summary
ts_max_lag_matrix_three_digits %>% 
  group_by(max_lag) %>% 
  summarise(
    n = n(),
    mean_max_corr = mean(max_corr, na.rm = TRUE)
  )

# (if wanting tp use multiple lags)
cluster_df <- ts_max_lag_matrix_three_digits %>% 
  filter(max_lag %in% c(-1,-2,-3,-4,-5,-6)) %>% 
  rename(question_code = question)

# first make cluster_df ready again for aggregate_tp function
cluster_df <- ts_max_lag_matrix_three_digits %>% 
  filter(max_lag == 0) %>% 
  rename(question_code = question)

#-------------------------------------------------------------------------------
# Rolling Window Approach 
#-------------------------------------------------------------------------------
# Idea: dont look at states but rather at turning point yes/no 
# if eg more than 50% Turning points in 3 months time frame -> Predict turning point 
# lets first just collect all turning point TRUE columns for expansion tps of cluster
cluster_df

# extract all expansion_tps from the cluster (accessing tp_df for this)
dates_list_exp_three <- map2(cluster_df$industry_code,
                       cluster_df$question_code,
                       ~ tp_three_digit_df %>% 
                         filter(aggregate == .x, question == .y) %>% 
                         pull(expansion_tps))

# combine into giant vector 
tp_vec_exp_three <- dates_list_exp_three %>% 
  flatten() %>% 
  unlist() %>% 
  as.Date()

# extract all expansion_tps from the cluster (accessing tp_df for this)
dates_list_contr_three <- map2(cluster_df$industry_code,
                         cluster_df$question_code,
                         ~ tp_three_digit_df %>% 
                           filter(aggregate == .x, question == .y) %>% 
                           pull(contraction_tps))

# combine into giant vector 
tp_vec_contr_three <- dates_list_contr_three %>% 
  flatten() %>% 
  unlist() %>% 
  as.Date()

# apply rolling window to filter out turning points
cluster_exp_tp <- get_rolling_tp(tp_vec_exp_three, 
                                 window_size = 5,
                                 sum_threshold = 35,
                                 return_df_filled = FALSE)

cluster_contr_tp <- get_rolling_tp(tp_vec_contr_three, 
                                   window_size = 5,
                                   sum_threshold = 35)

# ensure minimum distance between determined turning points 
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

# evaluate
evaluate_indicator(main_dates = main_m,
                   indicator_dates = cluster_contr_tp,
                   valid_lead_time = 6)


plot(cluster_exp_tp$month, cluster_exp_tp$n_turning_points, type = "l")
