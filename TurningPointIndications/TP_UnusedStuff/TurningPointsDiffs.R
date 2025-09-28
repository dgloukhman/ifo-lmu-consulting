
library(here)
source(here("utils","setup_packages.R"))

install_packages_from_file()

source(here("utils","load_data.R"))


# load in data and preprocess
data_path <- here("Data")
data_path_dict <- here("Data")
ifo_tbl <- read_ifo_data(data_path)
ifo_tbl <- preprocess_ifo_data(ifo_tbl)

question_df <- read.csv(paste0(data_path, "/questions_codes_titles.csv"))
industries_df <- read.csv(paste0(data_path, "/industries_codes_titles.csv"))

question_vars <- ifo_tbl %>% select (-date, -industry_code, -level) %>% colnames() 


# subset to three-digits
ts_df <- ifo_tbl %>% 
  filter(level == 1) %>% 
  select(-level) %>% arrange(industry_code)

# ------------------------------------------------------------------------------
# Bry-Boschan TP of main series

# subset to main aggregate (whole manufacturing)
C00_subset <- subset(ifo_tbl, ifo_tbl$industry_code == 'C0000000')
C00_KLD <- C00_subset %>% select(date, KLD) %>% rename(main_kld = KLD) %>% slice(2:nrow(C00_subset))

C00_KLD_diffs <- C00_KLD %>%mutate(across(where(is.numeric), ~ .x - lag(.x))) %>% drop_na()

# apply function and extract expansion and contraction points)
bb_turning_points <- get_bb_turning_points(
  values = C00_subset$KLD,
  dates = C00_subset$date
)
main_exp_bb <- bb_turning_points$tp_expansion
main_contr_bb <- bb_turning_points$tp_contraction

# replacing with first diffs
ts_df_diffs <- ts_df %>%
  group_by(industry_code) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(across(where(is.numeric), ~ .x - lag(.x))) %>%
  ungroup() %>% 
  drop_na()

ifo_long_diffs <- ts_df_diffs %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )


#ifo_probs_two_digit <- ifo_long %>%
#    group_by(industry_code, question_code) %>%
#    arrange(date, .by_group = TRUE) %>%
#    mutate(prob = get_markov_probabilities(value, date)) %>%
#    ungroup()

ifo_probs_two_digit_diffs <- ifo_probs_two_digit 

ifo_probs_two_digit <- ifo_probs_two_digit %>%
  mutate(prob = ifelse(question_code == "LUS", 1 - prob, prob))

turning_points_df <- ifo_probs_two_digit %>%
  group_by(industry_code, question_code) %>%
  mark_turningpoints(upper = 0.9, lower = 0.1) %>%
  ungroup() %>%
  arrange(industry_code, question_code, date)


# extract all turning points and collect in tp_df
tp_df <- turning_points_df %>%
  filter(turning_point) %>%  # keep only rows marked as turning points
  group_by(industry_code, question_code) %>%
  summarize(
    expansion_tps   = list(date[phase == "expansion"]),
    contraction_tps = list(date[phase == "contraction"]),
    .groups = "drop"
  )

eval_df <- evaluate_tp_df(tp_df,
                          main_exp_bb,
                          main_contr_bb,
                          valid_range = c(0,6),
                          phase = "all")

eval_df %>% arrange(desc(f1_score))

#===============================================================================


# Retrieving lag value with highest value of ccf 
ts_max_lag_matrix  <- get_max_lag_matrix(
  tbl = ts_df_diffs,
  question_vars = question_vars,
  max_lag = 6,
  only_negative_lag = FALSE,
  C00_KLD = C00_KLD_diffs
)


# look at a small summary
ts_max_lag_matrix %>% 
  group_by(max_lag) %>% 
  summarise(
    n = n(),
    mean_max_corr = mean(max_corr, na.rm = TRUE)
  )

# Taking mildly lagging group 
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag %in% c(-4,-5,-6)) 

# Averaging Probabilities of cluster
cluster_df <- cluster_components_df
cluster_states_df <- average_cluster_probabilities(ifo_probs_two_digit_diffs, cluster_df, C00_KLD_diffs$date)

cluster_states_df <- mark_turningpoints(cluster_states_df, 
                                        lower = 0.4,
                                        upper = 0.55)

cluster_exp_tp <- cluster_states_df %>% filter(phase == "expansion", turning_point == TRUE) %>% pull(date)
cluster_contr_tp <- cluster_states_df %>% filter(phase == "contraction", turning_point == TRUE) %>% pull(date)

evaluate_indicator(main_exp_bb, cluster_exp_tp, c(-6,6))
evaluate_indicator(main_contr_bb, cluster_contr_tp, c(-6,6))
view(cluster_states_df)




