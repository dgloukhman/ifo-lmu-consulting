# Main Script, where gathering all Turning Points (TP) Stuff
# Will repeat this script with three digits in a separate file

# function to install and load missing packages for project in general 
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

# quickly backup current tp_df 
#tp_df_backup_twodigits <- tp_df
#eval_df_backup_twodigits <- eval_df

# ------------------------------------------------------------------------------
# Bry-Boschan TP of main series

# subset to main aggregate (whole manufacturing)
C00_subset <- subset(ifo_tbl, ifo_tbl$industry_code == 'C0000000')

# apply function and extract expansion and contraction points)
bb_turning_points <- get_bb_turning_points(
  values = C00_subset$KLD,
  dates = C00_subset$date
)
main_exp_bb <- bb_turning_points$tp_expansion
main_contr_bb <- bb_turning_points$tp_contraction


# create df with turning points to plot them along the climate graph
turning_points <- C00_subset %>%
  mutate(
    phase = case_when(
      date %in% main_exp_bb ~ "expansion",
      date %in% main_contr_bb ~ "contraction",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(date %in% c(tp_exp, tp_contr)) %>%
  select(date, KLD, phase)

# Plot climate graph with turning points along the line
ggplot(C00_subset, aes(x = date, y = KLD)) +
  geom_line() +
  geom_point(
    data = turning_points,
    aes(x = date, y = KLD, color = phase),
    size = 2
  ) +
  scale_color_manual(
    values = c("expansion" = "darkgreen", "contraction" = "darkred")
  ) +
  labs(
    x = "",
    y = "Manufacturing Business Climate",
    color = "Bry-Boschan Turning Points"
  )
theme_minimal()


# ------------------------------------------------------------------------------
# Get Markov probabilities of all subseries 

# subset to two-digits
ifo_subset_two_digits <- ifo_tbl %>% filter(level == 1) %>% select(-level)

# pivot to get just one value per row (appending industries & questions rowwise)
ifo_long <- ifo_subset_two_digits %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )

lus_subset <- ifo_tbl %>% filter(industry_code == "C2400000") 
plot(lus_subset$date, lus_subset$LUS, type ="l")

# group by by industries & questions and get markov probabilities (takes a while)
#ifo_probs_two_digit <- ifo_long %>%
#  group_by(industry_code, question_code) %>%
#  arrange(date, .by_group = TRUE) %>%
#  mutate(prob = get_markov_probabilities(value, date)) %>%
#  ungroup()

# LUS is the only anti-cyclical series, its upper-regime is corresponding to contraction
# thus we take counterprobabilities to 'swap' regimes
ifo_probs_two_digit <- ifo_probs_two_digit %>%
  mutate(prob = ifelse(question_code == "LUS", 1 - prob, prob))

# upper entries for C10000000 should be 1 
ifo_probs_two_digit %>% filter(question_code == "LUS")
# ------------------------------------------------------------------------------
# Mark turning points according to Markov probabilities 

# mark turning points by applying mark_turningpoints function to probs df
turning_points_df <- ifo_probs_two_digit %>%
  group_by(industry_code, question_code) %>%
  mark_turningpoints(upper = 0.66, lower = 0.33) %>%
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

# ------------------------------------------------------------------------------
# Evaluate turning points with some standard metrics
eval_df <- evaluate_tp_df(tp_df,
                          main_exp_bb,
                          main_contr_bb,
                          valid_range = c(0,6),
                          phase = "all")

eval_df %>% arrange(desc(f1_score)) 
# -> Rather bad, there is not really a single time series standing out as predictive
# Probably also due to nature of Markov Points in comparison to Bry-Boschan points,
# so allowing for a little lag of three months after Bry-Boschan turning points
eval_df <- evaluate_tp_df(tp_df,
                          main_exp_bb,
                          main_contr_bb,
                          valid_range = c(-3,6),
                          phase = "all")

eval_df %>% arrange(desc(f1_score)) 
# -> still no single series sticking out, so lets try some grouping 

#===============================================================================
# Grouping Subaggregates and looking at combined TP Indications
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Grouping by highest lag correlation

# Retrieving lag value with highest value of ccf 
ts_max_lag_matrix  <- get_max_lag_matrix(
  tbl = ifo_subset_two_digits,
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

#-------------------------------------------------------------------------------
# Taking mildly lagging group 
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag %in% c(-1,-2,-3)) 

# Averaging Probabilities of cluster
cluster_states_df <- average_cluster_probabilities(ifo_probs_two_digit, cluster_df, C00_KLD$date)

cluster_states_df <- mark_turningpoints(cluster_states_df, 
                                        lower = 0.33,
                                        upper = 0.66)

cluster_exp_tp <- cluster_states_df %>% filter(phase == "expansion", turning_point == TRUE) %>% pull(date)
cluster_contr_tp <- cluster_states_df %>% filter(phase == "contraction", turning_point == TRUE) %>% pull(date)

evaluate_indicator(main_exp_bb, cluster_exp_tp, c(-3,6))
evaluate_indicator(main_contr_bb, cluster_contr_tp, c(-3,6))
cluster_exp_tp
main_exp_bb
cluster_contr_tp
main_contr_bb

# The turning points of the cluster do roughly correspond to the main turning points,
# however, usually lagging half a year up to two years behind 

#-------------------------------------------------------------------------------
# Taking stronger lagging group 
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag %in% c(-4,-5,-6)) 
cluster_states_df <- average_cluster_probabilities(ifo_probs_two_digit, cluster_df, C00_KLD$date)

cluster_states_df <- mark_turningpoints(cluster_states_df, 
                                        lower = 0.33,
                                        upper = 0.66)

cluster_exp_tp <- cluster_states_df %>% filter(phase == "expansion", turning_point == TRUE) %>% pull(date)
cluster_contr_tp <- cluster_states_df %>% filter(phase == "contraction", turning_point == TRUE) %>% pull(date)

evaluate_indicator(main_exp_bb, cluster_exp_tp, c(-3,6))
evaluate_indicator(main_contr_bb, cluster_contr_tp, c(-3,6))
cluster_exp_tp
main_exp_bb
cluster_contr_tp
main_contr_bb
# much more noisy compared to the less lagging group and also behind by a few years

#===============================================================================
# Opposite signal - Indicating turning points of opposite phase
#-------------------------------------------------------------------------------
# Idea: 'By now all series of this group reached the phase with their Markov TP
#.        -> Probably already over soon' 
#.    - So we switch regimes seeing if we might get a consistent lead_time for opposite TP 

#-------------------------------------------------------------------------------
# Taking mildly lagging group again
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag %in% c(2,1,0,-1,-2)) 
cluster_df <- ts_max_lag_matrix
# Averaging Probabilities of cluster
cluster_states_df <- average_cluster_probabilities(ifo_probs_two_digit, cluster_df, C00_KLD$date)

cluster_states_df <- mark_turningpoints(cluster_states_df, 
                                        lower = 0.33,
                                        upper = 0.66)

cluster_exp_tp <- cluster_states_df %>% filter(phase == "expansion", turning_point == TRUE) %>% pull(date)
cluster_contr_tp <- cluster_states_df %>% filter(phase == "contraction", turning_point == TRUE) %>% pull(date)

evaluate_indicator(main_contr_bb, cluster_exp_tp, c(0,12))
evaluate_indicator(main_exp_bb, cluster_contr_tp, c(0,12))
cluster_exp_tp
main_contr_bb
cluster_contr_tp
main_exp_bb

# 
#-------------------------------------------------------------------------------