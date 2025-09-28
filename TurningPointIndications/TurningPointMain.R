# Main Script for turning points 
# function to install and load missing packages for project in general 
library(here)
source(here("utils","setup_packages.R"))

install_packages_from_file()

source(here("utils","load_data.R"))
source(here("TurningPointIndications","turningpoint_utils.R"))


# load in data and preprocess
data_path <- here("Data")
data_path_dict <- here("Data")
ifo_tbl <- read_ifo_data(data_path)
ifo_tbl <- preprocess_ifo_data(ifo_tbl)

question_df <- read.csv(paste0(data_path, "/questions_codes_titles.csv"))
industries_df <- read.csv(paste0(data_path, "/industries_codes_titles.csv"))

question_vars <- ifo_tbl %>% select (-date, -industry_code, -level) %>% colnames() 

# ------------------------------------------------------------------------------
# Bry-Boschan TP of main series

# subset to main aggregate (whole manufacturing)
C00_subset <- subset(ifo_tbl, ifo_tbl$industry_code == 'C0000000')
C00_KLD <- C00_subset$KLD

# apply function and extract expansion and contraction points of main series 
bb_turning_points <- get_bb_turning_points(
  values = C00_subset$KLD,
  dates = C00_subset$date
)
main_exp_bb <- bb_turning_points$tp_expansion
main_contr_bb <- bb_turning_points$tp_contraction


# create df with BB turning points to plot them along main series 
turning_points <- C00_subset %>%
  mutate(
    phase = case_when(
      date %in% main_exp_bb ~ "expansion",
      date %in% main_contr_bb ~ "contraction",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(date %in% c(main_exp_bb, main_contr_bb)) %>%
  select(date, KLD, phase)

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
  ) +
theme_minimal()

#===============================================================================
# Using Markobv Regime Switching to get (realtime) turning points of subseries 
# ------------------------------------------------------------------------------
# Get Markov probabilities of all subseries (two-digits)

# subset to two-digits
ifo_subset_two_digits <- ifo_tbl %>% filter(level == 1) %>% select(-level)

# pivot 
ifo_long <- ifo_subset_two_digits %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )

ifo_probs_two_digit <- read.csv('/Users/jakobfreytag/Desktop/R Directionaries/ifo_probs_backup/ifo_probs_two_digits.csv')
# group by by industries & questions and get markov probabilities (takes a while)
#ifo_probs_two_digit <- ifo_long %>%
#  group_by(industry_code, question_code) %>%
#  arrange(date, .by_group = TRUE) %>%
#  mutate(prob = get_markov_probabilities(value, date)) %>%
#  ungroup()


# LUS is the only anti-cyclical question, its upper-regime is corresponding to contraction,
# thus we take counterprobabilities to 'swap' regimes
ifo_probs_two_digit <- ifo_probs_two_digit %>%
  mutate(prob = ifelse(question_code == "LUS", 1 - prob, prob))

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
# so allowing for a little lag of three months after Bry-Boschan turning points:
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
# getting c00 ready
C00_kld_df <- C00_subset %>% select(date, KLD) %>% rename(main_kld = KLD)

# Retrieving lag value with highest value of ccf 
ts_max_lag_matrix  <- get_max_lag_matrix(
  tbl = ifo_subset_two_digits,
  question_vars = question_vars,
  max_lag = 6,
  only_negative_lag = FALSE,
  C00_KLD = C00_kld_df
)

# look at a small summary
ts_max_lag_matrix %>% 
  group_by(max_lag) %>% 
  summarise(
    n = n(),
    mean_max_corr = mean(max_corr, na.rm = TRUE)
  )

#-------------------------------------------------------------------------------
# taking group with roughly composite moving characteristics
cluster_df <- ts_max_lag_matrix %>% 
  filter(max_lag %in% c(1,0,-1)) 

# Averaging Probabilities of cluster
cluster_states_df <- average_cluster_probabilities(ifo_probs_two_digit, cluster_df, C00_kld_df$date)

cluster_states_df <- mark_turningpoints(cluster_states_df, 
                                        lower = 0.33,
                                        upper = 0.66)

cluster_exp_tp <- cluster_states_df %>% filter(phase == "expansion", turning_point == TRUE) %>% pull(date)
cluster_contr_tp <- cluster_states_df %>% filter(phase == "contraction", turning_point == TRUE) %>% pull(date)

evaluate_indicator(main_contr_bb, cluster_exp_tp, c(0,12))
evaluate_indicator(main_exp_bb, cluster_contr_tp, c(0,12))

cluster_exp_tp
cluster_contr_tp
main_exp_bb
main_contr_bb

# Plotting cluster turning points alongside C00 KLD with main turning points 
# little helper df
tp_plot_df <- bind_rows(
  tibble(date = as.Date(cluster_exp_tp),  phase = "expansion",   type = "Markov Switching"),
  tibble(date = as.Date(cluster_contr_tp), phase = "contraction", type = "Markov Switching"),
  tibble(date = as.Date(main_exp_bb),      phase = "expansion",   type = "Bry-Boschan"),
  tibble(date = as.Date(main_contr_bb),    phase = "contraction", type = "Bry-Boschan")
) %>%
  # Join with your KLD values to get y-coordinate for plotting
  left_join(C00_subset %>% select(date, KLD), by = "date")

# plot
ggplot(C00_subset, aes(x = date, y = KLD)) +
  geom_line() +
  geom_point(
    data = tp_plot_df,
    aes(x = date, y = KLD, color = interaction(phase, type)),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "expansion.Bry-Boschan" = "#4CBB17",
      "contraction.Bry-Boschan" = "#FF2400",
      "expansion.Markov Switching" = "#708238",
      "contraction.Markov Switching" = "#7C0A02"
    ),
    labels = c(
      "expansion.Bry-Boschan" = "Main BB-Expansion",
      "contraction.Bry-Boschan" = "Main BB-Contraction",
      "expansion.Markov Switching" = "Group Markov Expansion",
      "contraction.Markov Switching" = "Group Markov Contraction"
    ),
    name = "Turning Points"
  ) +
  labs(
    x = "",
    y = "Main Index"
  ) +
  theme_minimal()
# The turning points of the cluster do roughly correspond to the main turning points,
# however, usually lagging half a year up to two years behind 




 
