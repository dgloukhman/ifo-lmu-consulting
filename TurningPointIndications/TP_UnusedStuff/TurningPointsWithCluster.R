# subset to main aggregate (whole manufacturing)
C00_subset <- subset(ifo_tbl, ifo_tbl$industry_code == 'C0000000')
C00_KLD <- C00_subset %>% select(date, KLD) %>% rename(main_kld = KLD)

# apply function and extract expansion and contraction points)
bb_turning_points <- get_bb_turning_points(
  values = C00_subset$KLD,
  dates = C00_subset$date
)
main_exp_bb <- bb_turning_points$tp_expansion
main_contr_bb <- bb_turning_points$tp_contraction


ifo_subset_three_digits <- ifo_tbl %>% filter(level == 2) %>% select(-level)

# pivot to get just one value per row (appending industries & questions rowwise)
ifo_long_three_digits <- ifo_subset_three_digits %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )

ifo_probs_three_digits <- read.csv('/Users/jakobfreytag/Desktop/R Directionaries/ifo_probs_backup/ifo_probs_three_digits.csv')

cluster_states_df <- average_cluster_probabilities(ifo_probs_three_digits, cluster_components_df, C00_KLD$date)

cluster_states_df <- mark_turningpoints(cluster_states_df, 
                                        lower = 0.33,
                                        upper = 0.66)

cluster_exp_tp <- cluster_states_df %>% filter(phase == "expansion", turning_point == TRUE) %>% pull(date)
cluster_contr_tp <- cluster_states_df %>% filter(phase == "contraction", turning_point == TRUE) %>% pull(date)

evaluate_indicator(main_exp_bb, cluster_exp_tp, c(-3,6))
evaluate_indicator(main_contr_bb, cluster_contr_tp, c(-3,6))

cluster_exp_tp
main_exp_bb
# Main Script,