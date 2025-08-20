# Small script just to have a quick look at turning points for lower digits
# subset to two-digits
ifo_subset_six_digits <- ifo_tbl %>% filter(level == 5) %>% select(-level)

# pivot to get just one value per row (appending industries & questions rowwise)
ifo_long <- ifo_subset_six_digits %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )


# group by by industries & questions and get markov probabilities (takes a while)
#ifo_probs_six_digits <- ifo_long %>%
#  group_by(industry_code, question_code) %>%
#  arrange(date, .by_group = TRUE) %>%
#  mutate(prob = get_markov_probabilities(value, date)) %>%
#  ungroup()

# LUS is the only anti-cyclical series, its upper-regime is corresponding to contraction
# thus we take counterprobabilities to 'swap' regimes
ifo_probs_six_digits <- ifo_probs_six_digits %>%
  mutate(prob = ifelse(question_code == "LUS", 1 - prob, prob))

# upper entries for C10000000 should be 1 
ifo_probs_six_digits %>% filter(question_code == "LUS")
# ------------------------------------------------------------------------------
# Mark turning points according to Markov probabilities 

# mark turning points by applying mark_turningpoints function to probs df
turning_points_df_six <- ifo_probs_six_digits %>%
  group_by(industry_code, question_code) %>%
  mark_turningpoints(upper = 0.66, lower = 0.33) %>%
  ungroup() %>%
  arrange(industry_code, question_code, date)


# extract all turning points and collect in tp_df
tp_df_six <- turning_points_df_six %>%
  filter(turning_point) %>%  # keep only rows marked as turning points
  group_by(industry_code, question_code) %>%
  summarize(
    expansion_tps   = list(date[phase == "expansion"]),
    contraction_tps = list(date[phase == "contraction"]),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Evaluate turning points with some standard metrics
eval_df_six <- evaluate_tp_df(tp_df_six,
                          main_exp_bb,
                          main_contr_bb,
                          valid_range = c(-3,6),
                          phase = "all")
eval_df_six %>% arrange(desc(f1_score))


#===============================================================================
# 5-digits
# ------------------------------------------------------------------------------

# subset to two-digits
ifo_subset_five_digits <- ifo_tbl %>% filter(level == 4) %>% select(-level)

# pivot to get just one value per row (appending industries & questions rowwise)
ifo_long <- ifo_subset_five_digits %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )


# group by by industries & questions and get markov probabilities (takes a while)
#ifo_probs_five_digits <- ifo_long %>%
#  group_by(industry_code, question_code) %>%
#  arrange(date, .by_group = TRUE) %>%
#  mutate(prob = get_markov_probabilities(value, date)) %>%
#  ungroup()

# LUS is the only anti-cyclical series, its upper-regime is corresponding to contraction
# thus we take counterprobabilities to 'swap' regimes
ifo_probs_six_digits <- ifo_probs_six_digits %>%
  mutate(prob = ifelse(question_code == "LUS", 1 - prob, prob))

# upper entries for C10000000 should be 1 
ifo_probs_six_digits %>% filter(question_code == "LUS")
# ------------------------------------------------------------------------------
# Mark turning points according to Markov probabilities 

# mark turning points by applying mark_turningpoints function to probs df
turning_points_df_six <- ifo_probs_six_digits %>%
  group_by(industry_code, question_code) %>%
  mark_turningpoints(upper = 0.66, lower = 0.33) %>%
  ungroup() %>%
  arrange(industry_code, question_code, date)


# extract all turning points and collect in tp_df
tp_df_six <- turning_points_df_six %>%
  filter(turning_point) %>%  # keep only rows marked as turning points
  group_by(industry_code, question_code) %>%
  summarize(
    expansion_tps   = list(date[phase == "expansion"]),
    contraction_tps = list(date[phase == "contraction"]),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Evaluate turning points with some standard metrics
eval_df_six <- evaluate_tp_df(tp_df_six,
                              main_exp_bb,
                              main_contr_bb,
                              valid_range = c(-3,6),
                              phase = "all")
eval_df_six %>% arrange(desc(f1_score))
