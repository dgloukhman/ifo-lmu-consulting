# Try to use this script to contain everything --------

# This time no clustering technique
# Group ts by same lag value with highest correlation
# Then rank according to correlation strength (maybe some cutoff?) 


# Looking for turning point indications. 
# Cluster based on euclidean distance of CCF of Markov Prob values
# Combine Binary states of a cluster and determine turning points based on 033/0.66
# (currently excluding LUS, as extracting markov probs was run without)

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
# Extracting Markov Probabilities (takes a while, maybe get rid of loop)
#-------------------------------------------------------------------------------
two_digit_aggregates <- c("C1000000", "C1100000", "C1300000", "C1400000", "C1500000", "C1600000", "C1700000", 
                          "C1800000", "C1900000", "C2000000", "C2200000", "C2300000", "C2400000", "C2500000", 
                          "C2600000", "C2700000", "C2800000", "C2900000", "C3000000", "C3100000", "C3200000")

extract_markov_probs <- function(ts, dates, smooth_probs = FALSE) {
  # simple model with intercept only
  lm <- lm(ts ~ 1)
  
  sw <- c(TRUE, FALSE) # vector for msm, allowing mean to change but constant variance
  
  # run 5 times due to unstable estimation sometimes, making sure that differences are big enough
  msm_model <- NULL
  
  for(i in 1:3) {
    temp_model <- MSwM::msmFit(lm, k =2, sw =sw)
    
    # maybe verify arbitrary threshold value of 5 
    if (abs(diff(temp_model@Coef[["(Intercept)"]])) < 5 & !is.null(msm_model)) next 
    
    # keep model if differences are big enough to actually trace regime changes
    msm_model <- temp_model    
  }
  
  # extract index of bigger regime (expansion)
  exp_regime_index <- which.max(msm_model@Coef[["(Intercept)"]])
  
  # extract smooth or filtered probabilities
  if (smooth_probs) {
    probs <- msm_model@Fit@smoProb[, exp_regime_index]
    probs <- probs[-1] #drop first line (MSwM adds first prob for some reason)
  } else {
    probs <- msm_model@Fit@filtProb[, exp_regime_index]
  } 
}

# empty dataframe for results
probs_df_two_digits <- data.frame()

# loop through industries and questions and fill result dataframe with probs
# (similar structure as ifo_tbl)
for (industry in two_digit_aggregates) {
  
  ind_subset <- subset(ifo_tbl, ifo_tbl$industry_code == industry)
  ts_date <- ind_subset$date
  ind_res_df <- data.frame(date = ts_date, industry_code = industry)
  
  for (question in question_vars_excl_lus) {
    ts <- ind_subset[[question]]
    probs <- extract_markov_probs(ts, ts_date)
    ind_res_df[[question]] <- probs
  }
  probs_df_two_digits <- rbind(probs_df_two_digits, ind_res_df)
}

view(probs_df_two_digits)

#-------------------------------------------------------------------------------
# If using normal TS for clustering, replace probs_df_two_digits with just subset
# of ifo_tbl
#-------------------------------------------------------------------------------

# .... 

#-------------------------------------------------------------------------------
# Clustering bsed on whole ccf vectors of Markov Probs (correlation at each lag)
#-------------------------------------------------------------------------------

# calcs ccf of given df with 
get_ccf_matrix<- function(tbl, question_vars, max_lag, only_negative_lag = TRUE) {
  # tbl - tibble containing date, industry_code, question1, question2, ... 
  # df containing each pair of industry + question and then small tibble with (date, ts, c00_kld)
    ts_df <- tbl %>%
    left_join(C00_KLD, by = "date") %>% # main kld for each ts now included 
    pivot_longer(cols = all_of(question_vars_excl_lus), names_to = "question", values_to = "value") %>%
    group_by(industry_code, question) %>%
    nest()
  
  # function to return vector of ccf vectors 
    get_ccf_vector <- function(x, y, max_lag = 6, only_negative_lag) {
    ccf_result <- ccf(x, y, plot = FALSE, na.action = na.pass, lag.max = max_lag)
    lags <- ccf_result$lag  # This includes lag values from -max_lag to +max_lag
    acf_vals <- as.numeric(ccf_result$acf)
    
    # if needed, save only past lags 
    if(only_negative_lag){
      keep_idx <- which(lags <= 0)
      lags <- lags[keep_idx]
      acf_vals <- acf_vals[keep_idx]      
    } 
    
    names(acf_vals) <- paste0("ccf_", lags)
    return(acf_vals)
    }
  
   # Compute cross-correlation vector for each ts
  ccf_matrix <- ts_df %>%
    mutate(
      ccf_vec = map(data, ~ get_ccf_vector(.x$value, .x$main_kld, max_lag, only_negative_lag))
    ) %>%
    select(industry_code, question, ccf_vec) %>%
    unnest_wider(ccf_vec) %>%
    mutate(ts_id = paste(industry_code, question, sep = "_")) %>%
    column_to_rownames("ts_id") %>%
    select(-industry_code, -question)
  
}

# df with ts of all two_digits
ts_two_digits_df <- ifo_tbl %>% 
  filter(level == 1) %>% #filter to two-digits
  select(-level)

# use either ccf of ts or of markov probs
ts_ccf_matrix <- get_ccf_matrix(ts_two_digits_df, question_vars, 6, only_negative_lag = FALSE)
probs_ccf_matrix <- get_ccf_matrix(probs_df_two_digits, question_vars_excl_lus, 6) #(still excluding LUS, run through later)

# euclidean clustering
dist_mat <- dist(as.matrix(ccf_matrix), method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

# Cut into clusters and asign in extra column 
k <- 5
ts_clusters <- cutree(hc, k)
ccf_matrix$cluster <- ts_clusters

# Take a look at mean ccf of resulting clusters
ccf_matrix %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(starts_with("ccf_"), ~mean(.x)))

# to take a closer look at a chosen cluster 
# (extracting present industries/questions in cluster)
cluster_df <- ccf_matrix %>% filter(cluster == 4) %>%
  rownames_to_column(var = "id") %>%
  separate(id, into = c("industry_code", "question_code"), sep = "_") %>% 
  select(industry_code, question_code)

cluster_df_names <- df_codes_to_titles(cluster_df, 
                                       question_code_col_name = "question_code",
                                       industry_code_col_name = "industry_code")

# look at codes and industries present in chosen cluster 
cluster_df


#-------------------------------------------------------------------------------
# Aggregating states of chosen cluster 
#-------------------------------------------------------------------------------

# function to aggregate current state of given subset of industries/questions
# returns average state and turning point classification according to 0.33/0.66 
aggregate_tp <- function(df, agg_vals = NULL, question_vals = NULL) {
  # aggrgating states of given industries/questions
  filtered_df <- df
  
  # Conditionally filter by aggregate values if they are provided
  if (!is.null(agg_vals)) {
    filtered_df <- filtered_df %>%
      filter(aggregate %in% agg_vals)
  }
  
  # Conditionally filter by question values if they are provided
  if (!is.null(question_vals)) {
    filtered_df <- filtered_df %>%
      filter(question %in% question_vals)
  }
  
  # normalize to 0-1 
  n_unique <- filtered_df %>%
    distinct(aggregate, question) %>%
    nrow()  
  
  # Group by date, summarize the state, and ungroup
  result <- filtered_df %>%
    group_by(date) %>%
    summarise(total_state = sum(state, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total_state = total_state/n_unique)
  
  mark_turningpoints <- function(probabilities) {
    df <- data.frame(prob = probabilities) %>%
      mutate(
        prev_value = lag(prob, default = first(prob)), # helper column previous values
        phase = case_when(
          prob > 0.66 ~ "expansion",
          prob < 0.33 ~ "contraction",
          TRUE ~ "uncertain"
        ),
        prev_status = case_when(
          prev_value > 0.66 ~ "expansion",
          prev_value < 0.33 ~ "contraction",
          TRUE ~ "uncertain"
        ),
        turning_point = case_when(
          (phase == "expansion" & prev_status != "expansion") ~ TRUE,
          (phase == "contraction" & prev_status != "contraction") ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>%
      select(-prev_value, -prev_status) # clean up intermediate columns
    
    return(df)
  }
  
  df <- mark_turningpoints(result$total_state)
  df$date <- result$date
  df$state <- result$total_state
  
  return(df)
}

# empty df to aggregate states of ts belonging cluster
cluster_states_df <- data.frame(date = total_avrg_regime$date) # change this date column
cluster_states_df$state <- 0

# Loop through industry/question pairs of cluster and aggregate binary states
for (i in 1:nrow(cluster_df)) {
  temp_df <- aggregate_tp(ts_df, 
                          agg_vals = cluster_df$industry_code[i],
                          question_vals = cluster_df$question_code[i])
  cluster_states_df$state <- cluster_states_df$state + temp_df$state
}

cluster_states_df$state <- cluster_states_df$state / nrow(cluster_df)

cluster_tp_df <- mark_turningpoints(cluster_states_df$state)
cluster_tp_df$date <- cluster_states_df$date

cluster_states_df

#-------------------------------------------------------------------------------
# Evaluating turning points of aggregate 
#-------------------------------------------------------------------------------
cluster_exp_tp <- cluster_tp_df %>% filter(turning_point == TRUE & phase == 'expansion') %>% pull(date)
cluster_contr_tp <- cluster_tp_df %>% filter(turning_point == TRUE & phase == 'contraction') %>%   pull(date)


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

# still not decisive enough, need either another decision rule, better clustering 
# or other form of aggregating 

