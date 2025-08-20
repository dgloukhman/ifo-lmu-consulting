# Script which first runs MSM to get turning points, then check out some methods to see if we can predict 
# C00 KLD BB turning points with Markov Turning Points of two-digits. Evaluated as a kind of classification task,
# so checking if main turning point following in certain time window after subseries turning point. 
#
# Not really significant results regarding single time series or groups like a certain industry or question, 
# thus trying out some clustering first

#-------------------------------------------------------------------------------
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
# define function which returns df with turning points for given time series
# (based on Markov Switching and 1/3, 2/3 rule)
ms_turning_points <- function(ts, dates, smooth_probs = FALSE) {
  
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
  if(smooth_probs){
    probs <- msm_model@Fit@smoProb[,exp_regime_index]
    probs <- probs[-1] #drop first line (MSwM adds first prob for some reason)
  } else {
    probs <-  msm_model@Fit@filtProb[,exp_regime_index]
  }
  
  # define function to mark turning points and merge into df (below 1/3 -> contraction, above 2/3 -> Expansion, rest is uncertain)
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
      select(-prev_value, -prev_status)  # clean up intermediate columns
    
    return(df)
  }
  
  tp_df <- mark_turningpoints(probs)
  tp_df$ts <- ts
  tp_df$date <- dates
  
  return(tp_df)
}

#-------------------------------------------------------------------------------
# Turning Points for Main Series 
#-------------------------------------------------------------------------------
C00_subset <- subset(ifo_tbl, ifo_tbl$industry_code == 'C0000000')

markov_tp_main <- ms_turning_points(ts = C00_subset$KLD, 
                                    dates = C00_subset$date, 
                                    smooth_probs = TRUE) #smooth to use all data

# Extract expansion and contraction dates
exp_main_markov <-  markov_tp_main %>% 
  filter(turning_point == TRUE, phase == "expansion") %>% pull(date)

contr_main_markov <-  markov_tp_main %>% 
  filter(turning_point == TRUE, phase == "contraction") %>% pull(date)

# Little stationarity test for whole ts and the regimes 
subset_upper_regime <- markov_tp_main %>% filter(phase == 'expansion')
subset_lower_regime <- markov_tp_main %>% filter(phase == 'contraction')

adf.test(subset_lower_regime$ts, alternative = "stationary")
adf.test(subset_upper_regime$ts, alternative = "stationary")
adf.test(markov_tp_main$ts, alternative = "stationary")
# --> All are stationary

#-------------------------------------------------------------------------------
# Define functions to get tp of subseries and evaluate against C00 KLD
#-------------------------------------------------------------------------------
# function to iterate through industries and questions and apply markov switching function, 
# extrating expansion and contracting turning points, with filtered probabilities (realtime data)
extract_markov_tp <- function(df, industry_codes, question_codes) {
  #iterate trough industry codes
  map_dfr(industry_codes, function(agr) {
    df_subset <- subset(df, industry_code == agr)
    
    # iterate through all questions of industry subset
    map_dfr(question_codes, function(q_var) {
      ts <- df_subset[[q_var]] 
      dates <- df_subset$date
      
      # run markov switching turning point function
      tp_df <- ms_turning_points(ts = ts, dates = dates, smooth_probs = FALSE)
      
      # extract turning points and store each into list
      tibble(
        aggregate = agr,
        question = q_var,
        expansion_tps = list(tp_df %>% filter(turning_point, phase == "expansion") %>% pull(date)),
        contraction_tps = list(tp_df %>% filter(turning_point, phase == "contraction") %>% pull(date))
      )
    })
  })
}

# define function to evaluate possible indicating turning points against main turning points
evaluate_indicator <- function(main_dates, indicator_dates, valid_lead_time = 6) {
  hits <- 0
  lead_times <- c()
  
  for (ind_date in indicator_dates) {
    ind_date <- as.Date(ind_date)
    diffs <- (year(ind_date) - year(main_dates)) * 12 + (month(ind_date) - month(main_dates))
    valid_diffs <- diffs[diffs <= 0 & diffs >= -valid_lead_time]
    
    if (length(valid_diffs) > 0) {
      hits <- hits + 1
      lead_times <- c(lead_times, min(valid_diffs) * (-1))
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

# function to evaluate tps of all given industries and questions against certain main turning points
# If aggregate: Summarized across expansion and contraction, otherwise looking at both individually
evaluate_tp_df <- function(tp_df, main_tp_exp, main_tp_contr, valid_lead_time = 6, aggregate = FALSE) {
  # Evaluate expansions tps 
  expansion_eval <- tp_df %>%
    rowwise() %>%
    mutate(
      eval = list(evaluate_indicator(main_tp_exp, expansion_tps, valid_lead_time)),
      phase = "expansion"
    ) %>%
    unnest(eval)
  
  # Evaluate contractions tps 
  contraction_eval <- tp_df %>%
    rowwise() %>%
    mutate(
      eval = list(evaluate_indicator(main_tp_contr, contraction_tps, valid_lead_time)),
      phase = "contraction"
    ) %>%
    unnest(eval)
  
  # Combine
  combined <- bind_rows(expansion_eval, contraction_eval) %>%
    select(aggregate, question, phase, everything())
  
  # if aggregate, then get total scores per time series, not looking at contraction/expansion
  if (aggregate) {
    combined <- combined %>%
      group_by(aggregate, question, expansion_tps, contraction_tps) %>%
      summarise(
        hits = sum(hits),
        total_pred = sum(total_pred),
        total_actual = sum(total_actual),
        precision = mean(precision, na.rm = TRUE),
        recall = mean(recall, na.rm = TRUE),
        f1_score = mean(f1_score, na.rm = TRUE),
        lead_time_avg = mean(lead_time_avg, na.rm = TRUE),
        lead_time_sd = mean(lead_time_sd, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  return(combined)
}

#-------------------------------------------------------------------------------
# Apply to two_digit aggregates (warning, this part takes a while)
#-------------------------------------------------------------------------------

two_digit_aggregates <- c("C1000000", "C1100000", "C1300000", "C1400000", "C1500000", "C1600000", "C1700000", 
                          "C1800000", "C1900000", "C2000000", "C2200000", "C2300000", "C2400000", "C2500000", 
                          "C2600000", "C2700000", "C2800000", "C2900000", "C3000000", "C3100000", "C3200000")


# Use all questions starting in 1991
question_vars <- question_df$question_code[1:13]


# extract turning points 
tp_df <- extract_markov_tp(ifo_tbl, 
                           industry_codes = two_digit_aggregates, 
                           question_codes = question_vars)

# Function to replace industry & question codes (provide data_path to get the according dfs)
# (maybe add this to data_loader later on)
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


#-------------------------------------------------------------------------------
# Bry-Boschan Main comparison
#-------------------------------------------------------------------------------
bb_points <- get_bb_turning_points(C00_subset$KLD, C00_subset$date)
exp_main_bb <- bb_points$tp_expansion
contr_main_bb <- bb_points$tp_contraction

# (switched contr and exp bb, to see if because of ionterpretational change)
eval_df <- evaluate_tp_df(tp_df,
                          contr_main_bb,
                          exp_main_bb,
                          valid_lead_time = 12)

eval_df <- df_codes_to_titles(eval_df, data_path, "question", "aggregate")

f1_by_aggregate <- eval_df %>%
  group_by(industry_title) %>%
  summarise(mean_f1 = mean(f1_score, na.rm = TRUE)) %>%
  arrange(desc(mean_f1))

ggplot(f1_by_aggregate, aes(x = reorder(industry_title, -mean_f1), y = mean_f1)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  labs(title = "Mean F1 by Industry Aggregate",
       x = "",
       y = "F1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by question and look at F1 score
f1_by_question <- eval_df %>%
  group_by(question_title) %>%
  summarise(mean_f1 = mean(f1_score, na.rm = TRUE)) %>%
  arrange(desc(mean_f1))

ggplot(f1_by_question, aes(x = reorder(question_title, -mean_f1), y = mean_f1)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  labs(title = "Mean F1 Score by Question",
       x = "",
       y = "F1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
# Scatterplot F1 score against lead time 

# Plot all three main questions 
p1 <- plot_f1_leadtime_scatter(eval_df, question = "Geschäftsklima")
p2 <- plot_f1_leadtime_scatter(eval_df, question = "Produktionspläne")
p3 <- plot_f1_leadtime_scatter(eval_df, question = "Produktion gegen Vormonat")

# Stack vertically
p1 / p2 / p3 + plot_layout(guides = "collect")

p1 <- plot_f1_leadtime_scatter(eval_df, industry = "Metallerzeugung und -bearbeitung")
p2 <- plot_f1_leadtime_scatter(eval_df, industry = "Herstellung von Textilien")
p3 <- plot_f1_leadtime_scatter(eval_df, industry = "Herstellung von Gummi- und Kunststoffwaren")

# Stack vertically
p1 / p2 / p3 + plot_layout(guides = "collect")


#-------------------------------------------------------------------------------
# Exemplary plot of single best time series by F1 score
exp_tp_best <- get_tp_eval_df(eval_df,
                              industry_title = "Herstellung von Textilien",
                              question_title = "Produktion gegen Vormonat",
                              phase = "expansion")

contr_tp_best <- get_tp_eval_df(eval_df,
                                industry_title = "Herstellung von Textilien",
                                question_title = "Produktion gegen Vormonat",
                                phase = "contraction")


plot_tp_along_C00KLD(C00_subset, 
                     exp_main = exp_main_bb,
                     contr_main = contr_main_bb,
                     exp_sub = exp_tp_best,
                     contr_sub = contr_tp_best)

#-------------------------------------------------------------------------------
# Now collect turning points of top 10 time series and plot with C00 KLD 
top10 <- eval_df %>%
  arrange(desc(f1_score)) %>%
  head(10)

top10_tp_exp <- top10 %>%
  pull(expansion_tps) %>%
  unlist() %>%
  as.Date()

top10_tp_contr <- top10 %>%
  pull(contraction_tps) %>%
  unlist() %>%
  as.Date()

plot_tp_along_C00KLD(C00_subset, 
                     exp_main = exp_main_bb,
                     contr_main = contr_main_bb,
                     exp_sub = top10_tp_exp,
                     contr_sub = top10_tp_contr)




#-------------------------------------------------------------------------------
# MARKOV TP MAIN: evaluate extracted turning points with markov TP of main
#-------------------------------------------------------------------------------
eval_df <- evaluate_tp_df(tp_df, 
                          exp_main_markov, 
                          contr_main_markov,
                          valid_lead_time = 6, 
                          aggregate = TRUE)
eval_df


# apply to tp_df (what for actually, do we plot this anywhere?)
turning_points <- df_codes_to_titles(tp_df, 
                                     data_path, 
                                     question_code_col_name = "question", 
                                     industry_code_col_name = "aggregate")
# apply to eval_df
eval_df <- df_codes_to_titles(eval_df, 
                              data_path, 
                              question_code_col_name = "question", 
                              industry_code_col_name = "aggregate")

#-------------------------------------------------------------------------------
# function to plot histogram of all turning points in eval df against main turning points 
# (still need to work on this)
plot_hist_tp_main_tp <- function(eval_df, exp_main_dates, contr_main_dates) {
  # want to count tps per month and phase
  monthly_tp_counts <- eval_df %>%
    select(expansion_tps, contraction_tps) %>%
    pivot_longer(cols = c(expansion_tps, contraction_tps), # 
                 names_to = "phase", 
                 values_to = "tp_dates") %>%
    mutate(phase = ifelse(phase == "expansion_tps", "expansion", "contraction")) %>%
    unnest(tp_dates) %>%
    group_by(tp_dates,phase) %>%
    summarise(n = n(), .groups = "drop")
  
  # combine the main turning points into a tibble, to add them to the plot
  main_tp <- tibble(
    dates = c(exp_main_dates, contr_main_dates),
    phase = c(rep("expansion", length(exp_main_dates)),
              rep("contraction", length(contr_main_dates)))
  )
  
  ggplot(monthly_tp_counts, aes(x = tp_dates, y = n, fill = phase)) +
    geom_col(position = "stack", width = 25) +
    geom_segment(data = main_tp,
                 aes(x = dates, xend = dates, y = 0, yend = max(monthly_tp_counts$n), color = phase),
                 linewidth = 1, linetype = 6) +
    scale_color_manual(values = c("contraction" = "red", "expansion" = "#4CBB17")) +
    scale_fill_manual(values = c("contraction" = "darkred", "expansion" = "darkgreen")) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    theme_minimal(base_size = 12) +
    labs(
      title = "Turning points ",
      x = "",
      y = "Number of Turning Points",
      fill = "Subaggregates",
      color = "Manufacturing Climate"
    )
}
plot_hist_tp_main_tp(eval_df,
                     exp_main_dates = exp_main_markov, 
                     contr_main_dates = contr_main_markov)

#-------------------------------------------------------------------------------
# Group by industry and look at F1 scores 
f1_by_aggregate <- eval_df %>%
  group_by(industry_title) %>%
  summarise(mean_f1 = mean(f1_score, na.rm = TRUE)) %>%
  arrange(desc(mean_f1))

ggplot(f1_by_aggregate, aes(x = reorder(industry_title, -mean_f1), y = mean_f1)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  labs(title = "Mean F1 by Industry Aggregate",
       x = "",
       y = "F1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by question and look at F1 score
f1_by_question <- eval_df %>%
  group_by(question_title) %>%
  summarise(mean_f1 = mean(f1_score, na.rm = TRUE)) %>%
  arrange(desc(mean_f1))

ggplot(f1_by_question, aes(x = reorder(question_title, -mean_f1), y = mean_f1)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  labs(title = "Mean F1 Score by Question",
       x = "",
       y = "F1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
# Scatterplot F1 score against lead time 

# function taking either question or industry as subset 
plot_f1_leadtime_scatter <- function(df, question = NULL, industry = NULL) {
  if (!is.null(question)){
    df_plot <- df %>%
      filter(question_title == !!question)
    
    color_ind <- "industry_title"
    scale_label <- "Industry"
    title <- question
  } else {
    df_plot <- df %>%
      filter(industry_title == all_of(industry))
    
    color_ind <- "question_title"
    scale_label <- "Question"
    title <- industry
  }
  
  ggplot(df_plot, aes(x = lead_time_avg, y = f1_score)) +
    geom_point(aes(size = hits, color = .data[[color_ind]]), alpha = 0.7) +
    geom_text(aes(label = round(lead_time_sd, 1)), vjust = -1, size = 2.3, color = "black") +
    scale_size_continuous(name = "Number of Hits (leadtime sd in brackets)", 
                          limits = c(0,20),
                          range = c(0,10)) +
    scale_color_discrete(name = scale_label) +
    ylim(c(0,1)) +
    xlim(c(0,12))+
    labs(
      title = paste0("TP Indication of ",title),
      x = "Average Lead Time of Hits",
      y = "F1 Score"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12)
    )
}

# Plot all three main questions 
p1 <- plot_f1_leadtime_scatter(eval_df, question = "Geschäftslage Erwartungen")
p2 <- plot_f1_leadtime_scatter(eval_df, question = "Nachfrage gegen Vormonat")
p3 <- plot_f1_leadtime_scatter(eval_df, question = "Auftragsbestand gegen Vormonat")

# Stack vertically
p1 / p2 / p3 + plot_layout(guides = "collect")

p1 <- plot_f1_leadtime_scatter(eval_df, industry = "Papiergewerbe")
p2 <- plot_f1_leadtime_scatter(eval_df, industry = "Holz-  Flecht-  Korb- und Korkwarenherstellung (ohne Möbel)")
p3 <- plot_f1_leadtime_scatter(eval_df, industry = "Herstellung von Textilien")

# Stack vertically
p1 / p2 / p3 + plot_layout(guides = "collect")

#-------------------------------------------------------------------------------
# Visual Inspection of single TS or Industries/Questions
#-------------------------------------------------------------------------------
# function to extract turning points from eval_df
get_tp_eval_df <- function(df, industry_title = NULL, question_title = NULL, phase = "expansion") {
  df_filtered <- df
  
  if (!is.null(industry_title)) {
    df_filtered <- df_filtered %>%
      filter(industry_title == !!industry_title)
  }
  
  if (!is.null(question_title)) {
    df_filtered <- df_filtered %>%
      filter(question_title == !!question_title)
  }
  
  tps_col <- if (phase == "expansion") "expansion_tps" else "contraction_tps"
  
  df_filtered %>%
    pull(!!sym(tps_col)) %>%
    unlist() %>%
    as.Date()
}

# function to plot turning points along C00 KLD graph
plot_tp_along_C00KLD <- function(df, exp_main, contr_main, exp_sub, contr_sub) {
  ggplot(df, aes(x = date, y = KLD)) +
    geom_line(color = "black") +  # KLD line
    geom_point(data = filter(df, date %in% exp_sub),
               aes(x = date, y = KLD -0.5), #-0.5 such that they are slightly visible when below main points
               color = "darkgreen", size = 2.5, shape = 15, alpha=0.8) +  
    geom_point(data = filter(df, date %in% contr_sub),
               aes(x = date, y = KLD-0.5),
               color = "darkred", size = 2.5, shape = 15, alpha = 0.8) +  
    geom_point(data = filter(df, date %in% exp_main),
               aes(x = date, y = KLD),
               color = "#228B22", size = 5, shape = 18) +  
    geom_point(data = filter(df, date %in% contr_main),
               aes(x = date, y = KLD),
               color = "red", size = 5, shape = 18) + 
    geom_vline(xintercept = as.numeric(exp_main), color = "#228B22", linetype = "solid", linewidth = 0.5, alpha = 0.5) +
    geom_vline(xintercept = as.numeric(contr_main), color = "red", linetype = "solid", linewidth = 0.5, alpha = 0.5) +
    labs(title = "",
         x = "", y = "C00 KLD") +
    theme_minimal()
}


# Exemplary plot of single best time series by F1 score
exp_tp_papier <- get_tp_eval_df(eval_df,
                            industry_title = "Herstellung von Gummi- und Kunststoffwaren",
                            question_title = "Geschäftslage Erwartungen",
                            phase = "expansion")

contr_tp_papier <- get_tp_eval_df(eval_df,
               industry_title = "Herstellung von Gummi- und Kunststoffwaren",
               question_title = "Geschäftslage Erwartungen",
               phase = "contraction")


plot_tp_along_C00KLD(C00_subset, 
                     exp_main = exp_main_markov,
                     contr_main = contr_main_markov,
                     exp_sub = exp_tp_papier,
                     contr_sub = contr_tp_papier)

# Now collect turning points of top 10 time series
top10 <- eval_df %>%
  arrange(desc(f1_score)) %>%
  head(10)

top10_tp_exp <- top10 %>%
  pull(expansion_tps) %>%
  unlist() %>%
  as.Date()

top10_tp_contr <- top10 %>%
  pull(contraction_tps) %>%
  unlist() %>%
  as.Date()

plot_tp_along_C00KLD(C00_subset, 
                     exp_main = exp_main_markov,
                     contr_main = contr_main_markov,
                     exp_sub = top10_tp_exp,
                     contr_sub = top10_tp_contr)
