# script for basic univariate and multivariate summary functions
library(tidyverse)
library("readxl")
library("purrr")
library(ggplot2)


 univariate_summaries <- function(df, industry_codes, question_codes, industry_dict = NULL, questions_dict = NULL) {
  # Function to get summaries about singles time series, can give multiple industry codes and questions
  
  # Filter to selected industry codes
  df_filtered <- df[df$industry_code %in% industry_codes, ]
  result_list <- list() #empty list for results
  
  # iterate through all given questions and then create df for summary statistics of all industries 
  for (q in question_codes) {
    for (ind in industry_codes) {
      subset_vals <- df_filtered[df_filtered$industry_code == ind, ][[q]]
      
      result_list[[length(result_list) + 1]] <- data.frame(
        question_code = q,
        industry_code = ind,
        mean = mean(subset_vals, na.rm = TRUE),
        sd = sd(subset_vals, na.rm = TRUE),
        n = sum(!is.na(subset_vals))
      )
    }
  }
  # combine them all into one df 
  res <- do.call(rbind, result_list)
  
  if(!is.null(industry_dict)) {
    res <- res %>%
      left_join(industry_df, by = "industry_code") %>% #insert industry_titles 
      select(-industry_code) %>% #remove old code column
      select(question_code, industry_title, everything()) #reorder titles to the begining 
  }
  
  if(!is.null(questions_dict)) {
    res <- res %>%
      left_join(questions_dict, by = "question_code") %>% #insert industry_titles 
      select(-question_code) %>% #remove old code column
      select(question_title, everything()) #reorder titles to the begining 
  }
  return(res)
}

lagged_corr_matrix <- function(df, industry_codes, question_code, lag = 1, industry_dict = NULL) {
  # returns lagged correlation matrix, "How is lagged version of industry X (rows) correlated with unlagged industry Y (columns)"
  # add industry_dict if desiring industry titles instead of instry codes 
  
  # subset to given industry_codes
  df_subset <- df[df$industry_code %in% industry_codes, ]
  
  # replace codes with titles, if given 
  if (!is.null(industry_dict)) {
    df_subset <- df_subset %>%
      left_join(industry_dict, by = "industry_code") %>% #insert industry_titles 
      select(-industry_code) %>% #remove old code column
      select(date, industry_title, everything()) %>%
      rename(industry = industry_title)#reorder titles to the begining
  } else {
    df_subset <- df_subset %>%
      rename(industry = industry_code)
  }
  
  # pivot to have all industries next to each other 
  df_wide <- df_subset %>%
    select(date, industry, all_of(question_code)) %>%
    pivot_wider(names_from = industry, values_from = all_of(question_code)) 
  
  # lag the whole thing 
  X <- df_wide[,-1] #remove date column
  X_lagged <- X[1:(nrow(X) - lag),] # remove last lag rows
  Y <- X[(lag + 1):nrow(X),] # remove first lag rows
  
  # Initialize matrix
  n_vars <- ncol(X_lagged)
  cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  
  # Loop over all pairs of variables
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      cor_matrix[i, j] <- cor(X_lagged[, i], Y[, j], use = "complete.obs") # check complete.obs
    }
  }
  
  # Name rows and columns
  rownames(cor_matrix) <- colnames(X_lagged)
  colnames(cor_matrix) <- colnames(Y)
  
  return(cor_matrix)
}