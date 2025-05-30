library(tidyverse)

plot_industry_timeseries <- function(df, industry_codes, question_code, industry_dict = NULL, questions_dict = NULL) {
  # Plot time series for multiple industries for one question
  # Changing legend and main to actual title, if industry_dict or questions_dict provided
  
  # Filter to selected industries
  df_filtered <- df[df$industry_code %in% industry_codes, ]
  
  # Replace industry codes with titles if df is provided
  if(!is.null(industry_dict)) {
    df_filtered <- df_filtered %>%
      left_join(industry_df, by = "industry_code") %>% #insert industry_titles 
      select(-industry_code) %>% #remove old code column
      select(date, industry_title, everything()) %>% #reorder titles to the beginning
      rename(industry = industry_title) #so that we only have to put industry into plot command
  } else {
    df_filtered <- df_filtered %>% # so that we only have to put industry into plot command
      rename(industry = industry_code)
  }
  
  if(!is.null(questions_dict)){
    question <- questions_dict$question_title[questions_dict$question_code == question_code]
  } else {
    question <- question_code
  }
  # Create the plot
  ggplot(df_filtered, aes(x = date, y = .data[[question_code]], color = industry)) +
    geom_line() +
    labs(title = paste(question),
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",  # Moves the legend to the bottom
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0.5)) 
}

plot_lagged_corr_heatmap <- function(cor_matrix, wrap_width = 30, lag) {
  
  # pivot for easier plotting (can do in one line with reshape2 package, but avoiding too many packages for now)
  cor_df <- as.data.frame(cor_matrix)
  cor_long <- cor_df %>%
    mutate(Lagged_Industry = rownames(cor_matrix)) %>%
    pivot_longer(
      -Lagged_Industry,
      names_to = "Current_Industry",
      values_to = "Correlation"
    )
  
  # Wrap long titles to improve readability (splitting it up into two rows )
  cor_long$Lagged_Industry <- stringr::str_wrap(cor_long$Lagged_Industry, width = wrap_width)
  cor_long$Current_Industry <- stringr::str_wrap(cor_long$Current_Industry, width = wrap_width)
  
  # Plot heatmap
  ggplot(cor_long, aes(x = Current_Industry, y = Lagged_Industry, fill = Correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                         limits = c(-1, 1), name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(hjust = 0.5)) +
    labs(
      title = paste0("Correlations lag = ",lag),
      x = NULL,
      y = "Lagged Industries"
    )
}
