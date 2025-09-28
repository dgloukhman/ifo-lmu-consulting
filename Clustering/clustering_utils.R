# ------------------------------------------------------------------------------
# Function: get_ccf_matrix
# Purpose:
#   Computes cross-correlation vectors between each industry-question time series
#   and the main KLD index, up to a given lag. Returns a matrix where each row is
#   an (industry, question) pair and columns are lagged CCF values.
# Arguments:
#   - tbl               : tibble containing (date, industry_code, question1,...)
#   - c00_kld           : tibble with (date, main_kld) reference series
#   - question_vars     : vector of column names of question variables
#   - max_lag           : maximum lag to compute for ccf()
#   - only_negative_lag : if TRUE, keeps only lags <= 0 (past information)
# Returns:
#   - tibble/matrix of CCF vectors with one row per (industry, question) pair
# ------------------------------------------------------------------------------
get_ccf_matrix<- function(tbl, c00_kld ,question_vars, max_lag, only_negative_lag = TRUE) {
  
  ts_df <- tbl %>%
    left_join(c00_kld, by = "date") %>% # main kld for each ts now included 
    pivot_longer(cols = all_of(question_vars), names_to = "question_code", values_to = "value") %>%
    group_by(industry_code, question_code) %>%
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
    select(industry_code, question_code, ccf_vec) %>%
    unnest_wider(ccf_vec) %>%
    mutate(ts_id = paste(industry_code, question_code, sep = "_")) %>%
    column_to_rownames("ts_id") %>%
    select(-industry_code, -question_code)
}


# ------------------------------------------------------------------------------
# Function: make_lags
# Purpose:
#   Adds lagged versions of specified columns to a dataframe, for all lags in range.
# Arguments:
#   - df   : dataframe with time series data
#   - cols : vector of column names to lag
#   - lags : vector of lags to generate
# Returns:
#   - dataframe with new lagged columns appended
# ------------------------------------------------------------------------------
make_lags <- function(df, cols, lags) {
  min_lag <- min(lags)
  max_lag <- max(lags)
  df <- df %>% arrange(.data[["date"]])
  for (col in cols) {
    for (l in min_lag:max_lag) {
      new_name <- paste0(col, "_lag", l)
      df[[new_name]] <- dplyr::lag(df[[col]], n = l)
    }
  }
  df
}


# ------------------------------------------------------------------------------
# Function: compare_augmented_to_baseline
# Purpose:
#   Fits baseline and augmented linear models to test whether cluster averages
#   add predictive power for the main KLD series beyond its own lags.
# Arguments:
#   - cluster_name : name of cluster column (e.g., "cluster_1")
#   - lags         : vector of lags to include
#   - data         : dataframe containing main_kld, cluster averages, and their lags
#   - baseline     : if TRUE, fit only baseline model using main_kld lags
# Returns:
#   - tibble with mse, rmse, adj_r2 evaluation metrics
# ------------------------------------------------------------------------------
compare_augmented_to_baseline <-  function(cluster_name = "cluster_1", lags, data,baseline = FALSE) {
  df <- data
  
  # extract lag values and assign to names
  min_lag <- min(lags)
  max_lag <- max(lags)
  main_lag_names <- paste0("main_kld", "_lag", min_lag:max_lag)
  cl_lag_names <- paste0(cluster_name, "_lag", min_lag:max_lag)
  
  # build formula for lm with these names
  baseline_formula <- as.formula(paste0("main_kld", " ~ ", paste(main_lag_names, collapse = " + ")))
  aug_formula <- as.formula(paste0("main_kld", " ~ ", paste(c(main_lag_names, cl_lag_names), collapse = " + ")))
  
  if(baseline == TRUE) {
    
    fit <- lm(baseline_formula, data=df)
  } else {

     fit <- lm(aug_formula, data = df)
  }
  
  # return evaluation metrics 
  mse <- mean(abs(fit$fitted.values - df$main_kld)^2)
  rmse <- sqrt(mean(abs(fit$fitted.values - df$main_kld)^2))
  adj_r2 <- summary(fit)$adj.r.squared
  return(tibble(mse = mse, rmse = rmse, adj_r2 = adj_r2))
}

# ------------------------------------------------------------------------------
# Function: get_cluster_avrg
# Purpose:
#   Computes average time series of each cluster over time, by taking the mean
#   across all members’ values within a cluster.
# Arguments:
#   - ts_long_df    : long-format tibble with (date, industry_code, question_code, value)
#   - ts_ccf_matrix : tibble with CCF vectors and cluster assignments
# Returns:
#   - wide-format tibble with one column per cluster (cluster_1, cluster_2, ...)
# ------------------------------------------------------------------------------
get_cluster_avrg <- function(ts_long_df, ts_ccf_matrix) {
  # mark clusters in long_df 
  ts_long_df_with_cluster <- ts_long_df %>%
    inner_join(
      ts_ccf_matrix %>% 
        rownames_to_column("id") %>%
        separate(id, into = c("industry_code", "question_code"), sep = "_") %>%
        select(industry_code, question_code, cluster),
      by = c("industry_code", "question_code")
    )
  
  # compute average value of each cluster
  cluster_avrgs <- ts_long_df_with_cluster %>%
    group_by(date, cluster) %>%
    summarise(cluster_value = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = cluster, values_from = cluster_value,
                names_prefix = "cluster_")
  
}


# ------------------------------------------------------------------------------
# Function: df_codes_to_titles
# Purpose:
#   Converts industry and question codes in a dataframe to their full titles,
#   using question_df and industries_df dictionaries.
# Arguments:
#   - df                     : dataframe to convert
#   - data_path              : path to csv dictionaries (unused here, could extend)
#   - question_code_col_name : column name with question codes
#   - industry_code_col_name : column name with industry codes
# Returns:
#   - dataframe with codes replaced by human-readable titles
# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# Function: plot_ccfs_cluster
# Purpose:
#   Plots average cross-correlation functions (CCFs) of each cluster relative
#   to the main KLD index, across all cluster members.
# Arguments:
#   - ts_ccf_matrix : tibble with rows of time series and cluster assignments
#   - legend        : whether to display legend (default = TRUE)
# Returns:
#   - ggplot object with mean CCF profiles per cluster
# ------------------------------------------------------------------------------
plot_ccfs_cluster <- function(ts_ccf_matrix, show_legend = TRUE) {
  # Calculate mean CCF per cluster (you already did this)
  mean_ccf_profiles <- ts_ccf_matrix %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      across(starts_with("ccf_"), ~mean(.x, na.rm = TRUE))
    )
  
  # Pivot data into a long format for plotting
  mean_ccf_long <- mean_ccf_profiles %>%
    pivot_longer(
      cols = starts_with("ccf_"),
      names_to = "lag",
      values_to = "mean_ccf",
      names_prefix = "ccf_"
    ) %>%
    mutate(lag = as.integer(lag))
  
  # Plot the profiles
  ggplot(mean_ccf_long, aes(x = lag, y = mean_ccf, group = cluster, color = factor(cluster))) +
    geom_line(alpha = 0.7,show.legend = show_legend) +
    geom_point(show.legend = show_legend) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = "Mean CCF Vectors of Each Cluster to Main Index",
      x = "Lag",
      y = "Mean CCF",
      color = "Cluster"
    ) +
    theme_minimal()
}


# ------------------------------------------------------------------------------
# Function: plot_cluster_with_main
# Purpose:
#   Plots time series of all members of a selected cluster together with the main
#   KLD index for visual comparison. Picks out time series from ifo_tbl according to
#   industry/question pairs in cluster_df
# Arguments:
#   - ifo_tbl    : tibble with (date, industry_code, question_code, value)
#   - cluster_df : tibble containing strings of components (industry_code, question_code)
# Returns:
#   - ggplot object of cluster series vs main series
# ------------------------------------------------------------------------------
plot_cluster_with_main <- function(ifo_tbl, cluster_df, title = "Cluster Components vs Main Index") {
  
  # Pivot to long format for easier filtering
  ifo_long <- ifo_tbl %>%
    pivot_longer(
      cols = -c(date, industry_code, level),
      names_to = "question_code",
      values_to = "value"
    )
  
  # Filter for cluster series
  cluster_series <- ifo_long %>%
    inner_join(cluster_df, by = c("industry_code", "question_code"))
  
  # Add the reference series (KLD, C0000000)
  ref_series <- ifo_long %>%
    filter(industry_code == "C0000000", question_code == "KLD")
  
  # Plot
  ggplot() +
    geom_line(data = cluster_series,
              aes(x = date, y = value, group = interaction(industry_code, question_code)),
              color = "grey70", alpha = 0.8) +
    geom_line(data = ref_series,
              aes(x = date, y = value),
              color = "blue") +
    labs(x = NULL, y = "",
         title = title) +
    ylim(c(-100,100)) +
    theme_minimal()
}

# ------------------------------------------------------------------------------
# Function: plot_cluster_avrg_with_main
# Purpose: 
#  Plot average of cluster with main series
# Arguments:
# - kld_cluster_df - df containing columns date, main_kld, cluster_index
# Returns:
# - ggplot 

plot_cluster_avrg_with_main <- function(c00_kld, cluster_index, 
                                        title = "Cluster Average vs Main Index", 
                                        show_legend = TRUE) {
  kld_cluster_df <- cbind(c00_kld, cluster_index)
  
  p <- ggplot(kld_cluster_df, aes(x = date)) +
    geom_line(aes(y = main_kld, color = "Main KLD")) +
    geom_line(aes(y = cluster_index, color = "Cluster Index")) +
    scale_color_manual(values = c("Main KLD" = "blue", 
                                  "Cluster Index" = "grey20")) +
    labs(title = title,
         x = "Date", 
         y = "Value", 
         color = "Series") +
    ylim(c(-100,100)) +
    theme_minimal()
  
  # control legend
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}
