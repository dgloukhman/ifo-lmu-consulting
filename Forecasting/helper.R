# Load required packages and data
source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))
source(here("Forecasting", "config.R"))

#' Load and preprocess the ifo data
#'
#' @param levels The levels to filter by.
#' @return A preprocessed tibble.
load_and_preprocess_data <- function(levels) {
  read_ifo_data() %>%
    preprocess_ifo_data() %>%
    filter(level %in% levels)
}

#' Get a timeseries by a specific question
#'
#' @param question The question to filter by (e.g., "KLD").
#' @param ifo_tbl The input tibble with ifo data.
#' @return A tsibble with the time series for the given question.
get_ts_by_question <- function(question = "KLD", ifo_tbl) {
  ifo_tbl %>%
    select(date, industry_code, question) %>%
    pivot_wider(names_from = industry_code, values_from = question) %>%
    as_tsibble(index = date)
}

#' Create a dataframe with lagged values
#'
#' This helper function creates a dataframe with lagged values of the time series data for regression analysis.
#'
#' @param data The input dataframe.
#' @param max_lag The maximum lag to create.
#' @param exclude_lag_until The starting point for creating lags.
#' @return A dataframe with lagged values.
create_lagged_df <- function(data, max_lag, exclude_lag_until = 0) {
  lagged_data <- data
  cols <- names(data)

  lags <- (exclude_lag_until + 1):max_lag
  for (lag in lags) {
    lagged_data <- lagged_data %>%
      mutate(across(cols, \(x) dplyr::lag(x, n = lag), .names = "{col}_lag_{lag}"))
  }
  return(lagged_data)
}

#' Performs a Granger causality test for two time series
#'
#' This function performs a hypothesis test for two time series at the significance level.
#'
#' @param x The independent variable (time series).
#' @param y The dependent variable (time series).
#' @param lag The lag to use for the test.
#' @param significance_level The significance level for the test.
#' @param type The type of test to perform ("granger" or other).
#' @return A tibble with the test results.
perform_univariate_granger_test <- function(x, y, lag = 1, significance_level = 0.05) {
  y_lags <- create_lagged_df(tibble(y), lag) %>% dplyr::select(-1)
  x_lags <- create_lagged_df(tibble(x), lag)
  reduced_data <- cbind(y_t = y, y_lags)

  model_data <- cbind(y_t = y, y_lags, x_lags)
  y_only_model <- lm(y_t ~ ., data = na.omit(as.data.frame(reduced_data)))
  full_model <- lm(y_t ~ ., data = na.omit(as.data.frame(model_data)))


  granger_test_vec(y_only_model, full_model, significance_level)
}

granger_test_vec <- function(y_only_model, full_model, significance_level = 0.05) {
  # Test function to perform F-test for two linear model

  a <- anova(y_only_model, full_model)

  tibble(
    causal = a$`Pr(>F)`[2] < significance_level,
    full_model_adj_r2 = summary(full_model)$adj.r.squared,
    y_only_model_adj_r2 = summary(y_only_model)$adj.r.squared,
    diff_adj_r2 = summary(full_model)$adj.r.squared - summary(y_only_model)$adj.r.squared
  )
  # list(y_only_model, full_model, a)
}
# Load industry and question code maps
i_map <- load_industry_code_map()
q_map <- load_question_map()
