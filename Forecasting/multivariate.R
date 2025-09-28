# Load required packages and data
source(here("Forecasting", "helper.R"))

# Load and preprocess ifo data
ifo_tbl <- load_and_preprocess_data(LEVELS)

# Extract the main KLD time series and filter it out from the main table
main_kld <- get_ts_by_question("KLD", ifo_tbl) %>%
  select("C0000000") %>%
  pull("C0000000")
ifo_tbl <- ifo_tbl %>% filter(industry_code != "C0000000")

# Get unique industry codes and questions
industry_codes <- unique(ifo_tbl$industry_code)
questions <- setdiff(names(ifo_tbl), c("date", "industry_code", "level"))

#' Perform multivariate Granger causality analysis
#'
#' This function applies a predictive test to all time series.
#'
#' @param ifo_tbl The input tibble with ifo data.
#' @return A tibble with the results of the Granger causality test.
multivariate_granger_main <- function(ifo_tbl, forecast_type = "simple") {
  # main logic to apply the predictive test function an all time series
  y <- main_kld

  results <- purrr::map_dfr(industry_codes, function(q) {
    data <- ifo_tbl %>% filter(industry_code == q)
    data <- tibble(data)

    vars <- expand_grid(industry_code = q, lag = 1:MAX_LAG)


    purrr::pmap_dfr(vars, function(industry_code, lag, .progress = TRUE) {
      y_lags <- create_lagged_df(tibble(y), lag) %>% dplyr::select(-1)
      reduced_data <- cbind(y_t = y, y_lags)

      if (forecast_type == "instantaneous") {
        x_lags <- create_lagged_df(select(data, all_of(questions)), lag)
      } else {
        x_lags <- create_lagged_df(select(data, all_of(questions)), lag) %>% select(-all_of(questions))
      }
      full_data <- cbind(y_t = y, y_lags, x_lags)

      y_only_model <- lm(y_t ~ ., data = reduced_data)
      full_model <- lm(y_t ~ ., data = full_data)

      res <- granger_test_vec(y_only_model, full_model, significance_level = SIGNIFICANCE_LEVEL)

      tibble(
        industry_code = industry_code,
        lag = lag,
        causal = res$causal,
        full_model_adj_r2 = res$full_model_adj_r2,
        y_only_model_adj_r2 = res$y_only_model_adj_r2,
        diff_adj_r2 = res$diff_adj_r2
      )
    })
  })


  return(results)
}

# Run the multivariate Granger causality analysis
multivariate_granger_results <- ifo_tbl %>% multivariate_granger_main()

# Filter and process the results
multivariate_granger_results <- multivariate_granger_results %>%
  group_by(industry_code) %>%
  filter(causal == TRUE & full_model_adj_r2 == max(full_model_adj_r2)) %>%
  ungroup() %>%
  mutate(industry = i_map[industry_code])
