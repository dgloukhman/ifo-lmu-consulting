library(here)
source(here("Forecasting", "helper.R"))

# Load and preprocess ifo data
ifo_tbl <- load_and_preprocess_data(UNIVARIATE_LEVELS)

# Extract the main KLD time series and filter it out from the main table
main_kld <- get_ts_by_question(ifo_tbl, "KLD") %>% pull("C0000000")
ifo_tbl <- ifo_tbl %>% filter(industry_code != "C0000000")

# Get the questions to be used in the analysis
questions <- setdiff(names(ifo_tbl), c("date", "industry_code", "level"))

#' Main function to perform Granger causality analysis
#'
#' This function applies the predictive test to all time series.
#'
#' @param forecast_type The type of forecast to perform.
#' @return A tibble with the results of the Granger causality test.
granger_main <- function(forecast_type = "simple", main_kld = main_kld, ifo_tbl = ifo_tbl, questions = questions) {
  # main logic to apply the predictive test function an all time series
  y <- main_kld

  results <- purrr::map_dfr(questions, function(q) {
    data <- get_ts_by_question(ifo_tbl, q)
    vars <- expand_grid(industry_code = names(data), lag = 1:MAX_LAG)

    purrr::pmap_dfr(vars, function(industry_code, lag, .progress = TRUE) {
      res <- perform_granger_test(select(tibble(data), all_of(industry_code)), y, lag = lag, significance_level = SIGNIFICANCE_LEVEL, forecast_type = forecast_type)
      tibble(question = q, industry_code = industry_code, causal = res$causal, lag = lag, full_model_adj_r2 = res$full_model_adj_r2, y_only_model_adj_r2 = res$y_only_model_adj_r2, diff_adj_r2 = res$diff_adj_r2)
    })
  })


  return(results)
}

