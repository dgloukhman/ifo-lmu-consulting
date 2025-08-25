source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))
source(here("Forecasting", "helper.R"))



SIGNIFICANCE_LEVEL <- 0.05
LEVELS <- c(0, 3)


ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  filter(level %in% LEVELS)

main_kld <- get_ts_by_question("KLD", ifo_tbl) %>% select("C0000000") %>% pull("C0000000")
ifo_tbl <- ifo_tbl %>% filter(industry_code != "C0000000")

industry_codes <- unique(ifo_tbl$industry_code)
questions <- setdiff(names(ifo_tbl), c("date", "industry_code", "level"))

multivariate_granger_main <- function(ifo_tbl) {
  # main logic to apply the predictive test function an all time series
  y <- main_kld

  results <- purrr::map_dfr(industry_codes, function(q) {
    data <- ifo_tbl %>% filter(industry_code == q)
    data <- tibble(data)

    vars <- expand_grid(industry_code = q, lag = 1:6)


    purrr::pmap_dfr(vars, function(industry_code, lag, .progress = TRUE) {

        y_lags <- create_lagged_df(tibble(y), lag) %>% dplyr::select(-1)
        reduced_data <- cbind(y_t = y, y_lags)

        x_lags <- create_lagged_df(select(data, all_of(questions)), lag)
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

tmp <- ifo_tbl %>% multivariate_granger_main() 

tmp <- tmp %>%
    group_by(industry_code) %>%
  filter(causal == TRUE & full_model_adj_r2 == max(full_model_adj_r2)) %>%
  ungroup() %>%
  mutate( industry = i_map[industry_code])

