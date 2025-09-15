source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))


# Which ts predicts the main index the best
SIGNIFICANCE_LEVEL <- 0.05
LEVELS <- c(0, 3)

i_map <- load_industry_code_map()
q_map <- load_question_map()

ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  filter(level %in% LEVELS)

get_ts_by_question <- function(question = "KLD", ifo_tbl) {
  ifo_tbl %>%
    select(date, industry_code, question) %>%
    pivot_wider(names_from = industry_code, values_from = question) %>%
    as_tsibble(index = date)
}

main_kld <- get_ts_by_question("KLD", ifo_tbl) %>% select("C0000000")

questions <- setdiff(names(ifo_tbl), c("date", "industry_code", "level"))

create_lagged_df <- function(data, lags) {
  #helper function to create a dataframe with lagged values of the timeseries data for regression analysis
  lagged_data <- data
  for (lag in lags) {
    lagged_data <- lagged_data %>%
      mutate(across(1, \(x) dplyr::lag(x, n = lag), .names = "{col}_lag_{lag}"))
  }
  return(lagged_data)
}

predictive_test_vec <- function(x, y, lag = 1, significance_level = 0.05, type = "granger") {
  # Test function to perform Granger causality test for two time series

  # Create a data frame with the dependent variable and lagged predictors
  # The model is y_t ~ y_{t-1} + ... + y_{t-lag} + x_{t} + ... + x_{t-lag}

  y_lags <- create_lagged_df(tibble(y), 1:lag) %>% dplyr::select(-1)
  x_lags <- create_lagged_df(tibble(x), 1:lag)
  reduced_data <- cbind(y_t = y, y_lags)

  if (type == "granger") {
    model_data <- cbind(y_t = y, y_lags, x_lags)
    y_only_model <- lm(y_t ~ ., data = na.omit(as.data.frame(reduced_data)))
    full_model <- lm(y_t ~ ., data = na.omit(as.data.frame(model_data)))
  } else {
    # Fit the model on non-missing data
    model_data <- cbind(y_t = y, x_lags)
    y_only_model <- lm(y_t ~ 1, data = na.omit(as.data.frame(reduced_data)))
    full_model <- lm(y_t ~ ., data = na.omit(as.data.frame(model_data)))
  }

  a <- anova(y_only_model, full_model)

  tibble(
    weight = 0,
    causal = a$`Pr(>F)`[2] < significance_level,
    adj_r2 = summary(full_model)$adj.r.squared,
    diff_adj_r2 = summary(full_model)$adj.r.squared - summary(y_only_model)$adj.r.squared
  )
  # list(y_only_model, full_model, a)
}

granger_main <- function() {
  #main logic to apply the predictive test function an all time series
  y <- main_kld[["C0000000"]]

  results <- purrr::map_dfr(questions, function(q) {
    data <- get_ts_by_question(q, ifo_tbl) %>% select(!"C0000000")
    vars <- expand_grid(industry_code = names(data), lag = 1:5)

    purrr::pmap_dfr(vars, function(industry_code, lag, .progress = TRUE) {
      res <- predictive_test_vec(data[[industry_code]], y, lag = lag, significance_level = SIGNIFICANCE_LEVEL, type = "forecast")
      tibble(question = q, industry_code = industry_code,  causal = res$causal, lag = lag, adj_r2 = res$adj_r2, diff_adj_r2 = res$diff_adj_r2)
    })
  })


  return(results)
}
# plot tible

plot_weight <- function(tsbl, title = paste0("Industries granger causing the main index")) {
  ggplot(tsbl, aes(x = industry_code, y = weight, fill = causal)) +
    geom_bar(stat = "identity", ) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
    labs(x = "Industry Code", fill = "Causal", title = title) + # Removed y-axis label
    theme_minimal() +
    theme(
      axis.text.y = element_blank(), # Removes y-axis text
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, margin = margin(t = 10))
    )
}
granger_tbl <- granger_main()

tmp <- granger_tbl %>%
  group_by(industry_code) %>% 
  filter(adj_r2 == max(adj_r2)) %>%
  ungroup() %>%
  mutate(question = q_map[question],code= industry_code , industry_code = i_map[industry_code])

# granger_tbl %>% plot_weight()
