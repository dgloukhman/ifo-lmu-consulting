source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))


get_ts_by_question <- function(question = "KLD", ifo_tbl) {
    ifo_tbl %>%
        select(date, industry_code, question) %>%
        pivot_wider(names_from = industry_code, values_from = question) %>%
        as_tsibble(index = date)
}

create_lagged_df <- function(data, max_lag, exclude_lag_until = 0) {
    # helper function to create a dataframe with lagged values of the timeseries data for regression analysis
    lagged_data <- data
    cols <- names(data)

    lags <- (exclude_lag_until + 1):max_lag
    for (lag in lags) {
        lagged_data <- lagged_data %>%
            mutate(across(cols, \(x) dplyr::lag(x, n = lag), .names = "{col}_lag_{lag}"))
    }
    return(lagged_data)
}

granger_test_vec <- function(y_only_model, full_model, significance_level = 0.05) {
    # Test function to perform Granger causality test for two linear model

    a <- anova(y_only_model, full_model)
    
    tibble(
        causal = a$`Pr(>F)`[2] < significance_level,
        full_model_adj_r2 = summary(full_model)$adj.r.squared,
        y_only_model_adj_r2 = summary(y_only_model)$adj.r.squared,
        diff_adj_r2 = summary(full_model)$adj.r.squared - summary(y_only_model)$adj.r.squared
    )
    # list(y_only_model, full_model, a)
}

i_map <- load_industry_code_map()
q_map <- load_question_map()
