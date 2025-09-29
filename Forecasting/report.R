# Forecasting/report.R
# This script contains the code to reproduce the visualizations and tables for the Indication chapter of the report.
# Load ggplot2 for plotting

source("utils/setup_packages.R")
install_packages_from_file()

library(here)
source(here("Forecasting", "helper.R"))
source(here("Forecasting", "univariate.R"))
library(ggplot2)

#' Plot adjusted R-squared for full vs. reduced models
#'
#' @param data A dataframe containing the adjusted R-squared values.
#' @return A ggplot object.
adj_r2_plot_f <- function(data, forecast_type) {
    data <- data %>%
        group_by(industry_code) %>%
        filter(causal == TRUE & full_model_adj_r2 == max(full_model_adj_r2)) %>%
        ungroup() %>%
        mutate(question = q_map[question], code = industry_code, industry_code = i_map[industry_code])


    p <- ggplot(data, aes(x = code)) +
        # Reduced model line
        geom_line(
            aes(y = y_only_model_adj_r2, color = "Reduced model"),
            group = 1,
            size = 1
        ) +

        # Full model points
        geom_point(
            aes(y = full_model_adj_r2, color = "Full model"),
            size = 3
        ) +
        labs(
            title = "R-squared full model vs. reduced model",
            x = "Industries", # <-- add x-axis label here
            y = "Adjusted R-squared",
            color = "Model"
        ) +
        scale_color_manual(
            values = c("Reduced model" = "blue", "Full model" = "red")
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_blank() # removes tick labels, keeps the axis title
        )

    ggsave(
        paste0("adj_r2_plot_", forecast_type, ".png"),
        p,
        width = 16,
        height = 9,
        units = "cm" # or "in"
    )
    p
}

#' Plot a time series
#'
#' @param data A dataframe containing the time series data.
#' @return A ggplot object.
plot_ts <- function(data) {
    ggplot(data = data, aes(x = date, y = KLD, color = industry)) +
        geom_line() + # This creates the line plot
        labs(
            title = "KLD Over Time by Industry",
            x = "date",
            y = "KLD",
            color = "Industry" # This will be the title of the legend
        ) +
        theme_minimal() # A clean theme for the plot
}

#' Plot two time series against each other
#'
#' @param ifo_tbl The input tibble with ifo data.
#' @param ts_1 The first time series.
#' @param ts_2 The second time series.
#' @return A ggplot object.
plot_cluster_with_main <- function(ifo_tbl, ts_1, ts_2, forecast_type) {
    # Pivot to long format for easier filtering
    data <- ifo_tbl %>%
        filter(industry_code %in% c(ts_1, ts_2)) %>%
        select(date, industry_code, KLD) %>%
        pivot_wider(id_cols = date, names_from = industry_code, values_from = KLD)


    # Plot
    p <- ggplot(data, aes(x = date)) +
        geom_line(aes(y = !!sym(ts_1), color = "Top Performer")) +
        geom_line(aes(y = !!sym(ts_2), color = "Main Index")) +
        labs(
            title = "Main Index vs Top Performer",
            x = "Date", y = "Value",
            color = "Series"
        ) +
        scale_color_manual(
            values = c("Main Index" = "blue", "Top Performer" = "grey20")
        ) +
        ylim(c(-100, 100)) +
        theme_minimal()

    ggsave(
        paste0("main_vs_top_", forecast_type, ".png"),
        p,
        width = 16,
        height = 9,
        units = "cm" # or "in"
    )
    p
}

#' Plot the distribution of Granger causality by question
#'
#' @param data A dataframe containing the Granger causality results.
#' @return A ggplot object.
plot_distr_of_gc_questions <- function(data, forecast_type) {
    df <- data %>%
        filter(industry_code != "date") %>%
        group_by(question) %>%
        summarise(fraction_true = mean(causal))


    p <- ggplot(df, aes(x = question, y = fraction_true)) +
        geom_col(fill = "blue") +
        geom_text(aes(label = round(fraction_true, 2)),
            vjust = -0.5, size = 3.5
        ) +
        ylim(0, 1) +
        labs(
            title = "Distribution of Granger Causality by Question",
            x = "Question",
            y = "Percentage of Granger Causality"
        ) +
        theme_minimal()

    ggsave(
        paste0("distribution_gc_questions_", forecast_type, ".png"),
        p,
        width = 16,
        height = 9,
        units = "cm" # or "in"
    )
    p
}

#' Create a report table from Granger causality results
#'
#' @param data A dataframe containing the Granger causality results.
#' @param univariate A boolean indicating if the data is from univariate analysis.
#' @return A tibble object.
create_report_table <- function(data, univariate = TRUE) {
    cols <- c(
        "industry_code"
    )
    if (univariate) {
        cols <- c(cols, "question")
    }

    cols <- c(
        cols,
        "lag",
        "diff_adj_r2"
    )
    print(cols)

    data %>%
        group_by(industry_code) %>%
        filter(causal == TRUE & full_model_adj_r2 == max(full_model_adj_r2)) %>%
        ungroup() %>%
        arrange(desc(diff_adj_r2)) %>%
        select(all_of(cols))
}

ifo_tbl <- load_and_preprocess_data(UNIVARIATE_LEVELS)
ifo_tbl_univariate <- ifo_tbl %>% filter(industry_code != "C0000000")

main_kld <- get_ts_by_question("KLD", ifo_tbl) %>% pull("C0000000")




#' @description
#' Generates all necessary artifacts for a given forecast type.
#' This function orchestrates the creation of outputs such as plots, tables,
#' and reports based on the specified forecasting method.
#'
#' @param forecast_type Character string indicating the type of forecast to process.
#' Supported values depend on the implementation details.
#'
#' @return Tuple of tibbles. Side effects include creation of files and/or visualizations.
create_all_artifacts <- function(forecast_type) {
    granger_univariate_tbl <- granger_main(
        forecast_type = forecast_type,
        main_kld = main_kld,
        ifo_tbl = ifo_tbl_univariate,
        questions = setdiff(names(ifo_tbl_univariate), c("date", "industry_code", "level"))
    )
    adj_r2_plot_f(granger_univariate_tbl, forecast_type)
    plot_distr_of_gc_questions(granger_univariate_tbl, forecast_type)
    univariate_report_table <- create_report_table(granger_univariate_tbl, TRUE)

    granger_multivariate_tbl <- multivariate_granger_main(ifo_tbl, forecast_type)
    multivariate_report_table <- create_report_table(granger_multivariate_tbl, FALSE)

    top_ts <- granger_univariate_tbl %>%
        filter(causal == TRUE) %>%
        slice_max(diff_adj_r2, n = 1) %>%
        pull(industry_code)
    plot_cluster_with_main(ifo_tbl, top_ts, "C0000000", forecast_type)
    return(univariate_report_table, multivariate_report_table)
}



forecast_types <- c("simple", "instantaneous")

purrr::map(
    forecast_types,
    create_all_artifacts
)
