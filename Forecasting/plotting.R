# Load ggplot2 for plotting
library(ggplot2)

#' Plot adjusted R-squared for full vs. reduced models
#'
#' @param data A dataframe containing the adjusted R-squared values.
#' @return A ggplot object.
adj_r2_plot_f <- function(data) {
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
        "adj_r2_univariate.png",
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
plot_cluster_with_main <- function(ifo_tbl, ts_1, ts_2) {
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
        "main_vs_top.png",
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
plot_distribution_of_gc_questions <- function(data) {
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
        "distribution_gc_questions.png",
        p,
        width = 16,
        height = 9,
        units = "cm" # or "in"
    )
    p
}