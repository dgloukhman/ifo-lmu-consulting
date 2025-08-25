library(ggplot2)

adj_r2_plot_f <- function(data) {
    ggplot(data, aes(x = code)) +
        # Plot y_only_model_adj_r2 as a line
        geom_line(aes(y = y_only_model_adj_r2, group = 1), color = "blue", size = 1) +

        # Plot full_model_adj_r2 as points
        geom_point(aes(y = full_model_adj_r2), color = "red", size = 3) +

        # Add labels and a title
        labs(
            title = "Model R-squared Comparison",
            x = "Code",
            y = "Adjusted R-squared"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
