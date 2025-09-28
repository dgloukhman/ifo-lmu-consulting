# This analysis explores inter-time series relationships using Granger causality tests.
# It did not end up in the final report but is kept for reference.

source(here("Forecasting", "helper.R"))

# Define the level for the analysis
LEVEL <- 3

# Load and preprocess the ifo data
ifo_tbl <- load_and_preprocess_data(LEVEL)

# Create a stationary time series for KLD
kld_tsbl <- ifo_tbl %>%
  select(date, industry_code, KLD) %>%
  pivot_wider(names_from = industry_code, values_from = KLD) %>%
  as_tsibble(index = date) %>%
  mutate(across(where(is.numeric), difference)) # make kld stationary

#' Create a Granger causality p-value matrix
#'
#' @param tsbl The input tsibble.
#' @param max_lag The maximum lag to consider.
#' @return A matrix with p-values.
granger_pval_matrix <- function(tsbl, max_lag = 1) {
  vars <- names(tsbl)[names(tsbl) != "date"]
  n <- length(vars)
  pval_mat <- matrix(NA, nrow = n, ncol = n, dimnames = list(vars, vars))

  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      if (i != j) {
        x <- tsbl[[vars[i]]]
        y <- tsbl[[vars[j]]]
        df <- data.frame(x = x, y = y)
        df <- na.omit(df)
        if (nrow(df) > max_lag + 1) {
          test <- try(
            lmtest::grangertest(x ~ y, order = max_lag, data = df),
            silent = TRUE
          )
          if (!inherits(test, "try-error")) {
            pval_mat[i, j] <- test$`Pr(>F)`[2]
          }
        }
      }
    }
  }

  pval_mat <- ifelse(pval_mat < .05, 1, 0)
  diag(pval_mat) <- 1

  return(pval_mat)
}

#' Plot a heatmap of the Granger causality matrix
#'
#' @param df The input dataframe (matrix).
#' @param lag The lag used for the analysis.
plot_heatmap <- function(df, lag) {
  # Plot heatmap
  title <- paste0("Granger causality - Level: ", LEVEL, ", Lag: ", lag)
  jpeg(
    paste0("plots/granger_level_", LEVEL, "_lag_", lag, ".jpg"),
    width = 800,
    height = 600
  )

  image(
    1:ncol(df),
    1:nrow(df),
    t(as.matrix(df[nrow(df):1, ])),
    main = title,
    axes = FALSE,
    xlab = "",
    ylab = "",
    col = c("red", "green")
  )

  # Add column names on x-axis
  axis(1, at = 1:ncol(df), labels = colnames(df))

  # Add row names on y-axis (reverse order to match image)
  axis(2, at = 1:nrow(df), labels = rev(rownames(df)), las = 2)
  dev.off()
}

# Define the lags to test
lags <- 0:3

# Loop through the lags and create heatmaps
for (lag in lags) {
  pval_matrix <- granger_pval_matrix(kld_tsbl, max_lag = lag)
  plot_heatmap(pval_matrix, lag)
}

# Calculate the number of Granger causes for a specific lag
pval_matrix <- granger_pval_matrix(kld_tsbl, max_lag = 1)
granger_causes <- rowSums(pval_matrix)
