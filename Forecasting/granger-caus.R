source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))


LEVEL <- 3

ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  filter(level == LEVEL)

kld_tsbl <- ifo_tbl %>%
  select(date, industry_code, KLD) %>%
  pivot_wider(names_from = industry_code, values_from = KLD) %>%
  as_tsibble(index = date) %>%
  mutate(across(where(is.numeric), difference)) # make kld stationary

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


plot_heatmap <- function(df, lag) {
  # Plot heatmap
  title = paste0("Granger causality - Level: ", LEVEL, ", Lag: ", lag)
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
    col = c('red', 'green')
  )

  # Add column names on x-axis
  axis(1, at = 1:ncol(df), labels = colnames(df))

  # Add row names on y-axis (reverse order to match image)
  axis(2, at = 1:nrow(df), labels = rev(rownames(df)), las = 2)
  dev.off()
}

lags <- 0:3


for (lag in lags) {
  pval_matrix <- granger_pval_matrix(kld_tsbl, max_lag = lag)
  plot_heatmap(pval_matrix, lag)
}

pval_matrix <- granger_pval_matrix(kld_tsbl, max_lag = 1)
granger_causes <- rowSums(pval_matrix)
