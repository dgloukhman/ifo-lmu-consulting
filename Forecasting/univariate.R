source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))


# Which ts predicts the main index the best
SIGNIFICANCE_LEVEL <- 0.05
LEVEL <- 3

ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data() %>%
  filter(level <= LEVEL)

get_ts_by_question <- function(question = "KLD", ifo_tbl) {
  ifo_tbl %>%
    select(date, industry_code, question) %>%
    pivot_wider(names_from = industry_code, values_from = question) %>%
    as_tsibble(index = date)
}

main_kld <- get_ts_by_question("KLD", ifo_tbl) %>% select("C0000000")

questions <- setdiff(names(ifo_tbl), c("date", "industry_code", "level"))

granger_main <- function() {
  y <- main_kld[["C0000000"]]

  granger_test_vec <- function(x, lag = 1) {
    df <- data.frame(x = x, y = y)
    test <- lmtest::grangertest(y ~ x, order = lag, data = df)
    tibble(
      weight = test$F[2],
      causal = test$`Pr(>F)`[2] < SIGNIFICANCE_LEVEL
    )
  }

  results <- purrr::map_dfr(questions, function(q) {
    data <- get_ts_by_question(q, ifo_tbl) %>% select(!"C0000000")
    vars <- expand_grid(industry_code = names(data), lag = 1:5)

    purrr::pmap_dfr(vars, function(industry_code, lag) {
      res <- granger_test_vec(data[[industry_code]], lag = lag)
      tibble(question = q, industry_code = industry_code, weight = res$weight, causal = res$causal, lag = lag)
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
granger_tbl <- granger_main() %>%
  arrange(desc(weight))
# granger_tbl %>% plot_weight()
