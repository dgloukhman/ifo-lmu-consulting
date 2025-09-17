source("utils/setup_packages.R")
install_packages_from_file()
source(here("utils", "load_data.R"))
source(here("Forecasting", "helper.R"))

top05_pred <- c("C2220000", "C1700000", "C2200000", "C1720000", "C2700000")
top05_pred_early <- c("C1700000", "C2220000", "C2200000", "C1600000", "C1720000")

SIGNIFICANCE_LEVEL <- 0.05


ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data()

main_kld <- get_ts_by_question("KLD", ifo_tbl) %>%
  select("C0000000") %>%
  pull("C0000000")
ifo_tbl <- ifo_tbl %>% filter(industry_code != "C0000000")

get_df_by_pred_set <- function(pred_set, ifo_tbl) {
  ifo_tbl %>%
    filter(industry_code %in% pred_set) %>%
    select(-c(level)) %>%
    pivot_wider(id_cols = date, names_from = industry_code, values_from = c(KLD, GUS, GES, LUS, BUS, XUS, AVS, BVS, QVS, PVS, QES, PWS, XES))
}
top05_pred_df <- get_df_by_pred_set(top05_pred, ifo_tbl)
pred_lm <- lm(main_kld ~ ., data = top05_pred_df)

top05_pred_early_df <- get_df_by_pred_set(top05_pred_early, ifo_tbl)