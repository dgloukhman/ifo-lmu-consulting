# Load required packages and data
source(here("Forecasting", "helper.R"))

# Load and preprocess ifo data
ifo_tbl <- load_and_preprocess_data(LEVELS)

# Extract the main KLD time series and filter it out from the main table
main_kld <- get_ts_by_question("KLD", ifo_tbl) %>%
  select("C0000000") %>%
  pull("C0000000")
ifo_tbl <- ifo_tbl %>% filter(industry_code != "C0000000")

#' Get a dataframe by a given prediction set
#'
#' @param pred_set A character vector of industry codes.
#' @param ifo_tbl The input tibble with ifo data.
#' @return A dataframe with the selected predictors.
get_df_by_pred_set <- function(pred_set, ifo_tbl) {
  ifo_tbl %>%
    filter(industry_code %in% pred_set) %>%
    select(-c(level)) %>%
    pivot_wider(id_cols = date, names_from = industry_code, values_from = c(KLD, GUS, GES, LUS, BUS, XUS, AVS, BVS, QVS, PVS, QES, PWS, XES))
}

# Create dataframes for the defined predictor sets
top05_pred_df <- get_df_by_pred_set(TOP05_PRED, ifo_tbl)
pred_lm <- lm(main_kld ~ ., data = top05_pred_df)

top05_pred_early_df <- get_df_by_pred_set(TOP05_PRED_EARLY, ifo_tbl)