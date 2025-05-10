# First Difference Function
get_first_diff <- function(df) {
  df %>%
    dplyr::group_by(industry_code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(across(where(is.numeric) & !any_of(c("industry_code")), 
                         ~ dplyr::lag(.) - ., 
                         .names = "diff1_{.col}")) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, industry_code, starts_with("diff1_")) %>%
    na.omit()
}

# Second Difference Function
get_second_diff <- function(df) {
  df %>%
    dplyr::group_by(industry_code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(across(where(is.numeric) & !any_of(c("industry_code")), 
                         ~ dplyr::lag(.) - 2 * . + dplyr::lead(.), 
                         .names = "diff2_{.col}")) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, industry_code, starts_with("diff2_")) %>%
    na.omit()
}

# Squared First Difference Function
get_squared_diff <- function(df) {
  df %>%
    group_by(industry_code) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(across(where(is.numeric) & !any_of(c("industry_code")), 
                  ~ (.-lag(.))^2, 
                  .names = "squared_diff_{.col}")) %>%
    ungroup()
}

# Residual Computation Function
compute_residuals_to_reference <- function(df, reference_code, value_columns = NULL) {
  # If no columns specified, default to all numeric except date and industry_code
  if (is.null(value_columns)) {
    value_columns <- df %>%
      dplyr::select(-date, -industry_code) %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
  }
  
  # Extract reference industry time series
  ref_df <- df %>%
    dplyr::filter(industry_code == reference_code) %>%
    dplyr::select(date, all_of(value_columns)) %>%
    dplyr::rename_with(~ paste0("ref_", .), all_of(value_columns))
  
  # Join with all data on date
  residuals_df <- df %>%
    dplyr::filter(industry_code != reference_code) %>%
    dplyr::inner_join(ref_df, by = "date") %>%
    dplyr::mutate(across(all_of(value_columns), 
                         ~ . - get(paste0("ref_", cur_column())),
                         .names = "resid_{.col}")) %>%
    dplyr::select(date, industry_code, starts_with("resid_"))
  
  return(residuals_df)
}