# First Difference Function
get_first_diff <- function(df) {
  df %>%
    dplyr::group_by(industry_code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(across(where(is.numeric) & !any_of(c("industry_code")), 
                         ~ . - dplyr::lag(.))) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, industry_code, where(is.numeric)) %>%
    na.omit()
}

# Second Difference Function
get_second_diff <- function(df) {
  df %>%
    dplyr::group_by(industry_code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(across(where(is.numeric) & !any_of(c("industry_code")), 
                         ~ dplyr::lead(.) - 2 * . + dplyr::lag(.))) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, industry_code, where(is.numeric)) %>%
    na.omit()
}

# Squared First Difference Function
get_squared_diff <- function(df) {
  df %>%
    dplyr::group_by(industry_code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(across(where(is.numeric) & !any_of(c("industry_code")), 
                         ~ (. - dplyr::lag(.))^2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, industry_code, where(is.numeric)) %>%
    na.omit()
}

# Residual Computation Function
compute_residuals_to_reference <- function(df, reference_code = "C0000000", value_columns = NULL) {
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
    dplyr::select(date, all_of(value_columns))
  
  # Join with all data on date
  residuals_df <- df %>%
    dplyr::filter(industry_code != reference_code) %>%
    dplyr::inner_join(ref_df, by = "date", suffix = c("", "_ref")) %>%
    dplyr::mutate(across(all_of(value_columns), 
                         ~ . - get(paste0(cur_column(), "_ref")))) %>%
    dplyr::select(date, industry_code, all_of(value_columns))
  
  return(residuals_df)
}