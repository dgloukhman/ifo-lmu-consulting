compare_augmented_ica_to_baseline <-  function(component, lags, baseline = FALSE) {
  # extract lag values and assign to names
  min_lag <- min(lags)
  max_lag <- max(lags)
  main_lag_names <- paste0("main_kld", "_lag", min_lag:max_lag)
  component_lag_names <- paste0(component, "_lag", min_lag:max_lag)
  
  # build formula with these names
  baseline_formula <- as.formula(paste0("main_kld", " ~ ", paste(main_lag_names, collapse = " + ")))
  #aug_formula <- as.formula(paste0("main_kld", " ~ ", paste(c(main_lag_names, component_lag_names), collapse = " + ")))
  aug_formula <- as.formula(paste0("main_kld", " ~ ", paste(c(main_lag_names, component_lag_names), collapse = " + ")))
  
  # fit Y only baseline model and augmented model with formula 
  fit <- lm(aug_formula, data = df)
  
  if(baseline == TRUE) {
    fit <- lm(baseline_formula, data=df)
  }
  
  #return(mean((aug_fit_full$fitted.values - df$main_kld)^2))
  mae <- mean(abs(fit$fitted.values - df$main_kld))
  mse <- mean(abs(fit$fitted.values - df$main_kld)^2)
  adj_r2 <- summary(fit)$adj.r.squared
  r2 <- summary(fit)$.r.squared
  return(tibble(mae = mae, mse = mse, adj_r2 = adj_r2, r2 = r2, component = component))
}

ica_get_ccf_matrix <- function(df, lag.max = 6) {
  
  results <- lapply(ica_colnames, function(comp) {
    ccf_out <- ccf(df[[comp]], df$main_kld, lag.max = lag.max, plot = FALSE)
    tibble::tibble(
      component = comp,
      lag = ccf_out$lag,
      correlation = ccf_out$acf[,1,1]
    )
  }) |> dplyr::bind_rows()
  
  # pivot to wide format
  results_wide <- results |>
    tidyr::pivot_wider(
      names_from = lag,
      values_from = correlation,
      names_prefix = "ccf_"
    ) |>
    dplyr::arrange(component)
  
  return(results_wide)
}