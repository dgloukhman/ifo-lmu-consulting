# Forecasting Analysis

This directory contains R scripts for performing univariate and multivariate Granger causality analysis on IFO time series data to identify leading indicators for the main KLD index.

## File Descriptions

- `config.R`: Holds all the configuration variables for the forecasting scripts, such as significance levels, analysis levels, top predictors, and maximum lag for Granger causality tests.
- `helper.R`: A utility script with functions for loading and preprocessing data, creating lagged dataframes for regression, and performing the core Granger causality test logic.
- `univariate.R`: Code for conducting a univariate Granger causality analysis. It iterates through specified questions and industries, testing each time series individually as a potential predictor for the main KLD time series.
- `multivariate.R`: Code for conducting a multivariate Granger causality analysis. It uses all available questions for a given industry as a combined set of predictors for the main KLD time series.
- `report.R`: Generates various plots to visualize the results from the analysis, including comparisons of adjusted R-squared values, time series plots, and distributions of causal relationships.
- `inter-timeseries-exploration.R`: Contains an exploratory analysis of inter-time series relationships using Granger causality. This script was not used in the final report but is kept for reference.

## Configuration

The analysis can be customized via the `config.R` file:

- `SIGNIFICANCE_LEVEL`: The p-value threshold for the Granger causality F-test.
- `LEVELS`: The industry levels (e.g., 0 for the main index, 2 for 2-digit industries) to include in the multivariate analysis.
- `UNIVARIATE_LEVELS`: The industry levels to include in the univariate analysis.
- `TOP05_PRED` / `TOP05_PRED_EARLY`: Pre-defined sets of top predictor codes.
- `MAX_LAG`: The maximum number of lags to consider in the regression models for the Granger causality test.

## Workflow

1.  **Configuration**: Set the desired parameters in `config.R`.
2.  **Execution**: The `report.R` script is the main entry point that runs the analysis for both "simple" and "instantaneous" forecast types and generates the corresponding plots. It saves the plots as PNG files in the root directory.
