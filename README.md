# ifo-timeseries

## General

- Folder Data contains all data files (need to be added locally)
- packages.txt contains all packages (packages are installed automatically)
- utils contains utility scripts (data loader and package loader)


## Correlation Analysis

- `lag_report.Rmd` contains the plots and final analysis etc.
- `Folder LagAnalysis` contains copmutation scripts and result data
- Computation is spread into `lag_analysis.R` and `lag_analysis_roll.R`
- Functions for computation are in the scripts `lag_utils.R`, `lag_functions.R` and 
stationarity_cointegration
- Results are saved to `LagAnalysis/results folder` 

Run Order:
1) LagAnalysis/lag_analysis.R
2) LagAnalysis/lag_analysis_roll.R
3) lag_report.Rmd


## Forecasting Analysis

This directory contains R scripts for performing univariate and multivariate Granger causality analysis on IFO time series data to identify leading indicators for the main KLD index.


- `config.R`: Holds all the configuration variables for the forecasting scripts, such as significance levels, analysis levels, top predictors, and maximum lag for Granger causality tests.
- `helper.R`: A utility script with functions for loading and preprocessing data, creating lagged dataframes for regression, and performing the core Granger causality test logic.
- `univariate.R`: Conducts a univariate Granger causality analysis. It iterates through specified questions and industries, testing each time series individually as a potential predictor for the main KLD time series.
- `multivariate.R`: Conducts a multivariate Granger causality analysis. It uses all available questions for a given industry as a combined set of predictors for the main KLD time series.
- `plotting.R`: Generates various plots to visualize the results from the analysis, including comparisons of adjusted R-squared values, time series plots, and distributions of causal relationships.
- `inter-timeseries-exploration.R`: Contains an exploratory analysis of inter-time series relationships using Granger causality. This script was not used in the final report but is kept for reference.


## Turning Point Indication Analysis

This section describes the scripts used for identifying leading indicators for business cycle turning points. The main goal is to find sub-series from the IFO survey that can predict turning points in the main manufacturing business climate index in real-time.

- `TurningPointMain.R`: This is the main script for the analysis. It identifies turning points in the main index using the Bry-Boschan algorithm as a benchmark. Then, it computes real-time turning point signals for sub-series using Markov-Switching models. It evaluates individual sub-series and also groups of sub-series to create a composite indicator, which is then evaluated.
- `turningpoint_utils.R`: Contains all the core utility functions for the turning point analysis. This includes functions for the Bry-Boschan algorithm, Markov-Switching models, signal evaluation, and grouping of series.
- `BB_TurningPoints.R`: A script focused on the main business climate index. It identifies turning points using both the Bry-Boschan algorithm and a Markov-Switching model and compares them in a plot.
- `TP_UnusedStuff/`: This directory contains various scripts from earlier, exploratory stages of the analysis. They are not part of the final workflow but are kept for reference.

Run Order:
The analysis can be run via `TurningPointMain.R`. `BB_TurningPoints.R` can be run independently for its specific comparison plot.