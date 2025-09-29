# ifo-timeseries

This repository contains the code used for the consulting project 'Uncovering Structural Relations in the ifo Business Survey', with multiple different analysis in the corresponding folders. 

## General

- Folder Data contains all data files (need to be added locally)
- packages.txt contains all packages (packages are installed automatically)
- utils contains utility scripts (data loader and package loader)

## Running 
- We use the 'here' package to avoid path issues. Make sure that its installed/loaded beforehand, the rest is done via `install_packages_from_file()`
- Some parts are computationally more intensive and might take several hours to run. For this, (intermediate) results with typical settings are stored as .csv in the Data folder, for a simple quick run. They need to be added locally. 


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
