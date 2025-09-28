# ifo-timeseries

## General

- Folder Data contains all data files (need to be added locally)
- packages.txt contains all packages (packages are installed automatically)
- utils contains utility scripts (data loader and package loader)


## Correlation Analysis

- lag_report.Rmd contains the plots and final analysis etc.
- Folder LagAnalysis contains copmutation scripts and result data
- Computation is spread into lag_analysis.R and lag_analysis_roll.R
- Functions for computation are in the scripts lag_utils.R, lag_functions.R and 
stationarity_cointegration
- Results are saved to LagAnalysis/results folder 

Run Order:
1) LagAnalysis/lag_analysis.R
2) LagAnalysis/lag_analysis_roll.R
3) lag_report.Rmd