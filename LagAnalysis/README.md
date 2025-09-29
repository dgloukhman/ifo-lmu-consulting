# Lag Analysis

This directory contains R scripts for analyzing lead–lag structures in ifo Business Survey time series. The goal is to identify sectoral dynamics, turning points, and stationarity properties across industries, both in full-sample and rolling-window settings.

## File Descriptions

- **lag_analysis.R**: Runs the full lag analysis across industries and questions, producing cross-correlation and distance-correlation results.  
- **lag_analysis_roll.R**: Conducts rolling-window lag analysis to capture time-varying relationships and structural changes.  
- **lag_functions.R**: Core functions for cross-correlation, peak detection, lag alignment, and supporting statistical tests.  
- **lag_utils.R**: Utility functions for preprocessing, reshaping, and handling analysis outputs.  
- **stationarity_cointegration.R**: Performs stationarity and cointegration testing (ADF, KPSS, Johansen, etc.) for each series.  
- **lag_report.Rmd**: RMarkdown report combining results, visualizations, and tables into a reproducible document.

## Configuration

The scripts can be customized through parameters set within the analysis scripts themselves. Key elements include:

- **Indicators**: Choice of survey questions (KLD, KLM, KLB, etc.) to include.  
- **Lag Settings**: Maximum lag order for cross-correlation analysis.  
- **Rolling Windows**: Window size and step length for rolling lag analysis.  
- **Stationarity Tests**: Selection of methods for testing unit roots and cointegration.  

## Workflow

1. **Full-Sample Analysis**:  
   Run `lag_analysis.R` to compute lag structures over the entire dataset.  

2. **Rolling-Window Analysis**:  
   Run `lag_analysis_roll.R` to evaluate time-varying dynamics.  

3. **Stationarity & Cointegration**:  
   Run `stationarity_cointegration.R` to test for unit roots and cointegration among industry indices.  

4. **Report Generation**:  
   Knit `lag_report.Rmd` to produce an HTML/PDF report summarizing results, figures, and interpretations.  