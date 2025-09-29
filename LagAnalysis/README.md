# Lag Analysis

This directory contains R scripts for analyzing lead–lag structures in ifo Business Survey time series. The goal is to identify sectoral dynamics, turning points, and stationarity properties across industries, both in full-sample and rolling-window settings.

## File Descriptions

- **lag_analysis.R**: Runs the full lag analysis across industries and questions, producing cross-correlation and distance-correlation results.  
- **lag_analysis_roll.R**: Conducts rolling-window lag analysis to capture time-varying relationships and structural changes.  
- **lag_functions.R**: Core functions for cross-correlation, peak detection, lag alignment, and supporting statistical tests.  
- **lag_utils.R**: Utility functions for preprocessing, reshaping, and handling analysis outputs.  
- **stationarity_cointegration.R**: Provides functions for stationarity and cointegration testing (ADF, KPSS, Johansen, etc.). Called from the lag analysis scripts, but can also be used independently.  
- **lag_report.Rmd**: RMarkdown report combining results, visualizations, and tables into a reproducible document.

## Configuration

The scripts can be customized through parameters set within the analysis scripts themselves. Key elements include:

- **Indicators**: Choice of survey questions (KLD, KLM, KLB, etc.) to include.  
- **Lag Settings**: Maximum lag order for cross-correlation analysis.  
- **Rolling Windows**: Window size and step length for rolling lag analysis.  
- **Stationarity Tests**: Selection of methods for testing unit roots and cointegration.  

## Workflow (Run Order)

1. **Full-Sample Analysis**  
   Run `LagAnalysis/lag_analysis.R` to compute lag structures over the entire dataset.  

2. **Rolling-Window Analysis**  
   Run `LagAnalysis/lag_analysis_roll.R` to evaluate time-varying dynamics.  
   - **Note (partial results for testing):** The script is currently configured to process only a **subset** of rolling windows for faster debug runs (e.g., using `.[1:5]` after `group_split()`). This intentionally produces **partial** rolling results. Remove or disable this limitation before final runs (e.g., comment out the subsetting or guard it with a `DEBUG_ROLL <- FALSE` flag) to generate complete outputs.

3. **Report Generation**  
   Knit `lag_report.Rmd` to produce an HTML/PDF report summarizing results, figures, and interpretations.

4. **(Optional) Stationarity & Cointegration**  
   While functions in `stationarity_cointegration.R` are already called from the lag analysis scripts, they can also be used independently for testing unit roots and cointegration among industry indices.