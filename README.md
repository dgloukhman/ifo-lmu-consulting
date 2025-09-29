# ifo-timeseries

This repository contains the code used for the project 'Uncovering Structural Relations in the ifo Business Survey' as a consulting-project for the Master's program in Statistics & Data Science at [Ludwig Maximilian University of Munich](https://www.stat.lmu.de/en/). 
The data used in these analyses represents only a subset of the [ifo Business Survey](https://www.ifo.de/en/survey/ifo-business-climate-index-germany) and covers only the manufacturing industries.
### General

- Folder Data contains all data files (need to be added locally)
- packages.txt contains all packages (packages are installed automatically)
- utils contains utility scripts (data loader and package loader)

### Code Execution
- The code-base is written in `R`
- Execute the code from the repo-root as the working directory 
- We use the `here` package to avoid path issues. Make sure that its installed/loaded beforehand, the rest is done via `install_packages_from_file()`
- Some parts are computationally more intensive and might take several hours to run. For this, (intermediate) results with typical settings are stored as .csv in the Data folder, for a simple quick run. They need to be added locally. 

### Folder Structure


- `LagAnalysis`: This directory 



- `Forecasting`: This directory contains scripts for performing univariate and multivariate Granger causality analysis identify leading indicators for the aggregate main-index.

- `Clustering`: This section contains the script for the CCF-based clustering. Individual series are clustered, according to their CCF with the main index.

 

- `TurningPointIndications` : This section describes the scripts used for identifying leading indicators for business cycle turning points. The main goal is to find sub-series from the IFO survey that can predict turning points in the main manufacturing business climate index in real-time.

Every Folder contains its own `README.md`

## Authors
- [Jakob Freytag](https://github.com/JakobFreytag)
- [Daniel Gloukhman](https://github.com/dgloukhman)
- [Domink Pitz](https://github.com/domi-2)





