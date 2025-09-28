# Configuration for forecasting scripts

# Significance level for Granger causality tests
SIGNIFICANCE_LEVEL <- 0.05

# Levels for multivariate analysis
LEVELS <- c(0, 2)

# Levels for univariate analysis
UNIVARIATE_LEVELS <- c(0, 2, 3)

# Top 5 predictors
TOP05_PRED <- c("C2220000", "C1700000", "C2200000", "C1720000", "C2700000")

# Top 5 early predictors
TOP05_PRED_EARLY <- c("C1700000", "C2220000", "C2200000", "C1600000", "C1720000")

# Maximum lag for Granger causality tests
MAX_LAG <- 6
