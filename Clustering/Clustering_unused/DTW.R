# Install if you haven't already
install.packages("dtwclust")

library(dtwclust)
library(dplyr)
library(tidyr)

# --- 1. Prepare data in the required format (a list of series) ---
# We'll use the same Level 2 data as before
ts_list <- ifo_long %>%
  filter(level == 2) %>%
  select(date, industry_code, question_code, value) %>%
  # Create a unique ID for each series
  unite("id", industry_code, question_code, sep = "_") %>%
  # convert to a list where each element is a numeric vector (a time series)
  split(.$id) %>%
  lapply(function(x) x$value)

# --- 2. Perform hierarchical clustering with DTW ---
# This can be computationally intensive if you have many series
hc_dtw <- tsclust(
  ts_list,
  type = "hierarchical",
  k = 50, # Let's stick with 50 clusters
  distance = "dtw_basic", # Use a basic (fast) DTW distance
  control = hierarchical_control(method = "ward.D2"),
  preproc = zscore # Pre-process by scaling series (recommended)
)

# --- 3. Plot the results ---
# This plot shows each series in its cluster, along with the cluster's "centroid" or medoid shape
plot(hc_dtw)