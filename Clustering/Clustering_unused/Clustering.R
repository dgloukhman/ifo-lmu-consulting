library(here)
source(here("utils","setup_packages.R"))

install_packages_from_file()

source(here("utils","load_data.R"))


# load in data and preprocess
data_path <- here("Data")
data_path_dict <- here("Data")
ifo_tbl <- read_ifo_data(data_path)
ifo_tbl <- preprocess_ifo_data(ifo_tbl)

question_df <- read.csv(paste0(data_path, "/questions_codes_titles.csv"))
industries_df <- read.csv(paste0(data_path, "/industries_codes_titles.csv"))

# long format, useful later on 
ifo_long <- ifo_tbl %>%
  pivot_longer(
    cols = -c(date, industry_code, level),
    names_to = "question_code",
    values_to = "value"
  )

# subset to three-digits
ts_df <- ifo_tbl %>% 
  filter(level == 2) %>% 
  select(-level)

# extract question_variables
question_vars <- ts_df %>%
  select(-c(date, industry_code)) %>%
  colnames()

# extract C00 kld as main series and rename to keep distinguishable
c00_kld <- ifo_tbl %>% 
  filter(industry_code == "C0000000") %>% 
  select(date, KLD) %>% 
  rename(main_kld = KLD)

# Get ccf vector for all series (13 elements, lags ranging from -6 to 6)
ts_ccf_matrix <- get_ccf_matrix(ts_df, c00_kld, question_vars, 6, only_negative_lag = FALSE)

# euclidean clustering
dist_mat <- dist(as.matrix(ts_ccf_matrix), method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

# Cut into clusters and assign in extra column 
k <- 40
ts_clusters <- cutree(hc, k)
ts_ccf_matrix$cluster <- ts_clusters

# Take a look at mean ccf of resulting clusters and plot
cluster_ccf_summary <- ts_ccf_matrix %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(starts_with("ccf_"), ~mean(.x))) 
  
plot_ccfs_cluster(ts_ccf_matrix)

#===============================================================================
# take average values of all time series in each cluster 
# join cluster assignments back to long format
ifo_long_with_cluster <- ifo_long %>%
  inner_join(
    ts_ccf_matrix %>% 
      rownames_to_column("id") %>%
      separate(id, into = c("industry_code", "question_code"), sep = "_") %>%
      select(industry_code, question_code, cluster),
    by = c("industry_code", "question_code")
  )

# compute cluster averages
cluster_indices <- ifo_long_with_cluster %>%
  group_by(date, cluster) %>%
  summarise(cluster_value = mean(value), .groups = "drop")

# in wide format
cluster_indices_wide <- cluster_indices %>%
  pivot_wider(names_from = cluster, values_from = cluster_value,
              names_prefix = "cluster_")

cluster_indices_wide$main_kld <-  c00_kld$main_kld
head(cluster_indices_wide)

#===============================================================================
# take ccf with main for each of these cluster averages 
# function to compute ccf for one series vs main_kld
get_ccf_vec <- function(x, y, max_lag = 6) {
  ccf_res <- ccf(x, y, lag.max = max_lag, plot = FALSE, na.action = na.omit)
  tibble(
    lag = ccf_res$lag,
    ccf = ccf_res$acf[,1,1]
  )
}

# apply to each cluster
ccf_cluster_df <- cluster_indices_wide %>%
  select(-date) %>%   # drop date
  pivot_longer(starts_with("cluster_"), names_to = "cluster", values_to = "cluster_value") %>%
  group_by(cluster) %>%
  group_modify(~ {
    ccf_tbl <- get_ccf_vec(.x$cluster_value, cluster_indices_wide$main_kld, max_lag = 6)
    ccf_tbl
  }) %>%
  ungroup()

# pivot to wide format: one row per cluster, columns for each lag
ccf_cluster_wide <- ccf_cluster_df %>%
  mutate(lag_name = paste0("ccf_", lag)) %>%
  select(-lag) %>%
  pivot_wider(names_from = lag_name, values_from = ccf)

plot_ccfs_cluster(ccf_cluster_wide)
plot_ccfs_cluster(ts_ccf_matrix)
#===============================================================================
# Looking at peak ccf (lets compare if it differs from ccf_cluster df or ccf_clusetwi)
ccf_cluster_wide #ccf of mean cluster ts
cluster_ccf_summary # mean ccf of all cluster ts

cluster_ccf_summary2 <- cluster_ccf_summary %>%
  pivot_longer(
    cols = starts_with("ccf_"),
    names_to = "lag",
    values_to = "ccf"
  ) %>%
  mutate(lag = as.integer(str_remove(lag, "ccf_"))) %>%
  group_by(cluster, n) %>%
  slice_max(ccf, n = 1, with_ties = FALSE) %>%   # pick lag with max correlation
  rename(max_lag = lag, max_ccf = ccf) %>%
  ungroup()

cluster_ccf_summary <- cluster_ccf_summary %>%
  left_join(cluster_ccf_summary2, by = c("cluster", "n"))
view(cluster_ccf_summary)
#===============================================================================
# Closer look at single cluster 
cluster_df <- ts_ccf_matrix %>% filter(cluster == 22) %>%
  rownames_to_column(var = "id") %>%
  separate(id, into = c("industry_code", "question_code"), sep = "_") %>% 
  dplyr::select(industry_code, question_code)
cluster_df

# plot the cluster together with main
plot_cluster_with_main(ifo_tbl, cluster_df)
# -> looks promising, lets evaluate common movement of the cluster

# extract values of all cluster time series
cluster_ts_data <- ifo_long %>% 
  inner_join(cluster_df, by = c("industry_code", "question_code"))

# average them to common index
cluster_index <- cluster_ts_data %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(cluster_value = mean(value)) # Average them

kld_cluster_df <- c00_kld %>% 
  inner_join(cluster_index, by = "date")

ggplot(kld_cluster_df, aes(x = date)) +
  geom_line(aes(y = main_kld, color = "main_kld")) +
  geom_line(aes(y = cluster_value, color = "cluster_value")) +
  labs(title = "Main KLD vs Cluster Value over Time",
       x = "Date", y = "Value", color = "Series") +
  theme_minimal()

cluster_df <- df_codes_to_titles(cluster_df,
                                 question_code_col_name = "question_code",
                                 industry_code_col_name = "industry_code")
cluster_df
#===============================================================================
# Normalize and take MAE for each lag of main_series 
cols_to_normalize <- setdiff(colnames(cluster_indices_wide), "date")

# Normalize cluster columns
cluster_indices_wide <- cluster_indices_wide %>%
  mutate(across(all_of(cols_to_normalize), ~ (. - mean(.)) / sd(.)))

# Function to compute MAE for a given lag without Metrics package
# Compute MAE in long format
compute_mae_lag_long <- function(data, lag_val) {
  data %>%
    mutate(main_kld_lag = lead(main_kld, lag_val)) %>%
    summarise(across(all_of(cols_to_normalize),
                     ~ mean(abs(main_kld_lag - .), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "cluster", values_to = "MAE") %>%
    mutate(lag = lag_val)
}

# Apply for lags 1:6 and combine
mae_results_long <- map_dfr(1:6, ~ compute_mae_lag_long(cluster_indices_wide, .x))
mae_results_long <- mae_results_long %>%
                      mutate(lag_name = paste0("lag_", lag)) %>%
                      select(-lag)%>% 
                      pivot_wider(names_from = lag_name, values_from = MAE)
view(mae_results_long)


#===============================================================================
# Granger causality

# (quick check if they are both stationary)
library(tseries)
adf.test(kld_cluster_df$main_kld)
adf.test(kld_cluster_df$cluster_value)

install.packages("vars")
library(vars)

# --- 1. Select the optimal number of lags for the model ---
# This is an important step. VARselect helps you choose based on stats criteria.
# We use the stationary data here (assuming it passed the ADF test).
kld_cluster_df
var_data <- kld_cluster_df %>% dplyr::select(main_kld, cluster_value)

VARselect(var_data, lag.max = 12, type = "const")

# Look at the output for $selection. The AIC or BIC criteria are good choices.
# Let's say it suggests 4 lags.
optimal_lags <- 4 

# --- 2. Run the causality test ---
# The test checks if cluster_value "Granger-causes" main_kld
granger_test <- causality(
  VAR(var_data, p = optimal_lags, type = "const"), 
  cause = "cluster_value"
)

# --- 3. Print and i
cluster_df <- df_codes_to_titles(cluster_df, 
                   question_code_col_name = "question_code",
                   industry_code_col_name = "industry_code")

