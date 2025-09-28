# Idea: Iterate through possible cluster sizes and store resulting cluster_metrics_summary for each size
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

# subset to three-digits
ts_df <- ifo_tbl %>% 
  filter(level == 2) %>% 
  select(-level)

# replacing with first diffs
ts_df <- ts_df %>%
  group_by(industry_code) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(across(where(is.numeric), ~ .x - lag(.x))) %>%
  ungroup() %>% 
  drop_na()

# replacing long with first diffs
ts_long_df <- ts_df %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )


# extract C00 kld as main series and rename to keep distinguishable
c00_kld <- ifo_tbl %>% 
  filter(industry_code == "C0000000") %>% 
  select(date, KLD) %>% 
  rename(main_kld = KLD)

c00_kld <- c00_kld %>%
  mutate(across(where(is.numeric), ~ .x - lag(.x))) %>% 
  drop_na()

cluster_sizes <- c(20,25,30,35,40,45,50,55,60)
lags <- c(4,6)

all_cluster_avrgs <- list()
all_cluster_metrics <- list()


# Loop through cluster_sizes, determine ccf, cluster by ccf vector, take average of each cluster, evaluate 
for (cluster_size in cluster_sizes) {
  
  # Get ccf vector for all series (13 elements, lags ranging from -6 to 6)
  ts_ccf_matrix <- get_ccf_matrix(ts_df, c00_kld, question_vars, 6, only_negative_lag = FALSE)
  
  # take euclidean distance and cluster 
  dist_mat <- dist(as.matrix(ts_ccf_matrix), method = "euclidean")
  hc <- hclust(dist_mat, method = "ward.D2")
  
  # cut clusters according to cluster size and assign to ccf vectors
  ts_clusters <- cutree(hc, cluster_size)
  ts_ccf_matrix$cluster <- ts_clusters
  
  # average all values of each cluster
  cluster_avrgs <- get_cluster_avrg(ts_long_df, ts_ccf_matrix)
  
  # add main_kld column to averages 
  cluster_avrgs$main_kld <-  c00_kld$main_kld
  
  # vector containig cluster columns 
  cluster_cols <- paste0("cluster_",1:cluster_size)
  
  # create df where we add columns lagged versions of all series
  df <- cluster_avrgs %>%
    arrange(.data[["date"]]) %>%
    make_lags(c("main_kld",cluster_cols), lags = lags) %>% 
    drop_na()
  
  # for each cluster, fit lm, predicting main_kld with lagged values of main_kld and the cluster average
  # store resulting mae, mse, r2 and so on for each
  res <- map_dfr(cluster_cols, ~compare_augmented_to_baseline(.x,lags,baseline = FALSE))
  
  # store mean ccf of series in the cluster to see its characteristics
  cluster_ccf_summary <- ts_ccf_matrix %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      across(starts_with("ccf_"), ~mean(.x))) 
  
  # merge metric into cluster summary
  cluster_metrics_summary <- cbind(cluster_ccf_summary, res)
  
  # store in list with all other evaluations with differing numbers of clusters 
  all_cluster_metrics[[paste0("k_", cluster_size)]] <- cluster_metrics_summary
  
  # also store cluster avrgs (IDs change with varying clustering size)
  all_cluster_avrgs[[paste0("k_", cluster_size)]] <- cluster_avrgs
}

# quickly look at metrics of model using only Y 
compare_augmented_to_baseline(cluster_name = "cluster_1", lags = c(4,6),baseline = TRUE)


# Find which dataframe has the overall minimum
mins <- sapply(all_cluster_metrics, function(df) min(df$mse))
mins

all_cluster_metrics$k_60 %>% arrange(mse)
view(all_cluster_metrics$k_35)
#===============================================================================
# Taking a closer look at a single cluster 

cluster_id <- 30
cluster_components_df <- ts_ccf_matrix %>% filter(cluster == cluster_id) %>%
  rownames_to_column(var = "id") %>%
  separate(id, into = c("industry_code", "question_code"), sep = "_") %>% 
  dplyr::select(industry_code, question_code)

# plot leveled series (non-diffs) against main
plot_cluster_with_main(ifo_tbl, cluster_components_df)

# look at cluster average 
cluster_index <- cluster_avrgs[[paste0("cluster_", cluster_id)]]
kld_cluster_df <- cbind(c00_kld, cluster_index)

ggplot(kld_cluster_df, aes(x = date)) +
  geom_line(aes(y = main_kld, color = "main_kld")) +
  geom_line(aes(y = cluster_index, color = "cluster_value")) +
  labs(title = "Main KLD vs Cluster Value over Time",
       x = "Date", y = "Value", color = "Series") +
  theme_minimal()

print(df_codes_to_titles(cluster_components_df, question_code_col_name = "question_code", industry_code_col_name = "industry_code"))
