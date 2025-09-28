library(here)
source(here("utils","setup_packages.R"))

install_packages_from_file()

source(here("utils","load_data.R"))
source(here("Clustering","clustering_utils.R"))

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

# long format (useful later on) 
ts_long_df <- ts_df %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )

# extract codes of question_variables 
question_vars <- ts_df %>%
  select(-c(date, industry_code)) %>%
  colnames()

# extract C00 kld (main aggregate business climate) 
# as main series and rename to main_kld keep distinguishable
c00_kld <- ifo_tbl %>% 
  filter(industry_code == "C0000000") %>% 
  select(date, KLD) %>% 
  rename(main_kld = KLD)

# Get ccf vector for all series (13 elements, lags ranging from -6 to 6)
ts_ccf_matrix <- get_ccf_matrix(ts_df, c00_kld, question_vars, 6, only_negative_lag = FALSE)

# cluster according to wards method
dist_mat <- dist(as.matrix(ts_ccf_matrix), method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

# min and max months to lag 
lags <- c(4,6)
cluster_sizes <- seq(20,80,by=5)

all_cluster_metrics <- list()
all_cluster_avrgs <- list()
all_cluster_assignments <- list()

# Loop through multiple numbers of clusters
# (considering multiple as a rough indicator to see if there are major differences
#  in resulting clusters performance-wise. Final choice is probably rather a question
#  of desired interpretation) 

for (cluster_size in cluster_sizes) {
  
  # cut clusters according to cluster size and assign to ccf vectors
  ts_clusters <- cutree(hc, cluster_size)
  ts_ccf_matrix_cluster <- ts_ccf_matrix
  ts_ccf_matrix_cluster$cluster <- ts_clusters
  
  # average all values of each cluster
  cluster_avrgs <- get_cluster_avrg(ts_long_df, ts_ccf_matrix_cluster)
  
  # add main_kld column to averages 
  cluster_avrgs$main_kld <-  c00_kld$main_kld
  
  # vector containig cluster columns 
  cluster_cols <- paste0("cluster_",1:cluster_size)
  
  # apply make_lags to create df, where we add columns with lagged versions
  # of all series (main_kld and all clusters)
  df_lagged <- cluster_avrgs %>%
    arrange(.data[["date"]]) %>%
    make_lags(c("main_kld",cluster_cols), lags = lags) %>% 
    drop_na() # removes rows where we have NaNs due to lagging 
  
  # for each cluster, fit lm, 
  #predicting main_kld with lagged values of main_kld and the cluster average
  # store resulting mse, rmse, adj. r2 for each
  metrics <- map_dfr(cluster_cols, ~compare_augmented_to_baseline(.x,lags,data=df_lagged,baseline = FALSE))
  
  # mean of ccf of all series present in the cluster 
  cluster_ccf_summary <- ts_ccf_matrix_cluster %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      across(starts_with("ccf_"), ~mean(.x))) 
  
  # merge metrics into cluster summary
  cluster_metrics_summary <- cbind(cluster_ccf_summary, metrics)
  
  # store in list with all other evaluations with differing numbers of clusters 
  all_cluster_metrics[[paste0("k_", cluster_size)]] <- cluster_metrics_summary
  
  # also store cluster avrgs and cluster asignments (IDs vary with n_cluster)
  all_cluster_avrgs[[paste0("k_", cluster_size)]] <- cluster_avrgs
  all_cluster_assignments[[paste0("k_", cluster_size)]] <- ts_ccf_matrix_cluster %>%
                                                            dplyr::select(cluster)
}

# quickly look at metrics of model using only Y
compare_augmented_to_baseline(data = df_lagged, lags = lags,baseline = TRUE)

# look at min mse of all differing numbers of clusters 
mins <- sapply(all_cluster_metrics, function(df) min(df$mse))
mins

# choose cluster size to look at at select metrics 
cluster_size <- 60
cluster_metrics <- all_cluster_metrics[[paste0("k_", cluster_size)]]
cluster_assignment <- all_cluster_assignments[[paste0("k_", cluster_size)]]
cluster_avrgs <- all_cluster_avrgs[[paste0("k_",cluster_size)]]

top_clusters <- cluster_metrics %>% arrange(mse) %>% slice(1:5)

plot_ccfs_cluster(top_clusters, show_legend = FALSE)


#===============================================================================
# Taking a closer look at a single cluster
# choose cluster id 
cluster_id <- 55

# extract df containing (question, industry) pairs 
cluster_components_df <- cluster_assignment %>% filter(cluster == cluster_id) %>%
  rownames_to_column(var = "id") %>%
  separate(id, into = c("industry_code", "question_code"), sep = "_") %>% 
  dplyr::select(industry_code, question_code)

# Plot all series in cluster with main index
plot_cluster_with_main(ifo_tbl, cluster_components_df)

# Plot average of all series in cluster with main index
cluster_index <- cluster_avrgs[[paste0("cluster_", cluster_id)]]
plot_cluster_avrg_with_main(c00_kld, cluster_index, show_legend = TRUE)

# Convert the codes within the cluster to titles and take a look at its actual components 
print(df_codes_to_titles(cluster_components_df, 
                         question_code_col_name = "question_code",
                         industry_code_col_name = "industry_code"))

