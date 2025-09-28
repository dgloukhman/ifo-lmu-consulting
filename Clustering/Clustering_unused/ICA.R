# Install if you haven't already
 #install.packages("fastICA")
 #install.packages("zoo") # For na.locf

# script to run ICA, check out multiple number of components and see if any of them indicate well in a similar fashion as the clustering based on ccf 
library(fastICA)
#library(zoo) # For handling missing values

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


# long format, useful later on 
ts_long_df <- ts_df %>%
  pivot_longer(
    cols = -c(date, industry_code),
    names_to = "question_code",
    values_to = "value"
  )

# extract question_variables
question_vars <- ts_df %>%
  select(-c(date, industry_code)) %>%
  colnames()

# extract C00 kld as main series and rename to keep distinguishable
c00_kld <- ifo_tbl %>% 
  filter(industry_code == "C0000000") %>% 
  select(date, KLD) %>% 
  rename(main_kld = KLD)
 
# Rows = time points, Columns = series
ts_wide_matrix <- ts_long_df %>%
  select(date, industry_code, question_code, value) %>%
  unite("id", industry_code, question_code, sep = "_") %>%
  # Pivot to wide format
  pivot_wider(names_from = id, values_from = value) %>%
  arrange(date) %>%
  select(-date) %>%
  as.matrix()

# number of components
n_comps <- seq(10,100,by = 5)

all_ica_metrics <- list()
all_ica_components <- list()
all_ica_results <- list()

# loop through number of components and get similar metrics as with ccf clusters
for (n_comp in n_comps) {
  ica_result <- fastICA(
    ts_wide_matrix, 
    n.comp = n_comp, # Number of components to extract
    method = "C" # Use the C implementation for speed
  )
  
  independent_components <- as.data.frame(ica_result$S)
  ica_colnames <- paste0("ica_comp_",1:n_comp)
  colnames(independent_components) <- ica_colnames
  
  dates <- ifo_tbl %>% distinct(date) %>% arrange(date)
  components_df <- bind_cols(date = dates, independent_components, main_kld = c00_kld$main_kld)
  
  
  df <- components_df %>%
    arrange(.data[["date"]]) %>%
    make_lags(c("main_kld",ica_colnames), lags = lags) %>% 
    drop_na()
  
  res <- map_dfr(ica_colnames, ~compare_augmented_ica_to_baseline(.x,lags,baseline = FALSE))
  
  ica_ccf_summary <- ica_get_ccf_matrix(components_df, lag.max = 6)
  
  all_ica_metrics[[paste0("n_comp_", n_comp)]] <- left_join(res, ica_ccf_summary, by = "component")
  all_ica_components[[paste0("n_comp_", n_comp)]] <- components_df
  all_ica_results[[paste0("n_comp_", n_comp)]] <- ica_result
}

# look at number of components with best mse
mins <- sapply(all_ica_metrics, function(df) min(df$mse))
mins

# look which component has best mse
all_ica_metrics$n_comp_35 %>% arrange(mse)

# create df containing contribution of all of these 
comp_contribution <- all_ica_results$n_comp_35$A[26,]
comp_names <- colnames(ts_wide_matrix)
compcontribution_df <-  data.frame(contribution = comp_contribution, comp = comp_names)

# extract industres and questions
comp_elements_df <- compcontribution_df %>%arrange(desc(contribution)) %>%  separate(comp, into = c("industry_code", "question_code"), sep = "_") %>% slice(1:10)

# extract series of this components
component_ts <- all_ica_components$n_comp_35$ica_comp_26
kld_component_df <- cbind(c00_kld, component_ts)


ggplot(kld_component_df, aes(x = date)) +
  geom_line(aes(y = main_kld, color = "main_kld")) +
  geom_line(aes(y = component_ts, color = "component")) +
  labs(title = "Main KLD vs Component over Time",
       x = "Date", y = "Value", color = "Series") +
  theme_minimal()

print(df_codes_to_titles(comp_elements_df, question_code_col_name = "question_code", industry_code_col_name = "industry_code"))

  

# --- 4. Plot one of the components to see what it looks like ---
# Let's look at the first component "V1"

ggplot(all_ica_components$n_comp_70, aes(x = date, y = ica_comp_1)) +
  geom_line() +
  labs(title = "Independent Component 1", y = "Signal Strength") +
  theme_minimal()
