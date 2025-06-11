library("tidyverse")
library("ggplot2")
source("GeneralUtils/setup_packages.R")
install_packages_from_file()

### 0. Data Preparattion ###
source("GeneralUtils/load_data.R")
source("GeneralUtils/structure_analysis.R")
ifo_tbl <- read_ifo_data()
ifo_tbl <- ifo_tbl %>% preprocess_ifo_data()

# Compute levels and prefixes
ifo_tbl <- ifo_tbl %>%
  mutate(
    level = vapply(industry_code, get_level, integer(1))
  )

# Filter on Level 0 and Level 2
ifo_tbl_l2 <- ifo_tbl %>%
  filter(level %in% c(0, 2)) %>%
  select(date, industry_code, KLD, level)

# Wide format 
ifo_tbl_l2_wide <- ifo_tbl_l2 %>% select(-level) %>%
  pivot_wider(names_from = industry_code, values_from = KLD)

### 1. Analyze Lag Structure ###
# - Anylaze Lag Structure between main index and level 2 compound indicies
# - Analyze General Lag Structure and Turning Point Lag Structure
# - Short-term and long-term lag analysis

## Set Up 
main_index <- "C0000000"
l2_targets <- setdiff(names(ifo_tbl_l2_wide), c("date", main_index))
max_lag <- 24

## Cross-Correlation 
# Function to compute full lag correlation for one sector
get_ccf_full <- function(target_code) {
  x <- ifo_tbl_l2_wide[[target_code]]
  y <- ifo_tbl_l2_wide[[main_index]]
  
  # Remove NAs
  non_na_idx <- complete.cases(x, y)
  x <- x[non_na_idx]
  y <- y[non_na_idx]
  
  # Compute CCF
  ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
  
  tibble(
    lag = ccf_obj$lag,
    correlation = ccf_obj$acf,
    industry_code = target_code
  )
}

# Build full tibble
ccf_full_tbl <- map_dfr(l2_targets, get_ccf_full)

# Extract peak lead/lag per Industry
ccf_peak_tbl <- ccf_full_tbl %>%
  group_by(industry_code) %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(peak_lag = lag, peak_corr = correlation)

## Visualization

# Sort full ccf tbl
ccf_full_sorted <- ccf_full_tbl %>%
  left_join(ccf_peak_tbl, by = "industry_code") %>%
  arrange(peak_lag, industry_code) %>%
  mutate(industry_code = factor(industry_code, levels = unique(industry_code)))

# Create markers for heatmap
ccf_peak_markers <- ccf_peak_tbl %>%
  mutate(industry_code = factor(industry_code, levels = levels(ccf_full_sorted$industry_code)))

# Plot heatmap
ggplot(ccf_full_sorted, aes(x = industry_code, y = lag, fill = correlation)) +
  geom_tile() +  # Base heatmap
  geom_tile(data = ccf_peak_markers, aes(x = industry_code, y = peak_lag),
            color = "yellow", fill = NA, linewidth = 0.8, width = 0.95, height = 0.95, inherit.aes = FALSE) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "CCF Heatmap: Highlighted Peak Correlations",
       x = "Industry Code (sorted by peak lag)", y = "Lag (months)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

### 2. Model main index with level 2 compound indicies
# - General model 
# - Model based on Leading Indices only 
# - Model based on Leading + Coinciding Indices (Regression on Residuals)
# - Full Prediction and only Turning Point Prediction



### 3. Refining Model 
# - Select most important indices (LASSO/Elastic-Net) 
# - Allow reweighting of compound indices (Grouped Loss)
# - Model Turning Point Prediction and Normal Prediction separatly 
#   (Variance based Band Filter or Markov-Switching based Filter)
