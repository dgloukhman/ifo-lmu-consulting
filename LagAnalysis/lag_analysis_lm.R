# ====================================================================
# General

# --------------------------------------------------------------------
# Installs necessary packages

source("utils/setup_packages.R")
install_packages_from_file()

# --------------------------------------------------------------------
# Necessary libaries

library("tidyverse")
library("tsibble")
library("tseries")
library("ggplot2")
library("feasts")

# --------------------------------------------------------------------
# Data Preparation

source("utils/load_data.R")


# Read Data
ifo_tbl <- read_ifo_data() %>%
  preprocess_ifo_data()


# Create tsibble
ifo_tsbl <- ifo_tbl %>% as_tsibble(key = industry_code, index = date)

# Set Main Index
main_index <- "C0000000"

# ====================================================================
# Setup

# --------------------------------------------------------------------
# Level 2

# Filter Level 0 and 2 for sector aggregates
ifo_tsbl_l2 <- ifo_tsbl %>%
  filter(level %in% c(0, 2)) %>%
  select(date, industry_code, KLD)

# Transform to wide format
ifo_tsbl_l2_wide <- ifo_tsbl_l2 %>%
  pivot_wider(names_from = industry_code, values_from = KLD)

# ====================================================================
# Lagged Regression / Transfer Function Models
