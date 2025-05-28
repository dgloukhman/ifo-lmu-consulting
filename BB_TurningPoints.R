source("load_data.R")
source("summary_tools.R")
source("visualization.R")

# load in data and preprocess
data_path <- '/Users/jakobfreytag/Desktop/R Directionaries/ifo Time Series/Data'
data_path_dict <- '/Users/jakobfreytag/Desktop/R Directionaries/ifo Time Series/Data/'
ifo_tbl <- read_ifo_data(data_path)
ifo_tbl <- preprocess_ifo_data(ifo_tbl)

# define function to return BB turning points 
get_bb_turning_points <- function(values, dates, mincycle = 24, minphase = 6) {
  library(BCDating)
  
  # get start and end to turn into ts object (requirement for BCDating package)
  start_year <- as.numeric(format(min(dates), "%Y"))
  start_month <- as.numeric(format(min(dates), "%m"))
  end_year <- as.numeric(format(max(dates), "%Y"))
  end_month <- as.numeric(format(max(dates), "%m"))
  
  ts_data <- ts(values, start = c(start_year, start_month), frequency = 12, end = c(end_year, end_month))
  
  # run Bry-Boschan 
  bb <- BCDating::BBQ(ts_data, mincycle = mincycle, minphase = minphase)
  
  # extract months of identified peaks and throughs 
  tp_exp <- C00_subset$date[bb@peaks]
  tp_contr <- C00_subset$date[bb@troughs]
  
  return(list(tp_expansion = tp_exp,tp_contraction = tp_contr))
}

# subset to main aggregate (whole manufacturing)
C00_subset <- subset(ifo_tbl, ifo_tbl$industry_code == 'C0000000')
ts <- C00_subset$KLD

# apply function and extract expansion and contraction points (dates of them )
bb_turning_points <- get_bb_turning_points(values = C00_subset$KLD, dates = C00_subset$date)
tp_exp <- bb_turning_points$tp_expansion
tp_contr <- bb_turning_points$tp_contraction

# create df with turning points to plot them along the climate graph 
turning_points <- C00_subset %>% 
  mutate(phase = case_when(
    date %in% tp_exp ~ "expansion",
    date %in% tp_contr ~ "contraction",
    TRUE ~ NA_character_
  )) %>%
  filter(date %in% c(tp_exp, tp_contr)) %>% 
  select(date, KLD, phase)

# Plot climate graph with turning points along the line
ggplot(C00_subset, aes(x = date, y = KLD)) +
  geom_line() + 
  geom_point(data = turning_points, aes(x = date, y = KLD, color = phase), size = 2) +  
  scale_color_manual(values = c("expansion" = "darkgreen", "contraction" = "darkred")) +
  labs(x = "", y = "Manufacturing Business Climate", color = "Bry-Boschan Turning Points")
  theme_minimal()
