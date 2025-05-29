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
ts <- C00_subset$KLD # KLD = Business Climate

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

  
#--------------------- Adding/Comparing with signals from Markov Switching -------------

# define function to return markov regime switching probabilities and define signals (one-third/two-thid rule, like in ifo handbook)
# Added AR option, but noticed that estimation was super unstable, so sticking for intercept-only for now   
ms_turning_points <- function(ts, dates, smooth_probs = FALSE, AR = FALSE) {
    
    if(!AR){
      # simple model with intercept only
      lm <- lm(ts ~ 1)
      
      sw <- c(TRUE, FALSE) # vector for msm, allowing mean to change but constant variance
    } else {
      y_lag1 <- ts[1:(length(ts)-1)] # lagged version, removing last (most recent) momth
      ts <- ts[-1] #removing first row/month
      
      lm <- lm(ts ~ y_lag1)
      dates <- dates[-1] #also remove first row of dates
      
      sw <- c(TRUE, FALSE, FALSE)
    }
    # fit markov switching model
    msm_model <- MSwM::msmFit(lm, 
                              k = 2,       # assuming two states
                              sw = sw)
    # extract index of bigger regime (expansion)
    exp_regime_index <- which.max(msm_model@Coef[["(Intercept)"]])
    
    # extract smooth or filtered probabilities 
    if(smooth_probs){
      probs <- msm_model@Fit@smoProb[,exp_regime_index]
      probs <- probs[-1] #drop first line (MSwM adds first prob for some reason)
    } else {
      probs <-  msm_model@Fit@filtProb[,exp_regime_index]
    }
    
    # define function to mark turning points and merge into df (below 1/3 -> contraction, above 2/3 -> Expansion, rest is uncertain)
    mark_turningpoints <- function(probabilities) {
      df <- data.frame(prob = probabilities) %>%
        mutate(
          prev_value = lag(prob, default = first(prob)), # helper column previous values 
          phase = case_when(
            prob > 0.66 ~ "expansion",
            prob < 0.33 ~ "contraction",
            TRUE ~ "uncertain" 
          ),
          prev_status = case_when(
            prev_value > 0.66 ~ "expansion",
            prev_value < 0.33 ~ "contraction",
            TRUE ~ "uncertain"
          ),
          turning_point = case_when(
            (phase == "expansion" & prev_status != "expansion") ~ TRUE,
            (phase == "contraction" & prev_status != "contraction") ~ TRUE,
            TRUE ~ FALSE
          )
        ) %>%
        select(-prev_value, -prev_status)  # clean up intermediate columns
      
      return(df)
    }
    
    tp_df <- mark_turningpoints(probs)
    tp_df$ts <- ts
    tp_df$date <- dates

    return(tp_df)
  }

# Apply to C00 with smoothed probabilites (meaning that all data is used -> Setting: Identifying turning points in hindsight)  
markov_df <- ms_turning_points(ts = C00_subset$KLD, dates = C00_subset$date, smooth_probs = TRUE, AR = FALSE)

# merge markov and BB turning points into one df, with type indicating which one 
tp_plot_df <- markov_df %>%
  filter(turning_point == TRUE) %>%
  rename(KLD = ts)  %>%
  select(-prob,-turning_point) %>%
  mutate(type = "Markov Switching") %>%
  rbind(turning_points %>% mutate(type = "Bry-Boschan"))


# Plot together with Business Climate Graph
ggplot(C00_subset, aes(x = date, y = KLD)) +
  geom_line() + 
  geom_point(data = tp_plot_df, aes(x = date, y = KLD, color = interaction(phase, type)), size = 3) + #(interaction combines phase and type for color indication)
  scale_color_manual(
    values = c(
      "expansion.Bry-Boschan" = "#4CBB17",
      "contraction.Bry-Boschan" = "#FF2400",
      "expansion.Markov Switching" = "#708238",
      "contraction.Markov Switching" = "#7C0A02"
    ),
    labels = c(
      "expansion.Bry-Boschan" = "Bry-Boschan Expansion",
      "contraction.Bry-Boschan" = "Bry-Boschan Contraction",
      "expansion.Markov Switching" = "Markov Expansion",
      "contraction.Markov Switching" = "Markov Contraction"
    ),
    name = "Turning Points"
  ) +
  labs(
    x = "",
    y = "Manufacturing Business Climate"
  ) +
  theme_minimal()




  