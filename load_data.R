# install.packages("tidyverse")
# install.packages("readxl")
library("readxl")
library("purrr")

.read_single_excel <- function(pathname){
  #Reads in file from pathname and returns a list of tibbles

df <- read_excel(pathname ,skip = 1 )
df <- df %>% slice(-1)
dates <- df[1]
dates <- pull(df,1)
df <- df[-1]
split_tibbles <- map(
  seq(1, ncol(df), by = 15), 
  ~ df[, .x:min(.x+14, ncol(df))]
  ) %>% 
  
  map(.augument_single_tibble, dates=dates)

split_tibbles

}


.augument_single_tibble <- function(tib,dates){
#  adds industry_code and dates to tibble

  cols <- names(tib)
  cols <- str_split(cols,':', simplify = TRUE)
  industry_code <- cols[1]
  if (any(cols[,1] != industry_code)){
    print("Error: industries in columns do not match")
  }
  names(tib) <- cols[,2]

  tib  %>% 
    mutate(
      date = dates,
      industry_code = industry_code
    ) %>% 
    select(date, industry_code, everything())
  
}

read_ifo_data <- function(DATA_PATH = "data"){
  
  filepaths <- list.files('data', full.names = TRUE)  
ifo_tbl <- filepaths %>% 
  map(.read_single_excel) %>% 
  flatten() %>% 
  bind_rows()
ifo_tbl
}

ifo_tbl <- read_ifo_data()
