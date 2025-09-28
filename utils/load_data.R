library("readxl")
library("tidyverse")

# ====================================================================
# Function: .read_single_excel
# Description:
#   Reads a single Excel file and splits it into multiple tibbles.
#   Each tibble corresponds to a group of 15 columns, augmented
#   with industry codes and dates.
# Arguments:
#   pathname : Filepath to the Excel file
# Returns:
#   A list of tibbles containing structured data
# ====================================================================
.read_single_excel <- function(pathname) {
  #Reads in file from pathname and returns a list of tibbles

  df <- read_excel(pathname, skip = 1)
  df <- df %>% slice(-1)
  dates <- df[1]
  dates <- pull(df, 1)
  df <- df[-1]
  split_tibbles <- map(
    seq(1, ncol(df), by = 15),
    ~ df[, .x:min(.x + 14, ncol(df))]
  ) %>%

    map(.augument_single_tibble, dates = dates)

  split_tibbles
}

# ====================================================================
# Function: .augument_single_tibble
# Description:
#   Enhances a tibble by:
#     - Extracting and validating the industry code from column names
#     - Renaming columns to question codes only
#     - Adding corresponding dates and industry_code as columns
# Arguments:
#   tib   : Tibble containing raw question data (15 columns per industry)
#   dates : Vector of dates corresponding to rows
# Returns:
#   A tibble with added `date` and `industry_code` columns
# ====================================================================
.augument_single_tibble <- function(tib, dates) {
  #  adds industry_code and dates to tibble

  cols <- names(tib)
  cols <- str_split(cols, ':', simplify = TRUE)
  industry_code <- cols[1]
  if (any(cols[, 1] != industry_code)) {
    print("Error: industries in columns do not match")
  }
  names(tib) <- cols[, 2]

  tib %>%
    mutate(
      date = dates,
      industry_code = industry_code
    ) %>%
    select(date, industry_code, everything())
}

# ====================================================================
# Function: read_ifo_data
# Description:
#   Reads and processes all Excel files in a given directory.
#   Applies .read_single_excel to each file and combines results.
# Arguments:
#   DATA_PATH : Directory path containing .xlsx files (default = "data")
# Returns:
#   A tibble with all industries, questions, and dates stacked together
# ====================================================================
read_ifo_data <- function(DATA_PATH = "data") {
  filepaths <- list.files(DATA_PATH, pattern = "\\.xlsx$", full.names = TRUE)
  ifo_tbl <- filepaths %>%
    map(.read_single_excel) %>%
    flatten() %>%
    bind_rows()
  ifo_tbl
}

# ====================================================================
# Function: preprocess_ifo_data
# Description:
#   Preprocesses IFO raw data by:
#     - Converting date strings into proper Date objects
#     - Cleaning column names (removing '§BDS' suffix)
#     - Ensuring all values are numeric
#     - Dropping specific unwanted columns (BES, PRS)
#     - Filtering out incomplete industries (NaN values)
#     - Adding hierarchy level for each industry code
# Arguments:
#   df : Raw IFO tibble from read_ifo_data()
# Returns:
#   A cleaned tibble with standardized dates and hierarchical levels
# ====================================================================
preprocess_ifo_data <- function(df) {
  df <- df %>%
    # Convert to proper Date format, adding 01 as a day to every month
    mutate(
      date = as.Date(paste0("01/", date), format = "%d/%m/%Y")
    ) %>%

    # Remove '§BDS' from the end of the last 15 column names
    rename_with(
      ~ sub("§BDS$", "", .x),
      .cols = tail(names(df), 15)
    ) %>%

    # make sure all values are numeric
    mutate(across(tail(names(.), 15), as.numeric))

  df <- df %>% select(-BES, -PRS)

  na_rows_df <- df[apply(is.na(df), 1, any), ]

  print('Preprocessing')
  print('Filtered out all subaggregates with NaN values: (Temporary)')
  print(unique(na_rows_df$industry_code))

  df <- df %>%
    filter(!industry_code %in% unique(na_rows_df$industry_code)) %>%
    mutate(level = vapply(industry_code, get_level, integer(1)))
}


# ====================================================================
# Hierarchy Utility: Determine Level of Industry Code
# Description:
#   Extracts the hierarchical level from standardized industry codes.
#   The industry code format assumes a leading character (e.g., 'C'),
#   followed by digits where each level is encoded in digit precision.
#   Example: C0000000 = Level 0, C1000000 = Level 1, C1100000 = Level 1
#            C1110000 = Level 2, etc.
#   Input:  Character vector of industry codes (e.g., "C1000000")
#   Output: Integer indicating hierarchy level (0 = root level)
# ====================================================================
get_level <- function(code) {
  # Remove the leading character (typically 'C')
  digits <- substring(code, 2)

  # If any non-numeric characters are present after stripping → invalid code
  if (grepl("[^0-9]", digits)) {
    return(NA_integer_)
  }

  # Strip all trailing zeros which act as padding (not level-defining)
  digits <- sub("0+$", "", digits)

  # If nothing remains after stripping → it's the root level (e.g., "C0000000")
  if (digits == "") {
    return(0L)
  }

  # Count remaining digits: e.g., "1" → len = 1, → Level = max(1, 1 - 1) = 1
  len <- nchar(digits)

  # Compute level: at least Level 1 if any digits remain, subtract 1 for offset
  return(max(1L, len - 1L))
}

# ====================================================================
# Function: get_industry_dict_df
# Description:
#   Extracts industry and/or question dictionaries from Excel metadata.
#   Useful for mapping codes to human-readable titles.
# Arguments:
#   data_path : Directory containing Excel files
#   questions : If TRUE, returns question dictionary
#               If FALSE, returns industry dictionary
# Returns:
#   Tibble with either (industry_code, industry_title) or
#   (question_code, question_title) pairs
# ====================================================================
get_industry_dict_df <- function(data_path, questions = FALSE) {
  # Function retuns df with industries and according industry codes.
  # Returns similar df with question titles and codes, if questions = TRUE
  # Useful for plotting or own overview

  # collect paths to all excels in Data file
  filepaths <- list.files(data_path, full.names = TRUE)

  # Function to extract code-title pairs from one Excel file
  .extract_pairs <- function(file) {
    # Read just the first 3 rows (suppressing Messages, due to weird output when assigning colnames)
    df <- suppressMessages(read_excel(
      file,
      range = cell_rows(2:3),
      col_names = FALSE
    ))

    # Only take columns from the second one onwards
    codes <- df[1, -1]
    titles <- df[2, -1]

    # Create a tibble of code-title pairs with filename
    tibble(
      file = basename(file),
      code = as.character(unlist(codes)),
      title = as.character(unlist(titles))
    )
  }

  # apply function to all excel files and create tiddle
  all_pairs <- map_dfr(filepaths, .extract_pairs)

  # split into industry code & title and question code & title
  code_title_tdl <- all_pairs %>%
    separate(code, into = c("industry_code", "question_code"), sep = ":") %>%

    separate(
      title,
      into = c("question_title", "industry_title"),
      sep = "\\s*(\\(D\\)|\\(S\\))\\s*",
      remove = FALSE,
      extra = "merge"
    ) %>%

    select(-title)

  if (questions == TRUE) {
    question_df <- code_title_tdl %>%
      select(question_code, question_title) %>%
      distinct()

    question_df$question_code <- sub("§BDS", "", question_df$question_code)
    return(question_df)
  }

  # Extract unique pairs of industry code and industry title
  industries <- code_title_tdl %>%
    select(industry_code, industry_title) %>%
    distinct()

  industries$industry_title <- sub(" BD SBR", "", industries$industry_title)
  industries
}

# ====================================================================
# Function: load_industry_code_map
# Description:
#   Loads industry code → industry title dictionary from CSV.
#   Creates a named vector for fast lookups.
# Arguments:
#   None (expects file: data/industries_codes_titles.csv)
# Returns:
#   Named vector: names = industry_code, values = industry_title
# ====================================================================
load_industry_code_map <- function() {
  i_map <- read_csv("data/industries_codes_titles.csv")
  i_map <- setNames(i_map$industry_title, i_map$industry_code)
  i_map
}

# ====================================================================
# Function: load_question_map
# Description:
#   Loads question code → question title dictionary from CSV.
#   Creates a named vector for fast lookups.
# Arguments:
#   None (expects file: data/questions_codes_titles.csv)
# Returns:
#   Named vector: names = question_code, values = question_title
# ====================================================================
load_question_map <- function() {
  q_map <- read_csv("data/questions_codes_titles.csv")
  q_map <- setNames( q_map$question_title, q_map$question_code)
  q_map
}

# ------------------------------------------------------------------------------
# Function: df_codes_to_titles
# Purpose: 
#.  Receives df and changes entries of codes to actual titles, using 
#   'question_df' and 'industries_df' as a dictionary to translate (need to implement)
# Arguments:
#. - df                     : df to change codes of 
#. - data_path.             : data_path to 'question_df' and 'industries_df'
#. - question_code_col_name : name of column containing question codes
#. - industry_code_col_name : name of column containing industry codes
# ------------------------------------------------------------------------------
df_codes_to_titles <- function(df, data_path, 
                               question_code_col_name = "question_code", 
                               industry_code_col_name = "industry_code") {
  df <- df %>%
    rename(question_code = all_of(question_code_col_name)) %>%
    left_join(question_df, by = "question_code") %>%
    select(-question_code) %>%
    relocate(question_title, .before = everything())
  
  df <- df %>% 
    rename(industry_code = all_of(industry_code_col_name) ) %>%
    left_join(industries_df, by = "industry_code") %>%
    select(-industry_code) %>%
    relocate(industry_title, .before = everything())
}
