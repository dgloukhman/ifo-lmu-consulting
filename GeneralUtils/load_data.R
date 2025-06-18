library("readxl")
library("tidyverse")

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

read_ifo_data <- function(DATA_PATH = "data") {
  filepaths <- list.files(DATA_PATH, pattern = "\\.xlsx$", full.names = TRUE)
  ifo_tbl <- filepaths %>%
    map(.read_single_excel) %>%
    flatten() %>%
    bind_rows()
  ifo_tbl
}

# preprocess ifo data
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
    filter(!industry_code %in% unique(na_rows_df$industry_code))
}

get_level <- function(code) {
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

#ifo_tbl <- read_ifo_data()
