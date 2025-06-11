library(tidyverse)

# # Load data
# industry_data <- read_csv("Data/industries_codes_titles.csv")

# Function to determine hierarchy level (self-contained)
get_level <- function(code) {
  digits <- substring(code, 2)  # remove leading letter
  if (grepl("[A-Za-z]", digits)) {
    return(NA_integer_)
  }
  # Inline rtrim: remove trailing zeros
  digits <- sub("0+$", "", digits)
  level <- nchar(digits)
  return(as.integer(max(0, level - 1)))
}

### Old Code ###

# # Function to extract prefix used for hierarchy comparison (self-contained)
# get_prefix <- function(code) {
#   core <- substring(code, 2)  # remove leading 'C'
#   sub("0+$", "", core)        # inline rtrim: strip trailing zeros
# }
# 
# # Compute levels and prefixes
# industry_data <- industry_data %>%
#   mutate(
#     level = vapply(industry_code, get_level, integer(1)),
#     prefix = get_prefix(industry_code)
#   )

# # Identify leaf nodes (no other code starts with this prefix and is longer)
# industry_data <- industry_data %>%
#   mutate(
#     is_leaf = !prefix %in%
#       prefix[map_lgl(prefix, function(p) {
#         any(prefix != p & startsWith(prefix, p))
#       })]
#   )
# 
# # Count total number of leaf nodes
# total_leaf_nodes <- industry_data %>%
#   filter(is_leaf) %>%
#   nrow()
# 
# # Print result
# cat("Total number of leaf (last-level) industry codes:", total_leaf_nodes, "\n")

# # ==== Plot 1: Number of codes per level ====
# level_counts <- industry_data %>%
#   count(level)
# 
# ggplot(level_counts, aes(x = factor(level), y = n)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   labs(
#     title = "Number of Industries per Hierarchical Level",
#     x = "Hierarchy Level (1 = Broadest)",
#     y = "Number of Industries"
#   ) +
#   theme_minimal()

# # ==== Plot 2: Number of leaf nodes per level ====
# leaf_counts <- industry_data %>%
#   filter(is_leaf) %>%
#   count(level)
# 
# ggplot(leaf_counts, aes(x = factor(level), y = n)) +
#   geom_bar(stat = "identity", fill = "darkgreen") +
#   labs(
#     title = "Number of Leaf (Last-Level) Industry Codes per Hierarchy Level",
#     x = "Hierarchy Level",
#     y = "Number of Leaf Nodes"
#   ) +
#   theme_minimal()

