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

# --------------------------------------------------------------------
# Function: get_level
# Purpose:  Classify an industry code into a hierarchical level
# Arguments:
#   - code: Single string (e.g., "C1000000") representing an industry code
# Returns:
#   - Integer: Hierarchy level (0 for root, 1+ for sublevels)
#              Returns NA if code is invalid (e.g., contains letters in numeric part)
# --------------------------------------------------------------------
get_level <- function(code) {
  # Remove the leading character (typically 'C')
  digits <- substring(code, 2)
  
  # If any non-numeric characters are present after stripping → invalid code
  if (grepl("[^0-9]", digits)) return(NA_integer_)
  
  # Strip all trailing zeros which act as padding (not level-defining)
  digits <- sub("0+$", "", digits)
  
  # If nothing remains after stripping → it's the root level (e.g., "C0000000")
  if (digits == "") return(0L)
  
  # Count remaining digits: e.g., "1" → len = 1, → Level = max(1, 1 - 1) = 1
  len <- nchar(digits)
  
  # Compute level: at least Level 1 if any digits remain, subtract 1 for offset
  return(max(1L, len - 1L))
}