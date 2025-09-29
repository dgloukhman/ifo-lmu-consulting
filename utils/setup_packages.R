library(here)
install_packages_from_file <- function(file_path = here("packages.txt"), load = TRUE) {
  if (!file.exists(file_path)) {
    stop("Package list file not found: ", file_path)
  }

  required_packages <- scan(file_path, what = character(), sep = "\n", quiet = TRUE)
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  } else {
    message("All packages already installed.")
  }

  if (load) {
    invisible(lapply(required_packages, require, character.only = TRUE))
  }
}
