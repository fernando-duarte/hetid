#' Download NY Fed ACM Term Premia Data
#'
#' Downloads the Adrian, Crump, and Moench (ACM) term premia data from the
#' New York Fed website and saves it as a CSV file in the package's data directory.
#'
#' @param force Logical. If TRUE, forces re-download even if data exists. Default is FALSE.
#' @param quiet Logical. If TRUE, suppresses download progress messages. Default is FALSE.
#'
#' @return Invisibly returns the path to the saved CSV file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the data (only if not already present)
#' download_term_premia()
#'
#' # Force re-download
#' download_term_premia(force = TRUE)
#' }
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013).
#' "Pricing the term structure with linear regressions."
#' Journal of Financial Economics, 110(1), 110-138.
#'
download_term_premia <- function(force = FALSE, quiet = FALSE) {
  # Use academic data source URL from constants
  url <- DATA_URLS$ACM_TERM_PREMIA

  # Use standardized path management
  validate_data_directory(create_if_missing = TRUE)
  csv_path <- get_acm_data_path()

  # Check if file already exists
  if (file.exists(csv_path) && !force) {
    if (!quiet) {
      message("Term premia data already exists. Use force = TRUE to re-download.")
    }
    return(invisible(csv_path))
  }

  # Download to temporary file
  temp_xls <- tempfile(fileext = ".xls")

  if (!quiet) {
    message("Downloading ACM term premia data from NY Fed...")
  }

  tryCatch(
    {
      download.file(
        url = url,
        destfile = temp_xls,
        mode = "wb",
        quiet = quiet
      )

      # Check if readxl is available
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop(
          "Package 'readxl' is required to read Excel files. ",
          "Please install it with: install.packages('readxl')"
        )
      }

      # Read Excel file
      if (!quiet) {
        message("Converting Excel to CSV...")
      }

      df <- readxl::read_excel(temp_xls)

      # Save as CSV
      write.csv(df, csv_path, row.names = FALSE)

      if (!quiet) {
        message("Term premia data saved to: ", csv_path)
        message("Data dimensions: ", nrow(df), " rows x ", ncol(df), " columns")
      }

      # Clean up temp file
      unlink(temp_xls)

      return(invisible(csv_path))
    },
    error = function(e) {
      # Clean up on error
      if (file.exists(temp_xls)) {
        unlink(temp_xls)
      }
      stop("Failed to download term premia data: ", e$message)
    }
  )
}
