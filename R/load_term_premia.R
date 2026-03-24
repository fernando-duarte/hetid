#' Load ACM Term Premia Data
#'
#' Loads the ACM term premia data from the package's data directory.
#' If the data doesn't exist, prompts the user to download it first.
#'
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it. Default is FALSE.
#'
#' @return A data frame containing the term premia data, or NULL if data is not available.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the data (will prompt to download if not available)
#' term_premia <- load_term_premia()
#'
#' # Automatically download if not available
#' term_premia <- load_term_premia(auto_download = TRUE)
#' }
#'
#' @seealso \code{\link{download_term_premia}} for downloading the data
#'
load_term_premia <- function(auto_download = FALSE) {
  # Use standardized path management
  csv_path <- get_acm_data_path()

  # Check if file exists
  if (!check_data_file_exists("ACMTermPremium.csv")) {
    if (auto_download) {
      message("Term premia data not found. Downloading...")
      download_term_premia()
      # Update path after download
      csv_path <- get_acm_data_path()
    } else {
      message("Term premia data not found. Please run download_term_premia() first.")
      return(NULL)
    }
  }

  # Load the data
  tryCatch(
    {
      tp_df <- read.csv(csv_path, stringsAsFactors = FALSE)

      # Convert DATE column to Date class if it exists
      if ("DATE" %in% names(tp_df)) {
        date_formats <- list(
          HETID_CONSTANTS$ACM_DATE_FORMAT,
          NULL,
          HETID_CONSTANTS$ISO_DATE_FORMAT
        )
        for (fmt in date_formats) {
          parsed <- tryCatch(
            if (is.null(fmt)) {
              as.Date(tp_df$DATE)
            } else {
              as.Date(tp_df$DATE, format = fmt)
            },
            error = function(e) NULL
          )
          if (!is.null(parsed) && !all(is.na(parsed))) {
            tp_df$DATE <- parsed
            break
          }
        }
        if (!inherits(tp_df$DATE, "Date")) {
          warning(
            "Could not convert DATE column to ",
            "Date class. Keeping as character."
          )
        }
      }

      # Standardize column name to lowercase 'date'
      if ("DATE" %in% names(tp_df)) {
        names(tp_df)[names(tp_df) == "DATE"] <- "date"
      }

      tp_df
    },
    error = function(e) {
      warning("Failed to load term premia data: ", e$message)
      NULL
    }
  )
}
