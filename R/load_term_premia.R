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
      df <- read.csv(csv_path, stringsAsFactors = FALSE)

      # Convert DATE column to Date class if it exists
      if ("DATE" %in% names(df)) {
        # Try to convert date - ACM data uses DD-Mon-YYYY format
        tryCatch(
          {
            df$DATE <- as.Date(df$DATE, format = HETID_CONSTANTS$ACM_DATE_FORMAT)
          },
          error = function(e) {
            # If that fails, try standard conversion
            tryCatch(
              {
                df$DATE <- as.Date(df$DATE)
              },
              error = function(e2) {
                # If that fails too, try ISO format
                tryCatch(
                  {
                    df$DATE <- as.Date(df$DATE, format = HETID_CONSTANTS$ISO_DATE_FORMAT)
                  },
                  error = function(e3) {
                    # Leave as character if all conversions fail
                    warning("Could not convert DATE column to Date class. Keeping as character.")
                  }
                )
              }
            )
          }
        )
      }

      # Standardize column name to lowercase 'date'
      if ("DATE" %in% names(df)) {
        names(df)[names(df) == "DATE"] <- "date"
      }

      df
    },
    error = function(e) {
      warning("Failed to load term premia data: ", e$message)
      NULL
    }
  )
}
