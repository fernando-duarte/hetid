#' Load ACM Term Premia Data
#'
#' Loads the ACM term premia data, preferring a downloaded copy in the
#' per-user data directory and falling back to the bundled copy.
#' If the data doesn't exist, prompts the user to download it first.
#'
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it. Default is FALSE.
#'
#' @return A data frame containing the term premia data, or NULL if data is not available.
#' @export
#'
#' @examples
#' # Load the bundled data
#' term_premia <- load_term_premia()
#' head(term_premia)
#'
#' @seealso \code{\link{download_term_premia}} for downloading the data
#'
load_term_premia <- function(auto_download = FALSE) {
  if (!check_data_file_exists(HETID_CONSTANTS$ACM_DATA_FILENAME)) {
    if (auto_download) {
      message("Term premia data not found. Downloading...")
      download_term_premia()
    } else {
      message("Term premia data not found. Please run download_term_premia() first.")
      return(NULL)
    }
  }

  # Resolve once, after any download, so a fresh user-cache copy is found
  csv_path <- get_acm_data_path()

  # Load the data
  tryCatch(
    {
      tp_df <- read.csv(csv_path, stringsAsFactors = FALSE)

      # Convert DATE column to Date class if it exists
      if ("DATE" %in% names(tp_df)) {
        raw_dates <- tp_df$DATE
        # Try the locale-safe ACM format, then R's default parser (NULL),
        # then explicit ISO: the default parser errors when the first
        # element is malformed even if the rest of the file is valid ISO
        date_formats <- list(
          HETID_CONSTANTS$ACM_DATE_FORMAT,
          NULL,
          HETID_CONSTANTS$ISO_DATE_FORMAT
        )
        for (fmt in date_formats) {
          parsed <- tryCatch(
            if (is.null(fmt)) {
              as.Date(raw_dates)
            } else {
              parse_dates_c_locale(raw_dates, fmt)
            },
            error = function(e) NULL
          )
          if (!is.null(parsed) && !all(is.na(parsed))) {
            n_new_na <- sum(is.na(parsed) & !is.na(raw_dates))
            if (n_new_na > 0) {
              warning(
                n_new_na,
                " DATE value(s) could not be parsed and became NA",
                call. = FALSE
              )
            }
            tp_df$DATE <- parsed
            break
          }
        }
        if (!inherits(tp_df$DATE, "Date")) {
          warning(
            "Could not convert DATE column to ",
            "Date class. Keeping as character.",
            call. = FALSE
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
      stop_hetid(paste0(
        "Failed to read term premia data: ",
        e$message
      ))
    }
  )
}
