#' Load Federal Reserve Yield Curve Data
#'
#' Loads yield curve data from the package's data directory.
#' If the data doesn't exist, prompts the user to download it first.
#'
#' @param dataset Character string specifying which dataset to load.
#'   Either "feds200628" (Svensson parameters) or "feds200533" (smoothed yields).
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it. Default is FALSE.
#'
#' @return A data frame containing the yield curve data, or NULL if data is not available.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load Svensson yield curve parameters
#' svensson_params <- load_yield_curve("feds200628")
#'
#' # Load smoothed yield curve
#' smoothed_yields <- load_yield_curve("feds200533")
#'
#' # Automatically download if not available
#' svensson_params <- load_yield_curve("feds200628", auto_download = TRUE)
#' }
#'
#' @seealso \code{\link{download_yield_curve}} for downloading the data
#'
load_yield_curve <- function(dataset = c("feds200628", "feds200533"),
                             auto_download = FALSE) {
  dataset <- match.arg(dataset)

  # Find the CSV file
  csv_filename <- paste0(dataset, ".csv")
  csv_path <- system.file("extdata", csv_filename, package = "hetid")

  # If not found in installed package, check development directory
  if (csv_path == "") {
    csv_path <- file.path("inst", "extdata", csv_filename)
  }

  # Check if file exists
  if (!file.exists(csv_path)) {
    if (auto_download) {
      message("Yield curve data '", dataset, "' not found. Downloading...")
      download_yield_curve(dataset)
      # Try loading again
      csv_path <- system.file("extdata", csv_filename, package = "hetid")
      if (csv_path == "") {
        csv_path <- file.path("inst", "extdata", csv_filename)
      }
    } else {
      message(
        "Yield curve data '", dataset, "' not found. ",
        "Please run download_yield_curve('", dataset, "') first."
      )
      return(NULL)
    }
  }

  # Load the data
  tryCatch(
    {
      df <- read.csv(csv_path, stringsAsFactors = FALSE)

      # Convert date columns based on dataset type
      if (dataset == "feds200628") {
        # This dataset typically has a Date column
        if ("Date" %in% names(df)) {
          df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
        }
      } else if (dataset == "feds200533") {
        # This dataset might have different date format
        date_cols <- grep("date", names(df), ignore.case = TRUE, value = TRUE)
        for (col in date_cols) {
          tryCatch(
            {
              df[[col]] <- as.Date(df[[col]])
            },
            error = function(e) {
              # Leave as character if conversion fails
              warning(
                "Could not convert column '", col,
                "' to Date class. Keeping as character."
              )
            }
          )
        }
      }

      # Standardize date column name to lowercase 'date'
      if ("Date" %in% names(df)) {
        names(df)[names(df) == "Date"] <- "date"
      } else if ("DATE" %in% names(df)) {
        names(df)[names(df) == "DATE"] <- "date"
      }

      df
    },
    error = function(e) {
      warning("Failed to load yield curve data: ", e$message)
      NULL
    }
  )
}
