#' Download Federal Reserve Yield Curve Data
#'
#' Downloads yield curve data from the Federal Reserve Board website.
#' Two datasets are available: the Svensson yield curve parameters (feds200628)
#' and the smoothed yield curve (feds200533).
#'
#' @param dataset Character string specifying which dataset to download.
#'   Either "feds200628" (Svensson parameters) or "feds200533" (smoothed yields).
#'   Can also use "both" to download both datasets.
#' @param force Logical. If TRUE, forces re-download even if data exists. Default is FALSE.
#' @param quiet Logical. If TRUE, suppresses download progress messages. Default is FALSE.
#'
#' @return Invisibly returns the path(s) to the saved CSV file(s).
#' @export
#'
#' @examples
#' \dontrun{
#' # Download Svensson yield curve parameters
#' download_yield_curve("feds200628")
#'
#' # Download smoothed yield curve
#' download_yield_curve("feds200533")
#'
#' # Download both datasets
#' download_yield_curve("both")
#'
#' # Force re-download
#' download_yield_curve("feds200628", force = TRUE)
#' }
#'
#' @references
#' Gürkaynak, R. S., Sack, B., and Wright, J. H. (2007).
#' "The U.S. Treasury yield curve: 1961 to the present."
#' Journal of Monetary Economics, 54(8), 2291-2304.
#'
#' Svensson, L. E. (1994).
#' "Estimating and interpreting forward interest rates: Sweden 1992-1994."
#' NBER Working Paper No. 4871.
#'
download_yield_curve <- function(dataset = c("feds200628", "feds200533", "both"),
                                 force = FALSE, quiet = FALSE) {
  dataset <- match.arg(dataset)

  # Define URLs
  urls <- list(
    feds200628 = "https://www.federalreserve.gov/data/yield-curve-tables/feds200628.csv",
    feds200533 = "https://www.federalreserve.gov/data/yield-curve-tables/feds200533.csv"
  )

  # Determine which datasets to download
  if (dataset == "both") {
    datasets_to_download <- names(urls)
  } else {
    datasets_to_download <- dataset
  }

  # Get package data directory
  pkg_dir <- system.file(package = "hetid")
  if (pkg_dir == "") {
    # Package not installed, use current directory structure
    csv_dir <- file.path("inst", "extdata")
  } else {
    # Package installed, use installed location
    csv_dir <- file.path(pkg_dir, "extdata")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  # Store paths of downloaded files
  downloaded_paths <- character()

  # Download each dataset
  for (ds in datasets_to_download) {
    url <- urls[[ds]]
    csv_path <- file.path(csv_dir, paste0(ds, ".csv"))

    # Check if file already exists
    if (file.exists(csv_path) && !force) {
      if (!quiet) {
        message(
          "Yield curve data '", ds, "' already exists. ",
          "Use force = TRUE to re-download."
        )
      }
      downloaded_paths <- c(downloaded_paths, csv_path)
      next
    }

    if (!quiet) {
      message("Downloading ", ds, " yield curve data from Federal Reserve...")
    }

    tryCatch(
      {
        # Download directly to destination since it's already CSV
        download.file(
          url = url,
          destfile = csv_path,
          mode = "w", # Text mode for CSV
          quiet = quiet
        )

        # Fed CSV files have header notes before the actual data
        # Read the file to find where the actual data starts
        all_lines <- readLines(csv_path)

        # Find the line that starts with "Date" (the actual header)
        header_line <- grep("^Date,", all_lines)[1]

        if (is.na(header_line)) {
          stop("Could not find data header in downloaded file")
        }

        # Skip lines before the header and re-read the CSV
        test_read <- tryCatch(
          read.csv(csv_path, skip = header_line - 1, nrows = 5),
          error = function(e) NULL
        )

        if (is.null(test_read)) {
          stop("Downloaded file does not appear to be valid CSV")
        }

        if (!quiet) {
          # Get file info, skipping header notes
          all_lines <- readLines(csv_path)
          header_line <- grep("^Date,", all_lines)[1]
          full_data <- read.csv(csv_path, skip = header_line - 1)

          # Rewrite the file without the header notes for easier loading
          write.csv(full_data, csv_path, row.names = FALSE)

          message(
            "Yield curve data '", ds, "' saved to: ", csv_path, "\n",
            "Data dimensions: ", nrow(full_data), " rows x ",
            ncol(full_data), " columns"
          )
        }

        downloaded_paths <- c(downloaded_paths, csv_path)
      },
      error = function(e) {
        # Clean up on error
        if (file.exists(csv_path)) {
          unlink(csv_path)
        }
        stop("Failed to download ", ds, " data: ", e$message)
      }
    )
  }

  invisible(downloaded_paths)
}
