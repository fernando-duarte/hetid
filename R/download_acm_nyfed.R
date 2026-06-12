#' Download the ACM Data from the NY Fed Workbook
#'
#' Opt-in fallback source: downloads the official NY Fed xls workbook,
#' converts it to CSV via \pkg{readxl}, and caches it under the
#' nyfed-specific filename. This source provides annual maturities
#' only.
#'
#' @param quiet Logical, suppress progress output
#' @return Invisibly returns the cached file path
#' @keywords internal
download_acm_nyfed <- function(quiet = FALSE) {
  download_url <- DATA_URLS$ACM_NYFED_XLS
  csv_path <- get_acm_download_path("nyfed")

  # Check if readxl is available before downloading
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop_hetid(paste0(
      "Package 'readxl' is required to read Excel files.",
      " Please install it with: ",
      "install.packages('readxl')"
    ))
  }

  # Download to temporary file
  temp_xls <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_xls), add = TRUE)

  if (!quiet) {
    message("Downloading ACM term premia data from NY Fed...")
  }

  tryCatch(
    {
      download.file(
        url = download_url,
        destfile = temp_xls,
        mode = "wb",
        quiet = quiet
      )

      # Read Excel file
      if (!quiet) {
        message("Converting Excel to CSV...")
      }

      tp_df <- readxl::read_excel(temp_xls)

      # Save as CSV
      write.csv(tp_df, csv_path, row.names = FALSE)

      if (!quiet) {
        message("Term premia data saved to: ", csv_path)
        message(
          "Data dimensions: ",
          nrow(tp_df), " rows x ", ncol(tp_df),
          " columns"
        )
      }

      invisible(csv_path)
    },
    error = function(e) {
      stop_hetid(paste0(
        "Failed to download term premia data: ",
        e$message
      ))
    }
  )
}
