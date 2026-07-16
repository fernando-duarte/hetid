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

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop_hetid(paste0(
      "Package 'readxl' is required to read Excel files.",
      " Please install it with: ",
      "install.packages('readxl')"
    ))
  }

  temp_xls <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_xls), add = TRUE, after = FALSE)

  if (!quiet) {
    message("Downloading ACM term premia data from NY Fed...")
  }
  fetch_url_to_file(
    download_url, temp_xls,
    quiet = quiet, what = "term premia data"
  )

  if (!quiet) {
    message("Converting Excel to CSV...")
  }
  tp_df <- tryCatch(
    readxl::read_excel(temp_xls),
    error = function(e) {
      stop_hetid(paste0(
        "Failed to convert term premia data: ", conditionMessage(e)
      ))
    }
  )

  # Atomic write: a partial write never half-overwrites the cache
  temp_csv <- tempfile(
    pattern = "acm_nyfed_", tmpdir = dirname(csv_path),
    fileext = ".csv"
  )
  on.exit(unlink(temp_csv), add = TRUE, after = FALSE)
  write.csv(tp_df, temp_csv, row.names = FALSE)
  atomic_replace(temp_csv, csv_path, "the converted term premia data")

  if (!quiet) {
    message("Term premia data saved to: ", csv_path)
    message(
      "Data dimensions: ",
      nrow(tp_df), " rows x ", ncol(tp_df),
      " columns"
    )
  }

  invisible(csv_path)
}
