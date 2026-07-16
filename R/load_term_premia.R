#' Load ACM Term Premia Data
#'
#' Loads the ACM term premia data. The default \code{"auto"} source
#' prefers a downloaded GitHub-release copy in the per-user data
#' directory and falls back to the bundled copy; the NY Fed fallback is
#' never loaded implicitly. Pass \code{source = "nyfed"} to load an
#' explicitly downloaded NY Fed cache instead. The daily-frequency
#' series is cache-only (no bundled copy): it must be downloaded first
#' unless \code{auto_download = TRUE}.
#'
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it (from the GitHub release for \code{"auto"}/
#'   \code{"github"}, from the NY Fed for \code{"nyfed"}). Default is FALSE.
#' @param source Data source: \code{"auto"} (default; GitHub user cache,
#'   then bundled copy), \code{"github"} (same resolution), or
#'   \code{"nyfed"} (the NY Fed cache only). Note the asymmetry with
#'   \code{\link{download_term_premia}}, which has no \code{"auto"}: a
#'   download is always source-specific.
#' @param frequency Data frequency: \code{"monthly"} (default) or
#'   \code{"daily"} (GitHub source only; user cache only).
#'
#' @return A data frame containing the term premia data. Raises a
#'   \code{hetid_error_insufficient_data} condition when the data is not
#'   available (every source now fails the same way; the \code{"auto"}/
#'   \code{"github"} path formerly returned NULL).
#' @export
#'
#' @examples
#' # Load the bundled data
#' term_premia <- load_term_premia()
#' head(term_premia)
#'
#' @seealso \code{\link{download_term_premia}} for downloading the data
#'
load_term_premia <- function(auto_download = FALSE,
                             source = c("auto", "github", "nyfed"),
                             frequency = c("monthly", "daily")) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  assert_flag(auto_download, "auto_download")
  if (!acm_data_available(source, frequency)) {
    if (auto_download) {
      message("Term premia data not found. Downloading...")
      download_term_premia(
        source = if (source == "nyfed") "nyfed" else "github",
        frequency = frequency
      )
    } else if (source == "nyfed") {
      stop_insufficient_data(paste0(
        "NY Fed ACM cache not found. Run ",
        "download_term_premia(source = \"nyfed\") first."
      ))
    } else {
      hint <- if (frequency == "daily") "frequency = \"daily\"" else ""
      stop_insufficient_data(paste0(
        "Term premia data not found. Run download_term_premia(",
        hint, ") first or set auto_download = TRUE."
      ))
    }
  }

  # Resolve after any download so a fresh user-cache copy is found
  csv_path <- get_acm_data_path(source, frequency)
  tp_df <- tryCatch(
    read.csv(csv_path, stringsAsFactors = FALSE),
    error = function(e) {
      stop_hetid(paste0(
        "Failed to read term premia data: ",
        conditionMessage(e)
      ))
    }
  )

  validate_acm_schema(tp_df, csv_path)

  date_col <- intersect(c("DATE", "date"), names(tp_df))[1]
  if (!is.na(date_col)) {
    tp_df[[date_col]] <- parse_and_warn_dates(tp_df[[date_col]], date_col)
    names(tp_df)[names(tp_df) == date_col] <- "date"
  }

  tp_df
}
