#' Load ACM Term Premia Data
#'
#' Loads the ACM term premia data. The default \code{"auto"} source
#' prefers a downloaded GitHub-release copy in the per-user data
#' directory and falls back to the bundled copy; the NY Fed fallback is
#' never loaded implicitly. Pass \code{source = "nyfed"} to load an
#' explicitly downloaded NY Fed cache instead.
#'
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it (from the GitHub release for \code{"auto"}/
#'   \code{"github"}, from the NY Fed for \code{"nyfed"}). Default is FALSE.
#' @param source Data source: \code{"auto"} (default; GitHub user cache,
#'   then bundled copy), \code{"github"} (same resolution), or
#'   \code{"nyfed"} (the NY Fed cache only). Note the asymmetry with
#'   \code{\link{download_term_premia}}, which has no \code{"auto"}: a
#'   download is always source-specific.
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
load_term_premia <- function(auto_download = FALSE,
                             source = c("auto", "github", "nyfed")) {
  source <- match.arg(source)
  if (!acm_data_available(source)) {
    if (auto_download) {
      message("Term premia data not found. Downloading...")
      download_term_premia(
        source = if (source == "nyfed") "nyfed" else "github"
      )
    } else if (source == "nyfed") {
      stop_insufficient_data(paste0(
        "NY Fed ACM cache not found. Run ",
        "download_term_premia(source = \"nyfed\") first."
      ))
    } else {
      message(
        "Term premia data not found. Please run download_term_premia() first."
      )
      return(NULL)
    }
  }

  # Resolve after any download so a fresh user-cache copy is found.
  csv_path <- get_acm_data_path(source)
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

  if ("DATE" %in% names(tp_df)) {
    tp_df$DATE <- parse_and_warn_dates(tp_df$DATE, "DATE")
    names(tp_df)[names(tp_df) == "DATE"] <- "date"
  }

  tp_df
}
