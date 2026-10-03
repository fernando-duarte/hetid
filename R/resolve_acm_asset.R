#' Resolve an ACM Asset or Pin Descriptor
#'
#' Resolves the data path without reading or verifying the asset. An explicit
#' release and digest also provide the requested release URL and snapshot identity.
#'
#' @param source A character scalar: \code{"auto"} (default),
#'   \code{"github"}, or \code{"nyfed"}.
#' @param frequency A character scalar: \code{"monthly"} (default) or
#'   \code{"daily"}. Quarterly extraction uses the monthly asset.
#' @template acm-pin
#'
#' @return A list with \code{release}, \code{sha256}, \code{frequency},
#'   \code{source_url}, and \code{path}. Without a pin, \code{release},
#'   \code{sha256}, and \code{source_url} are \code{NULL}: the origin of an
#'   unpinned cache or bundled file is not inferred from a download URL.
#'   With a pin, the release, normalized digest and URL describe the request.
#'   The path need not exist, and neither form verifies its bytes or provenance.
#'
#' @details
#' Unpinned monthly resolution prefers the user cache and then the bundled file.
#' Daily and NY Fed assets resolve only their own cache paths; daily NY Fed data
#' and NY Fed pins are unavailable. Invalid arguments raise structured
#' \code{hetid_error_bad_argument} conditions.
#'
#' No asset is read, downloaded or copied and no cache directory is created.
#' Pin identity hashing uses a temporary file. Callers can hash \code{path}
#' before extraction or construct offline fixtures with \code{source_url}.
#' A requested URL does not establish that matching bytes were published there.
#'
#' @seealso \code{\link{load_term_premia}}, \code{\link{download_term_premia}}
#' @export
#' @examples
#' resolve_acm_asset()
#' resolve_acm_asset(
#'   frequency = "daily", release = "example-tag",
#'   expected_sha256 = strrep("a", 64)
#' )
resolve_acm_asset <- function(source = "auto", frequency = "monthly",
                              release = NULL, expected_sha256 = NULL) {
  assert_acm_pin_string(source, "source")
  assert_acm_pin_string(frequency, "frequency")
  assert_bad_argument_ok(
    source %in% c("auto", "github", "nyfed"),
    "source must be one of auto, github, or nyfed",
    arg = "source"
  )
  assert_bad_argument_ok(
    frequency %in% c("monthly", "daily"),
    "frequency must be monthly or daily",
    arg = "frequency"
  )
  pin <- acm_pin(release, expected_sha256, source, frequency)
  if (!is.null(pin)) {
    return(pin)
  }
  list(
    release = NULL, sha256 = NULL, frequency = frequency, source_url = NULL,
    path = get_acm_data_path(source, frequency)
  )
}
