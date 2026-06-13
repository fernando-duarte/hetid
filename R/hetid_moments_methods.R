#' Methods and Assertions for hetid_moments Objects
#'
#' @name hetid_moments_methods
#' @keywords internal
NULL

#' Assert a Valid hetid_moments Object
#'
#' @param x Object to check
#' @param arg Argument name for the structured error
#'
#' @return Invisible TRUE when valid
#' @keywords internal
assert_hetid_moments <- function(x, arg = "moments") {
  assert_bad_argument_ok(
    inherits(x, "hetid_moments"),
    paste0(
      arg, " must be a hetid_moments object created by ",
      "compute_identification_moments()"
    ),
    arg = arg
  )
  invisible(TRUE)
}

#' Print a hetid_moments Object
#'
#' @param x A \code{hetid_moments} object
#' @param ... Unused, for method consistency
#'
#' @return \code{x}, invisibly
#' @examples
#' set.seed(42)
#' w1 <- rnorm(100)
#' w2 <- matrix(rnorm(100 * 4), nrow = 100, ncol = 4)
#' pcs <- matrix(rnorm(100 * 3), nrow = 100, ncol = 3)
#' moments <- compute_identification_moments(w1, w2, pcs)
#' print(moments)
#' @export
print.hetid_moments <- function(x, ...) {
  maturities <- attr(x, "maturities")
  cat("<hetid_moments>\n")
  cat("  observations: ", attr(x, "n_obs"), "\n", sep = "")
  cat("  instruments (J): ", nrow(x$r_i_0), "\n", sep = "")
  cat("  components (theta axis): ", attr(x, "n_components"), "\n", sep = "")
  cat(
    "  maturities (constraint axis): ",
    paste(maturities, collapse = ", "), "\n",
    sep = ""
  )
  invisible(x)
}
