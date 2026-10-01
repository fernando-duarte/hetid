#' Methods and Assertions for hetid_moments Objects
#'
#' Internal class assertion and print method for moment containers created by
#' \code{\link{compute_identification_moments}}.
#'
#' @name hetid_moments_methods
#' @keywords internal
NULL

#' Assert hetid_moments Class Membership
#'
#' Checks whether an object inherits from \code{hetid_moments}. This does not
#' validate its statistics or attributes; use \code{\link{validate_hetid_moments}}
#' for structural validation.
#'
#' @param x Object to check for class inheritance.
#' @param arg Character string naming the argument in the error message and
#'   condition. Defaults to \code{"moments"}.
#'
#' @return The logical scalar \code{TRUE}, invisibly, when the class is present.
#'   Otherwise, signals a \code{hetid_error_bad_argument} condition with the
#'   supplied \code{arg} field.
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
#' Prints the observation and instrument counts, the theta-axis dimension,
#' and the selected constraint-axis indices to the console.
#'
#' @param x A \code{hetid_moments} list created by
#'   \code{\link{compute_identification_moments}}.
#' @param ... Additional arguments, ignored by this method.
#'
#' @return The supplied \code{hetid_moments} object \code{x}, invisibly.
#' @details The displayed maturities are column indices of \code{w2}, not
#'   necessarily bond maturities. The theta axis includes all \code{w2} columns;
#'   the constraint axis includes only the selected indices.
#' @seealso \code{\link[base:print]{print}} for the generic.
#' @examples
#' local({
#'   old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
#'     get(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     NULL
#'   }
#'   on.exit({
#'     if (is.null(old_seed)) {
#'       rm(".Random.seed", envir = .GlobalEnv)
#'     } else {
#'       assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'     }
#'   })
#'   set.seed(42)
#'   w1 <- rnorm(100)
#'   w2 <- matrix(rnorm(100 * 2), nrow = 100, ncol = 2)
#'   pcs <- matrix(rnorm(100), nrow = 100, ncol = 1)
#'   moments <- compute_identification_moments(w1, w2, pcs, maturities = 2)
#'   print(moments)
#' })
#' @export
print.hetid_moments <- function(x, ...) {
  maturities <- attr(x, "maturities")
  cat("<hetid_moments>\n")
  cat("  observations: ", attr(x, "n_obs"), "\n", sep = "")
  cat("  instruments (J): ", attr(x, "n_instruments"), "\n", sep = "")
  cat("  components (theta axis): ", attr(x, "n_components"), "\n", sep = "")
  cat(
    "  maturities (constraint axis): ",
    paste(maturities, collapse = ", "), "\n",
    sep = ""
  )
  invisible(x)
}
