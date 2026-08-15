#' Methods and Assertions for hetid_tau0_fit Objects
#'
#' @name hetid_tau0_fit_methods
#' @keywords internal
NULL

#' Assert a Valid hetid_tau0_fit Object
#'
#' @param x Object to check
#' @param arg Argument name for the structured error
#'
#' @return Invisible TRUE when valid
#' @keywords internal
assert_hetid_tau0_fit <- function(x, arg = "fit") {
  assert_bad_argument_ok(
    inherits(x, "hetid_tau0_fit"),
    paste0(
      arg, " must be a hetid_tau0_fit object created by ",
      "compute_tau0_system()"
    ),
    arg = arg
  )
  invisible(TRUE)
}

#' Print a hetid_tau0_fit Object
#'
#' Reports the sample size, whether the null was imposed on the second
#' reduced form, and the tau=0 point when one exists. When the stacked
#' system has no unique consistent solution, \code{point} is \code{NULL}
#' for any of several reasons (rank deficiency, under-determination,
#' residual inconsistency); the printed message stays generic rather
#' than guessing which one applies.
#'
#' @param x A \code{hetid_tau0_fit} object
#' @param ... Unused, for method consistency
#'
#' @return \code{x}, invisibly
#' @export
print.hetid_tau0_fit <- function(x, ...) {
  cat("<hetid_tau0_fit>\n")
  cat("  n_obs: ", attr(x, "n_obs"), "\n", sep = "")
  cat("  impose_null: ", attr(x, "impose_null"), "\n", sep = "")
  if (is.null(x$point)) {
    cat("  point: no tau=0 point\n")
  } else {
    cat(
      "  point: theta = ",
      paste(format(x$point$theta), collapse = ", "),
      "; cond = ", format(x$point$cond), "\n",
      sep = ""
    )
  }
  invisible(x)
}
