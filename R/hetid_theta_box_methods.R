#' Methods for hetid_theta_box Objects
#'
#' Front-door assertion and the print method.
#'
#' @name hetid_theta_box_methods
#' @keywords internal
NULL

#' Assert a hetid_theta_box Object
#'
#' @param x Object to check
#' @param arg Argument name used in the error message
#' @return Invisibly TRUE
#' @keywords internal
assert_hetid_theta_box <- function(x, arg = "box") {
  assert_bad_argument_ok(
    inherits(x, "hetid_theta_box"),
    paste0(arg, " must be a hetid_theta_box object"),
    arg = arg
  )
  invisible(TRUE)
}

#' Print a hetid_theta_box Object
#'
#' @param x A \code{hetid_theta_box} object
#' @param ... Unused, for method consistency
#' @return \code{x}, invisibly
#' @export
#' @examples
#' set.seed(42)
#' n_obs <- 200
#' x <- cbind(x1 = rnorm(n_obs), x2 = rnorm(n_obs))
#' z <- rnorm(n_obs)
#' e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(n_obs * 2), n_obs, 2)
#' y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
#' colnames(y2) <- c("news1", "news2")
#' y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% c(0.8, -0.5) + rnorm(n_obs))
#'
#' fit <- compute_tau0_system(y1, y2, x, z)
#' print(compute_identified_set_box(fit, tau = 0.05))
print.hetid_theta_box <- function(x, ...) {
  unbounded <- sum(!is.finite(x$bounds$lower) | !is.finite(x$bounds$upper))
  cat("<hetid_theta_box>\n")
  cat("  slack (tau): ", attr(x, "tau"), "\n", sep = "")
  cat("  observations: ", attr(x, "n_obs"), "\n", sep = "")
  cat("  components (theta axis): ", attr(x, "n_components"), "\n", sep = "")
  cat("  grid points per coordinate: ", attr(x, "n_grid"), "\n", sep = "")
  cat("  unbounded sides: ", unbounded, "\n", sep = "")
  invisible(x)
}
