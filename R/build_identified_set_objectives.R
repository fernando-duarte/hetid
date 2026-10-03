#' Named Coordinate and Structural Objectives
#'
#' Returns coordinate objectives followed by the slopes of the structural map
#' beta1(theta) = beta1r - t(beta2r) theta. Structural offsets are not included.
#'
#' @param fit A validated hetid_tau0_fit object.
#' @param null_loading_rtol Finite scalar in [0, 1). Defaults to zero, which
#'   retains every nonzero loading. A positive value uses the row-relative
#'   threshold documented for compute_identified_set_box().
#' @return A matrix with theta-named rows and unique objective-named columns.
#'   The first block is the identity; the second is negative beta2r after the
#'   requested zeroing. Overlapping coordinate and structural names are rejected.
#' @export
#' @seealso [compute_identified_set_box()]
#' @examples
#' t <- seq_len(40)
#' x <- matrix(sin(t), ncol = 1, dimnames = list(NULL, "x"))
#' z <- seq(-1, 1, length.out = length(t))
#' y2 <- matrix(x[, 1] + (1 + z) * cos(2 * t),
#'   ncol = 1,
#'   dimnames = list(NULL, "news")
#' )
#' y1 <- 0.5 + 0.2 * x[, 1] + 0.7 * y2[, 1] + sin(3 * t)
#' fit <- compute_tau0_system(y1, y2, x, z)
#' build_identified_set_objectives(fit)
build_identified_set_objectives <- function(fit, null_loading_rtol = 0) {
  validate_box_fit(fit)
  assert_scalar_finite(null_loading_rtol, "null_loading_rtol")
  assert_bad_argument_ok(null_loading_rtol >= 0 && null_loading_rtol < 1,
    "null_loading_rtol must lie in [0, 1)",
    arg = "null_loading_rtol"
  )
  objective_names <- c(colnames(fit$w2), names(fit$beta1r))
  assert_instrument_names(objective_names, "objectives")
  out <- identified_set_objectives(fit, ncol(fit$w2), null_loading_rtol)
  dimnames(out) <- list(colnames(fit$w2), objective_names)
  out
}
