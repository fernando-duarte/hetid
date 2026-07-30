# Diagnostics rows the shared cell builder deliberately does not produce: the
# tau = 0 point rows, and the normal-theory cross-check on the bootstrap
# calibration. Both are diagnostics-only by design. Keeping the superseded
# normal-theory critical values out of the shared builder is what guarantees a
# single live path into a published cell, so they are computed here, at the one
# place that writes the CSV, and nowhere else.

paper_source_once(paper_path(
  "support", "identification", "inference_calibration.R"
))

# An all-NA frame with the prototype's columns and types, so tau = 0 rows share
# the display rows' skeleton without hard-coding the column list twice.
.set_id_blank_rows <- function(prototype, n) {
  prototype[rep(NA_integer_, n), , drop = FALSE]
}

# The tau = 0 rows. A point is a degenerate interval, so the point lands on both
# set sides and its single robust scale on both scale columns -- exactly how the
# point-identified display rows already read, where se_lower equals se_upper and
# the two endpoint t statistics coincide. That reuse is why the tau = 0 scale
# becomes checkable from the CSV alone, which it was not before: the set-identified
# block's scales lived only in the draw cache.
set_id_boot_tau0_rows <- function(point_t, prototype) {
  rows <- .set_id_blank_rows(prototype, nrow(point_t))
  rows$coef <- point_t$coef
  rows$tau <- 0
  rows$set_lower <- point_t$point
  rows$set_upper <- point_t$point
  rows$width <- 0
  rows$set_status <- PAPER_ENDPOINT_STATUS[["bounded"]]
  rows$se_lower <- point_t$se
  rows$se_upper <- point_t$se
  rows$t_lower <- point_t$statistic
  rows$t_upper <- point_t$statistic
  rows$min_reps <- point_t$min_reps
  rows$reason <- point_t$reason
  cbind(
    rows,
    point_n_bounded = point_t$n_bounded,
    point_n_unbounded = point_t$n_unbounded,
    point_n_unreliable = point_t$n_unreliable,
    point_n_failed = point_t$n_failed,
    point_n_valid = point_t$n_valid_point,
    point_n_non_failed = point_t$n_non_failed,
    point_frac_bounded = point_t$frac_bounded,
    p_value = point_t$p_value,
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Pad the display rows with the tau = 0 block's own extra columns, taking their
# names and types from a tau = 0 row rather than restating them. Restating was the
# thing .set_id_blank_rows exists to avoid, and the two lists had already drifted
# apart once in review.
set_id_boot_pad_display <- function(rows, tau0_rows) {
  extra <- setdiff(names(tau0_rows), names(rows))
  cbind(
    rows,
    .set_id_blank_rows(tau0_rows[extra], nrow(rows)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# The normal-theory calibration the bootstrap replaced, as a cross-check on the
# normal approximation rather than as a second live path. rho is the endpoint
# correlation the bootstrap never has to estimate, and c_stoye and c_im are what
# the superseded fitted-bivariate-normal calibration would have returned at the
# same widths and scales. A large c_s against a small c_stoye is the normal fit
# understating the root's tails, which is the substantive finding here.
set_id_boot_normal_cross_check <- function(rows, cell, control) {
  n <- nrow(rows)
  rho <- vapply(seq_len(n), function(k) {
    robust_endpoint_cor(cell$lower[, k], cell$upper[, k], control)
  }, numeric(1))
  usable <- is.finite(rows$width) & rows$width > 0 &
    is.finite(rows$se_lower) & is.finite(rows$se_upper) &
    rows$se_lower > 0 & rows$se_upper > 0
  one <- function(k, fn, ...) {
    if (!usable[k]) {
      return(NA_real_)
    }
    fn(rows$width[k], rows$se_lower[k], rows$se_upper[k], ...)
  }
  alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
  data.frame(
    rho = rho,
    c_stoye = vapply(seq_len(n), function(k) {
      if (!usable[k] || !is.finite(rho[k])) {
        return(NA_real_)
      }
      one(k, stoye_critical, rho[k], alpha, control)
    }, numeric(1)),
    c_im = vapply(seq_len(n), function(k) {
      one(k, im_critical, alpha, control)
    }, numeric(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}
