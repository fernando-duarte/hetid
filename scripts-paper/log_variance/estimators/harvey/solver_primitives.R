# Guarded linear-algebra primitives for the Harvey solver.

hv_chol_xx <- function(x_mat, chol_xx) {
  if (is.null(chol_xx)) chol(crossprod(x_mat)) else chol_xx
}

hv_chol_solve <- function(chol_xx, m) {
  backsolve(
    chol_xx,
    forwardsolve(
      chol_xx,
      m,
      upper.tri = TRUE,
      transpose = TRUE
    )
  )
}

hv_eval <- function(theta, y, x_mat, pos, col_abs) {
  if (!all(is.finite(theta))) {
    return(NULL)
  }
  eta <- drop(x_mat %*% theta)
  if (!all(is.finite(eta)) ||
    any(eta > log(.Machine$double.xmax))) {
    return(NULL)
  }
  r <- logvar_harvey_ratio(theta, y, x_mat)
  if (!is.numeric(r) || length(r) != length(y) || anyNA(r) ||
    !all(is.finite(r[pos]))) {
    return(NULL)
  }
  q <- 0.5 * (sum(eta) + sum(r[pos]))
  moment <- drop(crossprod(x_mat, r - 1))
  score_norm <- max(abs(moment) / col_abs)
  if (!is.finite(q) || !is.finite(score_norm)) {
    return(NULL)
  }
  list(
    theta = theta,
    eta = eta,
    r = r,
    q = q,
    moment = moment,
    score_norm = score_norm
  )
}
