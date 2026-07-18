# Definitions for computing fixed and identified-set variance shares.

centered_cov_t <- function(mat) {
  mat <- as.matrix(mat)
  crossprod(sweep(mat, 2, colMeans(mat))) / nrow(mat)
}

block_share <- function(coefs, s_block) {
  100 * rowSums((coefs %*% s_block) * coefs) / var_c
}

share_quad <- function(p_mat, q_vec, r_val) {
  list(
    value = function(pts) {
      rowSums((pts %*% p_mat) * pts) + drop(pts %*% q_vec) + r_val
    },
    grad = function(th) drop(2 * (p_mat %*% th) + q_vec)
  )
}

# Polish one share extreme in the identified-set solver's native scaling.
polish_extreme <- function(x0, quad, sq, box, sign_mult, delta, omega) {
  res <- solve_scaled_quadratic_program(
    quadratic = quad,
    x0 = x0,
    objective = function(theta) {
      sign_mult * sq$value(matrix(theta, 1))
    },
    gradient = function(theta) {
      sign_mult * sq$grad(theta)
    },
    lower = box$set_lower,
    upper = box$set_upper,
    method = "slsqp",
    objective_scale = "none"
  )
  if (!all(is.finite(res$theta))) {
    return(NA_real_)
  }
  th <- pmin(
    pmax(res$theta, box$set_lower),
    box$set_upper
  )
  resid <- res$feasibility_residual
  if (is.finite(resid) &&
    resid <= PAPER_QUADRATIC_CONTROL$feasibility_tolerance) {
    sq$value(matrix(th, 1))
  } else {
    NA_real_
  }
}

# Bound a convex block share over the joint identified set. A non-finite side
# cannot be gridded, so the range is not computed; the caller decides what that
# means from box$status, because the endpoints cannot say -- an uncertified
# solve can return finite ones and a certified-unbounded set infinite ones.
set_share_range <- function(box, quad, sq) {
  if (any(!is.finite(c(box$set_lower, box$set_upper)))) {
    return(c(NA_real_, NA_real_))
  }
  delta <- .derive_theta_scale(quad)
  omega <- .derive_constraint_scales(quad, delta)
  axes <- Map(
    \(l, h) seq(
      l,
      h,
      length.out =
        PAPER_ANALYSIS_CONTRACT$variance_share$grid_points_per_axis
    ),
    box$set_lower,
    box$set_upper
  )
  pts <- as.matrix(expand.grid(axes))
  constraint_values <- quadratic_constraint_values(pts, quad, omega)
  feas <- rowSums(
    constraint_values >
      PAPER_QUADRATIC_CONTROL$admission_tolerance
  ) == 0
  pts <- pts[feas, , drop = FALSE]
  stopifnot("no feasible grid point in the box" = nrow(pts) > 0)
  vals <- sq$value(pts)
  starts <- pts[unique(c(
    which.min(vals),
    which.max(vals),
    apply(pts, 2, which.min),
    apply(pts, 2, which.max)
  )), , drop = FALSE]
  polished <- function(sign_mult) {
    cand <- apply(
      starts,
      1,
      \(x0) polish_extreme(x0, quad, sq, box, sign_mult, delta, omega)
    )
    cand[is.finite(cand)]
  }
  c(min(vals, polished(1)), max(vals, polished(-1)))
}

# The mapped interval carries its row's status so the two cannot come apart on
# the way to the cell.
component_share_range <- function(tab, s_block) {
  scale <- 100 * diag(s_block) / var_c
  lo <- ifelse(
    tab$set_lower <= 0 & tab$set_upper >= 0,
    0,
    pmin(tab$set_lower^2, tab$set_upper^2)
  )
  data.frame(
    lo = scale * lo,
    hi = scale * pmax(tab$set_lower^2, tab$set_upper^2),
    status = tab$status
  )
}

fixed_shares <- function(field) {
  b_n <- set_id_mean_eq$theta_table[[field]]
  b_e <- set_id_mean_eq$beta1_table[[field]][e_rows]
  c(
    block_share(matrix(b_e, 1), s_e),
    100 * b_e^2 * diag(s_e) / var_c,
    block_share(matrix(b_n, 1), s_n),
    100 * b_n^2 * diag(s_n) / var_c
  )
}
