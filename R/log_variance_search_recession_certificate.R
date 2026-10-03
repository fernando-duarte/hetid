lv_set_lp <- function(obj, gmat, hvec, emat, evec, lower, upper, x0, control) {
  result <- tryCatch(nloptr::slsqp(
    x0 = x0, fn = function(x) sum(obj * x),
    gr = function(x) obj, lower = lower, upper = upper,
    hin = function(x) drop(gmat %*% x) - hvec, hinjac = function(x) gmat,
    heq = function(x) drop(emat %*% x) - evec, heqjac = function(x) emat,
    control = list(xtol_rel = control$lp_xtol_rel, maxeval = control$lp_maxeval),
    deprecatedBehavior = FALSE
  ), error = function(e) NULL)
  if (is.null(result) || any(!is.finite(result$par))) {
    return(NULL)
  }
  result$par
}

lv_set_facet_primal <- function(c_vec, z_pos, j, sign_j, control) {
  p <- length(c_vec)
  x0 <- numeric(p)
  x0[j] <- sign_j
  emat <- matrix(0, 1, p)
  emat[1, j] <- 1
  u <- lv_set_lp(
    c_vec, -z_pos, rep(0, nrow(z_pos)), emat, sign_j, rep(-1, p),
    rep(1, p), x0, control
  )
  if (is.null(u)) {
    return(list(ok = FALSE))
  }
  list(
    ok = TRUE, u = u, value = sum(c_vec * u),
    feas_viol = max(c(drop(-z_pos %*% u), abs(u[j] - sign_j), abs(u) - 1))
  )
}

lv_set_facet_dual_bound <- function(c_vec, gmat, hvec, e_j, sign_j, lambda, nu) {
  residual <- c_vec + drop(t(gmat) %*% lambda) + e_j * nu
  -sum(hvec * lambda) - sign_j * nu - sum(abs(residual))
}

lv_set_facet_dual <- function(c_vec, z_pos, j, sign_j, control) {
  p <- length(c_vec)
  gmat <- rbind(-z_pos, diag(p), -diag(p))
  hvec <- c(rep(0, nrow(z_pos)), rep(1, 2 * p))
  m <- nrow(gmat)
  e_j <- numeric(p)
  e_j[j] <- 1
  solution <- lv_set_lp(
    c(hvec, sign_j), matrix(0, 1, m + 1), 0, cbind(t(gmat), e_j),
    -c_vec, c(rep(0, m), -control$lp_bound), rep(control$lp_bound, m + 1), rep(0, m + 1),
    control
  )
  if (is.null(solution)) {
    return(list(ok = FALSE))
  }
  lambda <- pmax(solution[seq_len(m)], 0)
  nu <- solution[m + 1]
  list(ok = TRUE, bound = lv_set_facet_dual_bound(
    c_vec, gmat, hvec, e_j, sign_j,
    lambda, nu
  ))
}

lv_set_facet_phase1 <- function(z_pos, j, sign_j, t_max, control) {
  p <- ncol(z_pos)
  n_pos <- nrow(z_pos)
  obj <- c(numeric(p), 1)
  emat <- matrix(0, 1, p + 1)
  emat[1, j] <- 1
  x0 <- c(numeric(p), t_max)
  x0[j] <- sign_j
  v <- lv_set_lp(
    obj, cbind(-z_pos, -1), rep(0, n_pos), emat, sign_j,
    c(rep(-1, p), 0), c(rep(1, p), t_max), x0, control
  )
  if (is.null(v)) {
    return(list(ok = FALSE))
  }
  g_full <- rbind(
    cbind(-z_pos, -1), cbind(diag(p), numeric(p)), cbind(-diag(p), numeric(p)),
    c(numeric(p), 1), c(numeric(p), -1)
  )
  h_full <- c(rep(0, n_pos), rep(1, 2 * p), t_max, 0)
  m <- nrow(g_full)
  dual <- lv_set_lp(
    c(h_full, sign_j), matrix(0, 1, m + 1), 0,
    cbind(t(g_full), emat[1, ]), -obj, c(rep(0, m), -control$lp_bound),
    rep(control$lp_bound, m + 1), rep(0, m + 1), control
  )
  if (is.null(dual)) {
    return(list(ok = TRUE, t_min = v[p + 1], dual_ok = FALSE))
  }
  lambda <- pmax(dual[seq_len(m)], 0)
  nu <- dual[m + 1]
  residual <- obj + drop(t(g_full) %*% lambda) + emat[1, ] * nu
  list(
    ok = TRUE, t_min = v[p + 1], dual_ok = TRUE,
    bound = -sum(h_full * lambda) - sign_j * nu - sum(abs(residual[seq_len(p)])) -
      t_max * abs(residual[p + 1])
  )
}

lv_set_facet <- function(c_vec, z_pos, j, sign_j, rate_tol, t_max, control) {
  primal <- lv_set_facet_primal(c_vec, z_pos, j, sign_j, control)
  found <- lv_set_facet_witness(primal, control, rate_tol)
  if (!is.na(found)) {
    if (!identical(found, "positive")) {
      return(list(status = found, primal = primal))
    }
    dual <- lv_set_facet_dual(c_vec, z_pos, j, sign_j, control)
    closed <- dual$ok && abs(primal$value - dual$bound) /
      max(1, abs(primal$value), abs(dual$bound)) <= control$certificate_tol
    certified <- dual$ok && dual$bound > rate_tol && closed
    return(list(
      status = if (certified) "certified_positive" else "unresolved",
      primal = primal
    ))
  }
  phase1 <- lv_set_facet_phase1(z_pos, j, sign_j, t_max, control)
  if (phase1$ok && phase1$t_min <= control$certificate_tol) {
    # the facet is feasible after all, so the rate is read again
    primal <- lv_set_facet_primal(c_vec, z_pos, j, sign_j, control)
    found <- lv_set_facet_witness(primal, control, rate_tol)
    status <- if (found %in% c("negative_witness", "zero_witness")) found else "unresolved"
    return(list(status = status, primal = primal))
  }
  empty <- phase1$ok && isTRUE(phase1$dual_ok) && phase1$bound > control$certificate_tol
  list(status = if (empty) "certified_infeasible" else "unresolved", primal = primal)
}

lv_set_recession <- function(y, x_mat, control) {
  p <- ncol(x_mat)
  norms <- pmax(1, sqrt(colSums(x_mat^2)))
  z_mat <- sweep(x_mat, 2, norms, "/")
  positive <- y > 0
  z_pos <- z_mat[positive, , drop = FALSE]
  x_pos <- x_mat[positive, , drop = FALSE]
  rank_x_pos <- if (nrow(x_pos) == 0L) {
    0L
  } else {
    norms <- sqrt(colSums(x_pos^2))
    scaled <- x_pos
    scaled[, norms > 0] <- sweep(x_pos[, norms > 0, drop = FALSE], 2, norms[norms > 0], "/")
    d <- svd(scaled)$d
    sum(d > control$recession_rank_tol * d[1])
  }
  c_vec <- colSums(z_mat)
  rate_tol <- control$recession_rate_multiplier * max(1, sum(abs(c_vec)))
  out <- list(rank_x_pos = rank_x_pos, rate_tol = rate_tol, facets = character(0))
  if (all(positive) && rank_x_pos == p) {
    out$classification <- "pass"
    return(out)
  }
  if (!any(positive)) {
    out$classification <- "negative_recession"
    return(out)
  }
  t_max <- max(1, max(rowSums(abs(z_pos))))
  for (j in seq_len(p)) {
    for (sign_j in c(-1, 1)) {
      status <- lv_set_facet(c_vec, z_pos, j, sign_j, rate_tol, t_max, control)$status
      out$facets <- c(out$facets, status)
      if (identical(status, "negative_witness")) {
        out$classification <- "negative_recession"
        return(out)
      }
    }
  }
  out$classification <- if (any(out$facets == "zero_witness")) {
    "zero_recession"
  } else if (all(out$facets %in% c("certified_positive", "certified_infeasible"))) {
    "pass"
  } else {
    "certificate_failure"
  }
  out
}

lv_set_facet_witness <- function(primal, control, rate_tol) {
  if (!(primal$ok && primal$feas_viol <= control$certificate_tol)) {
    return(NA_character_)
  }
  if (primal$value < -rate_tol) {
    "negative_witness"
  } else if (primal$value <= rate_tol) {
    "zero_witness"
  } else {
    "positive"
  }
}
