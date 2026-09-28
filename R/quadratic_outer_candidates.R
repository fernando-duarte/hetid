# Candidate weights for verified outer bounds. Every output here is a proposal
# that outer_verify() must accept before it can bound anything

outer_certificate_ok <- function(certificate, m) {
  s <- certificate$scales
  outer_weights_ok(certificate$weights, m) && length(s) == m &&
    all(is.finite(s)) && all(s > 0)
}

# Same combination: weights on A_i/scales_i become w_i*2^k_i/scales_i on power-of-two rows
# Use log space to avoid extreme-ratio overflow; return simplex weights or NULL
outer_certificate_weights <- function(certificate, sys) {
  if (!outer_certificate_ok(certificate, sys$m)) {
    return(NULL)
  }
  w <- certificate$weights
  keep <- w > 0
  log_w <- rep(-Inf, sys$m)
  log_w[keep] <- log(w[keep]) - log(certificate$scales[keep]) + sys$k[keep] * log(2)
  v <- exp(log_w - max(log_w[keep]))
  v / sum(v)
}

# Unverified closed-form bound used only to steer the search. Rejected weights
# score double.xmax rather than Inf, which Nelder-Mead would warn about
outer_search_value <- function(sys, v, loading) {
  mat <- Reduce(`+`, Map(`*`, sys$A, v))
  eig <- eigen(mat, symmetric = TRUE)
  if (!(min(eig$values) > 0)) {
    return(.Machine$double.xmax)
  }
  lin <- Reduce(`+`, Map(`*`, sys$b, v))
  proj_b <- crossprod(eig$vectors, lin)
  proj_l <- crossprod(eig$vectors, loading)
  r <- sum(proj_b^2 / eig$values) / 4 - sum(v * sys$c)
  a <- sum(proj_l^2 / eig$values)
  value <- -sum(proj_l * proj_b / eig$values) / 2 + sqrt(max(r, 0)) * sqrt(a)
  if (is.finite(value)) value else .Machine$double.xmax
}

# Interior simplex search for m >= 2 rows; returns simplex weights
outer_simplex_search <- function(sys, loading, v0, maxit) {
  m <- sys$m
  full <- function(p) c(p, 1 - sum(p))
  value <- function(p) {
    v <- full(p)
    if (any(v < 0)) {
      return(.Machine$double.xmax)
    }
    outer_search_value(sys, v, loading)
  }
  rtol <- HETID_CONSTANTS$QUADRATIC_OUTER_RTOL
  if (m == 2L) {
    return(full(stats::optimize(value, c(0, 1), tol = rtol)$minimum))
  }
  floor_weight <- HETID_CONSTANTS$QUADRATIC_WEIGHT_FLOOR
  initial <- pmax(v0[-m], floor_weight)
  if (sum(initial) >= 1) initial <- initial / (sum(initial) + floor_weight)
  fit <- stats::optim(initial, value,
    method = "Nelder-Mead",
    control = list(maxit = maxit, reltol = rtol)
  )
  full(fit$par)
}

# Budgeted upper-bound search for one normalized loading starts at v0 and uses no random numbers
# The interior misses exact vertices, so v0 and single-row vertices also compete
outer_side_search <- function(sys, loading, v0, maxit) {
  m <- sys$m
  if (m == 1L || maxit == 0L) {
    return(v0)
  }
  found <- outer_simplex_search(sys, loading, v0, maxit)
  contenders <- c(list(v0), lapply(seq_len(m), function(i) replace(numeric(m), i, 1)))
  if (all(is.finite(found)) && all(found >= 0)) contenders <- c(list(found), contenders)
  scores <- vapply(contenders, function(v) outer_search_value(sys, v, loading), 0)
  contenders[[which.min(scores)]]
}
