# Constrained-solve helpers for the LAD crossing witness (crossing_domain.R
# sources this file): row geometry, deterministic starts, the squared-normalized-
# residual minimizer, the b_scales-metric projection onto H_i, the signed feasible
# anchor solve, and the simultaneous-hit sign-cone enumeration. All nloptr solves
# use the house hin <= 0 convention (deprecatedBehavior = FALSE) with analytic
# gradients. No residual floor is ever applied.

# Row-i geometry read off ctx: normal w2[i, ], rhs w1[i], residual e_i(b), the
# normalized constraint values g_k(b) <= 0, and their analytic Jacobian.
.lad_domain_geom <- function(i, qs, ctx) {
  w2 <- ctx$w2
  normal <- as.numeric(w2[i, ])
  rhs <- ctx$w1[i]
  omega <- ctx$b_scales$omega
  kdim <- ncol(w2)
  n_con <- length(qs$A_i)
  gvals <- function(b) {
    vapply(seq_len(n_con), function(k) {
      (drop(t(b) %*% qs$A_i[[k]] %*% b) + sum(qs$b_i[[k]] * b) +
        qs$c_i[k]) / omega[k]
    }, numeric(1))
  }
  gjac <- function(b) {
    matrix(t(vapply(seq_len(n_con), function(k) {
      (2 * drop(qs$A_i[[k]] %*% b) + qs$b_i[[k]]) / omega[k]
    }, numeric(kdim))), nrow = n_con, ncol = kdim)
  }
  list(
    normal = normal, rhs = rhs, esc = ctx$e_scale_ref,
    b_sc = ctx$b_scales$delta, kdim = kdim, n_con = n_con,
    resid = function(b) rhs - sum(normal * b), gvals = gvals, gjac = gjac
  )
}

# Deterministic witness starts: the census box corners and midpoint, one-at-a-time
# box-edge points, and the Euclidean projection of each onto H_i (a sign-pair
# generator falls out because projecting straddling points lands on the plane).
.lad_witness_starts <- function(geom, b_tab) {
  normal <- geom$normal
  nn <- sum(normal^2)
  proj <- function(b) if (nn > 0) b + normal * (geom$rhs - sum(normal * b)) / nn else b
  lo <- as.numeric(b_tab$set_lower)
  hi <- as.numeric(b_tab$set_upper)
  mid <- (lo + hi) / 2
  half <- (hi - lo) / 2
  base <- list(mid, lo, hi)
  for (s in c(-0.5, 0.5)) {
    for (k in seq_len(geom$kdim)) {
      b <- mid
      b[k] <- mid[k] + s * half[k]
      base[[length(base) + 1L]] <- b
    }
  }
  unique(c(base, lapply(base, proj)))
}

# Minimize (e_i(b) / e_scale_ref)^2 over the feasible set from every start; return
# the feasible candidate with the smallest normalized row residual (convex inner
# problem, so any feasible start that reaches H_i suffices).
.lad_witness_min <- function(geom, starts) {
  esc <- geom$esc
  normal <- geom$normal
  obj <- function(b) (geom$resid(b) / esc)^2
  gobj <- function(b) as.numeric(-2 * geom$resid(b) * normal / esc^2)
  best <- NULL
  best_val <- Inf
  for (x0 in starts) {
    r <- tryCatch(nloptr::slsqp(
      x0 = x0, fn = obj, gr = gobj, hin = geom$gvals, hinjac = geom$gjac,
      control = list(xtol_rel = 1e-10, maxeval = 2000L), deprecatedBehavior = FALSE
    ), error = function(e) NULL)
    if (is.null(r) || any(!is.finite(r$par))) next
    b <- r$par
    if (max(geom$gvals(b)) > 1e-8) next
    v <- abs(geom$resid(b)) / esc
    if (v < best_val) {
      best_val <- v
      best <- b
    }
  }
  best
}

# Project onto H_i in the b_scales metric (diagonal weights b_sc^2).
.lad_project_metric <- function(geom, b) {
  d <- geom$b_sc^2
  normal <- geom$normal
  denom <- sum(normal * d * normal)
  if (!is.finite(denom) || denom <= 0) {
    return(b)
  }
  b + d * normal * (geom$rhs - sum(normal * b)) / denom
}

# Pull a candidate back along the segment from the feasible witness until the
# exact constraint g(b) <= 0 holds. A quadratic set constraint is numerically flat
# on a very thin component, so slsqp can report a point far outside it as feasible
# under a fixed tolerance; bisecting on the exact constraint collapses such a point
# back onto the true set, where the claimed residual no longer survives.
.lad_pull_feasible <- function(geom, b_cross, b) {
  if (max(geom$gvals(b)) <= 0) {
    return(b)
  }
  lo <- 0
  hi <- 1
  for (it in seq_len(50L)) {
    mid <- (lo + hi) / 2
    if (max(geom$gvals(b_cross + mid * (b - b_cross))) <= 0) lo <- mid else hi <- mid
  }
  b_cross + lo * (b - b_cross)
}

# Signed approach anchor on side s in {-1, 1}: the feasible point inside the local
# 0.05 b_scales ball maximizing s * e_i(b), pulled onto the exact feasible set.
# Accept only when the signed residual takes side s with magnitude at least
# 1e-8 * e_scale_ref, so a thin component yields no anchor and stays unresolved.
.lad_anchor <- function(geom, b_cross, s) {
  thr <- 1e-8 * geom$esc
  ball <- function(b) sum(((b - b_cross) / geom$b_sc)^2) - 0.05^2
  ball_j <- function(b) 2 * (b - b_cross) / (geom$b_sc^2)
  hin <- function(b) c(geom$gvals(b), ball(b))
  hinj <- function(b) rbind(geom$gjac(b), ball_j(b))
  r <- tryCatch(nloptr::slsqp(
    x0 = b_cross, fn = function(b) s * sum(geom$normal * b),
    gr = function(b) s * geom$normal, hin = hin, hinjac = hinj,
    control = list(xtol_rel = 1e-10, maxeval = 1000L), deprecatedBehavior = FALSE
  ), error = function(e) NULL)
  if (is.null(r) || any(!is.finite(r$par))) {
    return(NULL)
  }
  b <- .lad_pull_feasible(geom, b_cross, r$par)
  sr <- geom$resid(b)
  if (ball(b) > 1e-8 || sign(sr) != s || abs(sr) < thr) {
    return(NULL)
  }
  list(b = b, sign = s, residual = sr)
}

# Enumerate the locally feasible residual-sign cones over every row crossing at
# b_cross (a simultaneous hit brings in more than row i). For each sign pattern the
# min-norm direction realizing it is stepped a short b_scales distance and kept
# only if feasible with residuals bounded off zero. The 2^q enumeration is capped.
.lad_sign_cones <- function(geom, b_cross, ctx, i) {
  w2 <- ctx$w2
  all_res <- (ctx$w1 - as.numeric(w2 %*% b_cross)) / geom$esc
  simul <- which(abs(all_res) <= 1e-8)
  if (length(simul) == 0L) simul <- i
  if (length(simul) > 8L) simul <- simul[seq_len(8L)]
  q <- length(simul)
  wj <- w2[simul, , drop = FALSE]
  rj <- ctx$w1[simul]
  thr <- 1e-8 * geom$esc
  step <- 0.02 * min(geom$b_sc)
  gram <- wj %*% t(wj)
  ridge <- 1e-12 * (mean(diag(gram)) + 1) * diag(q)
  pats <- as.matrix(expand.grid(rep(list(c(-1, 1)), q)))
  cones <- list()
  for (r in seq_len(nrow(pats))) {
    s <- pats[r, ]
    d <- tryCatch(as.numeric(t(wj) %*% solve(gram + ridge, -s)), error = function(e) NULL)
    nd <- if (is.null(d)) 0 else sqrt(sum(d^2))
    if (nd <= 0) next
    b <- b_cross + step * d / nd
    resj <- rj - as.numeric(wj %*% b)
    if (max(geom$gvals(b)) <= 1e-8 && all(sign(resj) == s) && all(abs(resj) >= thr)) {
      cones[[length(cones) + 1L]] <- list(signs = as.integer(s), b = b)
    }
  }
  list(cones = cones, simultaneous_rows = simul)
}
