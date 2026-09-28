# Verified outer bounds from one nonnegative constraint combination.
#
# For weights v >= 0, every x in the feasible set satisfies
# x'Mx + B'x + C <= 0 with M = sum v_i A_i. With any centre z, u = x - z,
# rho = B + 2Mz and Q(z), the Lagrangian bound over a scalar rescaling
# t > 0 has the closed form l'z + sqrt(a r) + sqrt(a) rho / (2 sqrt(lambda)),
# where a >= l'M^-1 l, lambda <= lambda_min(M) and r = -Q(z) + rho^2/(4 lambda).
# Every computed quantity carries a forward-error bound, so optimizer output
# and the centre are candidates only. A nonfinite intermediate result returns
# NULL (unknown), never a bound

outer_gamma <- function(k) {
  u <- .Machine$double.eps / 2
  k * u / (1 - k * u)
}

# Base-2 exponent k of max(abs(x)); x/2^k is exact unless a nonzero entry falls below normal range
# floor(log2(double.xmax)) = 1024, so clamp k to keep 2^k normal
outer_pow2_exponent <- function(x) {
  top <- max(abs(x))
  if (top == 0) {
    return(0)
  }
  min(max(floor(log2(top)), -1022), 1023)
}

outer_pow2_scale <- function(x, k) {
  scaled <- x / 2^k
  lost <- x != 0 & abs(scaled) < .Machine$double.xmin
  if (any(lost)) NULL else scaled
}

# Euclidean norm with an upper-bound guarantee and no overflow of squares
outer_norm2 <- function(x) {
  top <- max(abs(x))
  if (!is.finite(top)) {
    return(Inf)
  }
  if (top == 0) {
    return(0)
  }
  n <- length(x)
  top * sqrt(sum((x / top)^2)) * (1 + outer_gamma(n + 4)) + n * .Machine$double.xmin
}

# Rows rescaled by powers of two leave the feasible set unchanged exactly
outer_normalize_system <- function(quadratic) {
  m <- length(quadratic$c_i)
  out <- list(
    A = vector("list", m), b = vector("list", m), c = numeric(m),
    k = numeric(m), n = nrow(quadratic$A_i[[1L]]), m = m
  )
  for (i in seq_len(m)) {
    k <- outer_pow2_exponent(c(quadratic$A_i[[i]], quadratic$b_i[[i]], quadratic$c_i[i]))
    a <- outer_pow2_scale(quadratic$A_i[[i]], k)
    b <- outer_pow2_scale(quadratic$b_i[[i]], k)
    constant <- outer_pow2_scale(quadratic$c_i[i], k)
    if (is.null(a) || is.null(b) || is.null(constant)) {
      return(NULL)
    }
    out$A[[i]] <- a
    out$b[[i]] <- b
    out$c[i] <- constant
    out$k[i] <- k
  }
  out
}

outer_weights_ok <- function(v, m) {
  length(v) == m && all(is.finite(v)) && all(v >= 0) && sum(v) > 0
}

# The weighted Hessian and linear term with their rounding bounds, and a
# lower bound on the exact combination's smallest eigenvalue
outer_combination <- function(sys, v) {
  n <- sys$n
  m <- sys$m
  tiny <- .Machine$double.xmin
  g <- outer_gamma(n * n + n + m + 4)
  mat <- Reduce(`+`, Map(`*`, sys$A, v))
  abs_m <- Reduce(`+`, Map(function(a, w) abs(a) * w, sys$A, v)) * (1 + g)
  lin <- Reduce(`+`, Map(`*`, sys$b, v))
  abs_b <- Reduce(`+`, Map(function(b, w) abs(b) * w, sys$b, v)) * (1 + g)
  e_m <- outer_gamma(m + 1) * outer_norm2(abs_m) * (1 + g) + m * tiny
  eig <- eigen(mat, symmetric = TRUE)
  safety <- HETID_CONSTANTS$QUADRATIC_OUTER_FACTOR * n * .Machine$double.eps
  lam_t <- min(eig$values) - safety * outer_norm2(mat) - n * tiny
  list(
    mat = mat, abs_m = abs_m, lin = lin, abs_b = abs_b, e_m = e_m,
    eig = eig, lam_t = lam_t, lam = lam_t - e_m, g = g
  )
}

# Squared radius bound r_bar of the containing ellipsoid about centre z
outer_radius <- function(sys, v, z, comb) {
  m <- sys$m
  tiny <- .Machine$double.xmin
  abs_z <- abs(z)
  vals <- vapply(seq_len(m), function(i) {
    sum(z * drop(sys$A[[i]] %*% z)) + sum(sys$b[[i]] * z) + sys$c[i]
  }, numeric(1))
  mags <- vapply(seq_len(m), function(i) {
    sum(abs_z * drop(abs(sys$A[[i]]) %*% abs_z)) + sum(abs(sys$b[[i]]) * abs_z) +
      abs(sys$c[i])
  }, numeric(1))
  q_value <- sum(v * vals)
  e_q <- 2 * comb$g * sum(v * (mags + abs(vals))) + (m + 1) * tiny
  rho <- comb$lin + 2 * drop(comb$mat %*% z)
  e_rho <- 2 * comb$g * (comb$abs_b + 2 * drop(comb$abs_m %*% abs_z)) +
    2 * comb$e_m * abs_z + tiny
  rho_bar <- outer_norm2(abs(rho) + e_rho) * (1 + comb$g)
  rterm <- rho_bar / (2 * sqrt(comb$lam))
  r_bar <- -q_value + e_q + rterm^2
  if (!all(is.finite(c(q_value, e_q, rho_bar, rterm, r_bar)))) {
    return(NULL)
  }
  list(r_bar = r_bar, rterm = rterm, e_q = e_q, rho_bar = rho_bar, empty = r_bar < 0)
}

# Verify one simplex weight vector on normalized rows for pieces reused by every objective bound
# Return NULL if the combination is not verified positive definite or any intermediate is nonfinite
outer_verify <- function(sys, v) {
  if (!outer_weights_ok(v, sys$m)) {
    return(NULL)
  }
  v <- v / max(v)
  v <- v / sum(v)
  comb <- outer_combination(sys, v)
  if (!is.finite(comb$lam) || comb$lam <= 0 || !(comb$e_m / comb$lam_t < 0.5)) {
    return(NULL)
  }
  vectors <- comb$eig$vectors
  solve_m <- function(rhs) vectors %*% (crossprod(vectors, rhs) / comb$eig$values)
  z <- -drop(solve_m(comb$lin)) / 2
  if (any(!is.finite(z))) {
    return(NULL)
  }
  radius <- outer_radius(sys, v, z, comb)
  if (is.null(radius)) {
    return(NULL)
  }
  c(list(
    v = v, mat = comb$mat, abs_m = abs(comb$mat), solve_m = solve_m,
    lam = comb$lam, lam_t = comb$lam_t, eta = comb$e_m / comb$lam_t, z = z,
    g = comb$g
  ), radius)
}

# Bounds of every column of a normalized loading matrix under one verified
# candidate. Columns with a nonfinite result are NA (unknown)
outer_candidate_bounds <- function(cand, scaled) {
  eps <- .Machine$double.eps
  tiny <- .Machine$double.xmin
  factor_eps <- HETID_CONSTANTS$QUADRATIC_OUTER_FACTOR * eps
  g <- cand$g
  y <- cand$solve_m(scaled)
  res <- scaled - cand$mat %*% y
  e_res <- g * (abs(scaled) + cand$abs_m %*% abs(y)) + tiny
  res_bar <- apply(abs(res) + e_res, 2L, outer_norm2)
  l_norm <- apply(scaled, 2L, outer_norm2)
  a_t <- colSums(scaled * y) + g * colSums(abs(scaled) * abs(y)) +
    l_norm * res_bar / cand$lam_t
  root_a <- sqrt(pmax(a_t, 0) / (1 - cand$eta))
  half <- (root_a * sqrt(max(cand$r_bar, 0)) + root_a * cand$rterm) * (1 + factor_eps)
  centre <- colSums(scaled * cand$z)
  e_centre <- g * colSums(abs(scaled) * abs(cand$z)) + abs(centre) * eps + tiny
  pad <- factor_eps * (abs(centre) + half) + tiny
  lower <- centre - e_centre - half - pad
  upper <- centre + e_centre + half + pad
  bad <- !is.finite(lower) | !is.finite(upper)
  lower[bad] <- NA_real_
  upper[bad] <- NA_real_
  list(lower = lower, upper = upper)
}
