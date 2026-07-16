# Offline mathematical, equivalence, normalization, and score-unit checks for
# the Harvey multiplicative-variance estimator (scripts-paper/
# likelihood.R and the response fit in estimator.R). The
# analytic score is the central FD of the objective; the observed information is
# the FD of the score; the moment primitive is exactly -2 times the score; the
# normal log-chi-square gap constant matches -log 2 - digamma(1/2); the
# zero-safe ratio never forms 0 * Inf at a zero response; the Gamma(link =
# "log") root and a hand-computed fixed-dispersion-2 Gamma log-likelihood
# difference reproduce the Harvey coefficients and objective gaps; scaling the
# response by c shifts only the intercept by 2 log|c|; the Gaussian
# normalization gap is +1.270362845 (Harvey larger); and the per-coordinate
# score bound is column-scaled by colSums(abs(X)) alone. Harvey
# modules do not exist yet, so every module-dependent assertion is guarded by
# safe() so one missing function cannot abort the suite. The entry point sources
# the fixtures and defines check().

fx <- harvey_fx
x_mat <- fx$x_mat
y <- fx$y
y_zero <- fx$y_zero

# Return FALSE instead of aborting when a required Harvey function errors.
safe <- function(expr) tryCatch(expr, error = function(e) FALSE)

# Central-difference gradient of the objective, per coordinate, from the module.
fd_grad <- function(theta, yv, x, h = 1e-5) {
  vapply(seq_along(theta), function(j) {
    tp <- theta
    tm <- theta
    tp[j] <- tp[j] + h
    tm[j] <- tm[j] - h
    (logvar_harvey_objective(tp, yv, x) - logvar_harvey_objective(tm, yv, x)) / (2 * h)
  }, numeric(1))
}

# Central-difference Jacobian of the score, i.e. the observed Hessian of Q_H.
fd_hess <- function(theta, yv, x, h = 1e-5) {
  p <- length(theta)
  out <- matrix(0, p, p)
  for (j in seq_len(p)) {
    tp <- theta
    tm <- theta
    tp[j] <- tp[j] + h
    tm[j] <- tm[j] - h
    out[, j] <- (logvar_harvey_score(tp, yv, x) - logvar_harvey_score(tm, yv, x)) / (2 * h)
  }
  out
}

# Fixed-dispersion Gamma(shape 1/2, scale 2h) log-likelihood; its theta
# differences equal -(Q_H differences) because the y-only terms cancel.
gamma_loglik <- function(theta, yv, x) {
  h <- exp(drop(x %*% theta))
  sum(-0.5 * log(2 * h) - yv / (2 * h) - 0.5 * log(yv) - lgamma(0.5))
}

# Mathematical block ---------------------------------------------------------

check("hm_score matches central FD of the objective at b_ref", safe({
  theta <- c(log(mean(y)), 0.1, -0.05, 0.08, -0.03)
  an <- logvar_harvey_score(theta, y, x_mat)
  max(abs(drop(an) - fd_grad(theta, y, x_mat))) <= 1e-5 * max(1, max(abs(an)))
}))

check("hm_score matches central FD of the objective at an exact zero", safe({
  theta <- c(log(mean(y_zero)), 0.1, -0.05, 0.08, -0.03)
  an <- logvar_harvey_score(theta, y_zero, x_mat)
  max(abs(drop(an) - fd_grad(theta, y_zero, x_mat))) <= 1e-5 * max(1, max(abs(an)))
}))

# The Hessian of Q_H is 0.5 X' diag(r) X = logvar_harvey_info (not 2 * info).
check("hm_observed information matches FD of the score", safe({
  theta <- c(log(mean(y)), 0.1, -0.05, 0.08, -0.03)
  an <- logvar_harvey_info(theta, y, x_mat)
  max(abs(an - fd_hess(theta, y, x_mat))) <= 1e-5 * max(1, max(abs(an)))
}))

check("hm_moment equals minus twice the score", safe({
  theta <- c(log(mean(y)), 0.1, -0.05, 0.08, -0.03)
  mo <- logvar_harvey_moment(theta, y, x_mat)
  sc <- logvar_harvey_score(theta, y, x_mat)
  max(abs(drop(mo) - drop(-2 * sc))) <= 1e-10 * max(1, max(abs(mo)))
}))

check("hm_normal log-chi-square gap equals -log2 - digamma(1/2)", safe({
  abs(logvar_normal_lnchisq_gap - (-log(2) - digamma(0.5))) < 1e-12 &&
    abs(logvar_normal_lnchisq_gap - 1.270362845) < 1e-9
}))

check("hm_objective adds only eta/2 on the zero row", safe({
  theta <- c(log(mean(y_zero)), 0.2, -0.1, 0.05, -0.05)
  r <- logvar_harvey_ratio(theta, y_zero, x_mat)
  eta <- drop(x_mat %*% theta)
  pos <- y_zero > 0
  hand <- 0.5 * (sum(eta) + sum(r[pos]))
  abs(logvar_harvey_objective(theta, y_zero, x_mat) - hand) <= 1e-9
}))

check("hm_ratio is zero-safe at moderately negative eta", safe({
  r <- logvar_harvey_ratio(c(-30, 0, 0, 0, 0), y_zero, x_mat)
  is.numeric(r) && !anyNA(r) &&
    all(r[y_zero == 0] == 0) && all(is.finite(r[y_zero > 0]))
}))

check("hm_ratio keeps zero rows exact under extreme negative eta", safe({
  r <- logvar_harvey_ratio(c(-600, 0, 0, 0, 0), y_zero, x_mat)
  is.numeric(r) && !anyNA(r) && all(r[y_zero == 0] == 0)
}))

# Equivalence block (positive-data test oracle) ------------------------------

check("hm_Harvey coefficients match the Gamma(log) oracle on positive data", safe({
  fit <- logvar_harvey_fit_response(y, x_mat)
  oracle <- stats::glm.fit(
    x_mat, y,
    family = stats::Gamma(link = "log"),
    control = stats::glm.control(maxit = 100L, epsilon = 1e-12)
  )
  max(abs(unname(fit$coef) - unname(oracle$coefficients))) < 1e-6
}))

check("hm_objective differences equal the fixed-dispersion Gamma gaps", safe({
  th1 <- logvar_harvey_fit_response(y, x_mat)$coef
  th2 <- th1 + c(0.12, -0.08, 0.05, 0.1, -0.04)
  ll_diff <- gamma_loglik(th1, y, x_mat) - gamma_loglik(th2, y, x_mat)
  q_diff <- logvar_harvey_objective(th1, y, x_mat) -
    logvar_harvey_objective(th2, y, x_mat)
  abs(ll_diff + q_diff) <= 1e-8 * max(1, abs(q_diff))
}))

check("hm_scaling the response by c shifts only the intercept by 2 log|c|", safe({
  cc <- fx$scale_c
  f1 <- logvar_harvey_fit_response(y, x_mat)
  f2 <- logvar_harvey_fit_response(cc^2 * y, x_mat)
  abs((f2$coef[1] - f1$coef[1]) - 2 * log(abs(cc))) < 1e-6 &&
    max(abs(f2$coef[-1] - f1$coef[-1])) < 1e-6
}))

# Normalization block (pinned Gaussian design) -------------------------------

norm_dat <- local({
  set.seed(42L)
  n <- 20000L
  r_des <- scale(matrix(rnorm(n * 4L), n, 4L), center = TRUE, scale = FALSE)
  beta <- c(0.3, -0.2, 0.1, 0.05)
  e <- sqrt(exp(-1 + drop(r_des %*% beta))) * rnorm(n)
  list(xg = cbind(1, r_des), e2 = e^2, beta = beta)
})

check("hm_Harvey and PPML slopes agree on the pinned Gaussian design", safe({
  fh <- logvar_harvey_fit_response(norm_dat$e2, norm_dat$xg)
  fp <- logvar_ppml_fit_response(norm_dat$e2, norm_dat$xg)
  max(abs(unname(fh$coef[-1]) - unname(fp$coef[-1]))) <= 0.02
}))

check("hm_Harvey minus log-OLS intercept is the +1.270362845 gap", safe({
  fh <- logvar_harvey_fit_response(norm_dat$e2, norm_dat$xg)
  ols <- stats::lm.fit(norm_dat$xg, log(norm_dat$e2))$coefficients
  gap <- unname(fh$coef[1]) - unname(ols[1])
  abs(gap - 1.270362845) <= 0.02 && gap > 0
}))

# Score-unit block -----------------------------------------------------------

check("hm_score bound uses colSums(abs(X)) with no response factor", safe({
  fit <- logvar_harvey_fit_response(y, x_mat)
  mo <- logvar_harvey_moment(fit$coef, y, x_mat)
  bound <- colSums(abs(x_mat))
  max(abs(drop(mo)) / bound) <= 1e-8
}))

check("hm_rescaling a regressor column preserves acceptance and coefficients", safe({
  fit <- logvar_harvey_fit_response(y, x_mat)
  x_big <- x_mat
  x_big[, 2] <- x_big[, 2] * 1e8
  fb <- logvar_harvey_fit_response(y, x_big)
  isTRUE(fb$converged) &&
    abs(fb$coef[2] * 1e8 - fit$coef[2]) < 1e-5 * max(1, abs(fit$coef[2])) &&
    max(abs(fb$coef[-2] - fit$coef[-2])) < 1e-5
}))

# Pure algebra (no module): the column-scaled bound scales with the column.
check("hm_column rescaling scales the score bound proportionally", {
  x_big <- x_mat
  x_big[, 2] <- x_big[, 2] * 1e8
  b1 <- colSums(abs(x_mat))
  b2 <- colSums(abs(x_big))
  abs(b2[2] - 1e8 * b1[2]) <= 1e-3 * b2[2] && max(abs(b2[-2] - b1[-2])) < 1e-8
})
