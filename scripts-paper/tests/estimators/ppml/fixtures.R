# Deterministic synthetic fixtures for the PPML log-variance checks
# (scripts-paper/log_variance/estimators/ppml/fit.R and estimator.R). One fixed seed
# builds a small triangular system whose squared reference residuals y(b) =
# (w1 - w2 b)^2 are strictly positive, plus the pathological variants the
# zero/existence and dimension-generic blocks need. Everything is returned in a
# single list `ppml_fx`; the intermediate objects stay inside local(). Sourced
# by the thin PPML test entry point before any check file that reads `ppml_fx`.

ppml_fx <- local({
  set.seed(20260713L)
  n <- 120L
  k <- 2L
  w2 <- matrix(rnorm(n * k), n, k)
  pcr <- scale(matrix(rnorm(n * 4L), n, 4L), center = TRUE, scale = FALSE)
  colnames(pcr) <- paste0("l.pc", 1:4)
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))

  # Reference point with a genuine exponential-mean signal in the PCs, so the
  # quasi-Poisson fit is well conditioned at response_scale = 1. A tiny floor
  # keeps every reference residual nonzero (positive log(e^2) for the oracle).
  b_ref <- c(0.4, -0.25)
  log_h <- 0.3 + drop(pcr %*% c(0.5, -0.3, 0.2, -0.1))
  e_ref <- sqrt(exp(log_h)) * rnorm(n)
  e_ref[abs(e_ref) < 1e-3] <- 1e-3
  w1 <- drop(w2 %*% b_ref) + e_ref
  y <- drop(w1 - w2 %*% b_ref)^2

  # Exact-zero-residual variants at b_ref: forcing w1 to the fitted value on a
  # row makes e there exactly zero, y exactly zero, while the remaining
  # positive-response rows keep full column rank.
  b_zero <- b_ref
  w1_zero <- w1
  w1_zero[1] <- drop(w2[1, ] %*% b_zero)
  w1_zeros_multi <- w1
  w1_zeros_multi[1:3] <- drop(w2[1:3, ] %*% b_zero)

  # Rank-deficient positive-response rows: the only positive responses sit on a
  # collinear block of the design (intercept plus one shared direction), so
  # rank(X[y > 0, ]) = 2 < ncol(x_mat) and existence cannot be certified.
  x_rankdef <- x_mat
  y_rankdef <- rep(0, n)
  pos <- 1:8
  x_rankdef[pos, ] <- cbind(1, outer(seq_along(pos) * 0.5, pcr[1, ]))
  y_rankdef[pos] <- seq_along(pos) * 0.2 + 0.5

  # Dimension-generic (p = 3) design reusing the same b path, for the
  # intercept-only fallback c(log(mean(y)), rep(0, p - 1)) and a 3 x K Jacobian.
  x3 <- cbind(1, pcr[, 1:2])
  colnames(x3) <- c("(Intercept)", "l.pc1", "l.pc2")

  list(
    n = n, k = k, w2 = w2, pcr = pcr, x_mat = x_mat, b_ref = b_ref,
    e_ref = e_ref, w1 = w1, y = y, qtr = seq_len(n), scale_s = 7,
    b_zero = b_zero, w1_zero = w1_zero, w1_zeros_multi = w1_zeros_multi,
    x_rankdef = x_rankdef, y_rankdef = y_rankdef, x3 = x3
  )
})
