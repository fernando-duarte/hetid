# Deterministic synthetic fixtures for the Harvey multiplicative-variance
# log-variance checks (scripts-paper/log_variance/estimators/harvey/likelihood.R and the solver,
# recession, and constructor modules). One fixed seed builds a small triangular
# system whose squared reference residuals y(b) = (w1 - w2 b)^2 are strictly
# positive, so the Gamma(link = "log") coefficient oracle and the log-OLS
# normalization identity have clean positive data. Alongside it sit the
# exact-zero, negative-recession, zero-rate, rank-deficient, all-zero, and
# heavy-tailed variants the zero-safe and existence blocks need. Everything is
# returned in a single list `harvey_fx`; the intermediate objects stay inside
# local(). Sourced by the thin Harvey test entry point before any check file
# that reads `harvey_fx`.

harvey_fx <- local({
  set.seed(20260713L)
  n <- 120L
  k <- 2L
  w2 <- matrix(rnorm(n * k), n, k)
  pcr <- scale(matrix(rnorm(n * 4L), n, 4L), center = TRUE, scale = FALSE)
  colnames(pcr) <- paste0("l.pc", 1:4)
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))

  # Reference point with a genuine exponential-variance signal in the PCs. A
  # tiny floor keeps every reference residual nonzero, so y(b_ref) > 0 and both
  # the Gamma-log oracle and the log-OLS normalization identity are well posed.
  b_ref <- c(0.4, -0.25)
  log_h <- 0.3 + drop(pcr %*% c(0.5, -0.3, 0.2, -0.1))
  e_ref <- sqrt(exp(log_h)) * rnorm(n)
  e_ref[abs(e_ref) < 1e-3] <- 1e-3
  w1 <- drop(w2 %*% b_ref) + e_ref
  y <- drop(w1 - w2 %*% b_ref)^2

  # Exact-zero-residual variants at b_ref: forcing w1 to the fitted value on a
  # row makes e there exactly zero, y exactly zero, while the remaining
  # positive-response rows keep full column rank (single zero and three zeros).
  b_zero <- b_ref
  w1_zero <- w1
  w1_zero[1] <- drop(w2[1, ] %*% b_zero)
  w1_zeros_multi <- w1
  w1_zeros_multi[1:3] <- drop(w2[1:3, ] %*% b_zero)
  y_zero <- drop(w1_zero - w2 %*% b_zero)^2
  y_zeros_multi <- drop(w1_zeros_multi - w2 %*% b_zero)^2

  # Finalized review's full-rank negative-recession counterexample, embedded in
  # five columns. Positive rows (1, 2, 4, 5, 6) span R^5, yet d_neg keeps
  # X_+ d >= 0 while the zero row (1, 100, 0, 0, 0) has rate 1 - 100 < 0, so a
  # rank-only existence test is fooled but the cone certificate is not.
  x_neg <- rbind(
    c(1, 0, 0, 0, 0),
    c(1, 1, 0, 0, 0),
    c(1, 100, 0, 0, 0),
    c(1, 1, 1, 0, 0),
    c(1, 1, 0, 1, 0),
    c(1, 1, 0, 0, 1)
  )
  y_neg <- c(1, 1, 0, 1, 1, 1)
  d_neg <- c(1, -1, 0, 0, 0)

  # The two-column base pieces of the same counterexample, for the direct
  # objective sweep Q_H(c d) at c = 0, 1, 10, 100 (approximately 1, -48.316,
  # -489.5, -4899.5): the X_+ rows, the x_0 zero row, response, and direction.
  x_neg2 <- rbind(c(1, 0), c(1, 1), c(1, 100))
  y_neg2 <- c(1, 1, 0)
  d_neg2 <- c(1, -1)

  # Zero-rate fixture: columns two and three coincide on every row, so d_zrate
  # is a flat feasible direction (X d = 0 on all rows, including the zero row),
  # giving a recession rate of exactly zero -> unresolved/nonregular, never a
  # negative recession.
  x_zrate <- rbind(
    c(1, 1, 1),
    c(1, 2, 2),
    c(1, 3, 3),
    c(1, 0, 0)
  )
  y_zrate <- c(1, 1, 1, 0)
  d_zrate <- c(0, 1, -1)

  # Rank-deficient positive rows: the only positive responses sit on a collinear
  # block (intercept plus one shared direction), so rank(X[y > 0, ]) = 2 < 5 and
  # existence cannot be certified by the positive-row rank alone.
  x_rankdef <- x_mat
  y_rankdef <- rep(0, n)
  pos <- 1:8
  x_rankdef[pos, ] <- cbind(1, outer(seq_along(pos) * 0.5, pcr[1, ]))
  y_rankdef[pos] <- seq_along(pos) * 0.2 + 0.5

  # All-zero and heavy-tailed responses: the former must fail loudly; the latter
  # must converge via backtracking despite one element ~1e6 times the rest.
  y_all_zero <- rep(0, n)
  y_heavy <- y
  y_heavy[1] <- 1e6 * stats::median(y)

  list(
    n = n, k = k, w2 = w2, pcr = pcr, x_mat = x_mat, b_ref = b_ref,
    e_ref = e_ref, w1 = w1, y = y, qtr = seq_len(n), scale_c = 3,
    b_zero = b_zero, w1_zero = w1_zero, w1_zeros_multi = w1_zeros_multi,
    y_zero = y_zero, y_zeros_multi = y_zeros_multi,
    x_neg = x_neg, y_neg = y_neg, d_neg = d_neg,
    x_neg2 = x_neg2, y_neg2 = y_neg2, d_neg2 = d_neg2,
    x_zrate = x_zrate, y_zrate = y_zrate, d_zrate = d_zrate,
    x_rankdef = x_rankdef, y_rankdef = y_rankdef,
    y_all_zero = y_all_zero, y_heavy = y_heavy
  )
})
