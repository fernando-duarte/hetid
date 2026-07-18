# Deterministic synthetic fixtures for the PPML log-variance checks
# (scripts-paper/log_variance/estimators/ppml/fit.R and estimator.R). One fixed seed
# builds a small triangular system whose squared reference residuals y(b) =
# (w1 - w2 b)^2 are strictly positive, plus the pathological variants the
# zero/existence and dimension-generic blocks need. Everything is returned in a
# single list `ppml_fx`; the intermediate objects stay inside local(). Sourced
# by the thin PPML test entry point before any check file that reads `ppml_fx`.

ppml_fx <- local({
  list2env(
    paper_test_variance_fixture(),
    environment()
  )

  # Reference point with a genuine exponential-mean signal in the PCs, so the
  # quasi-Poisson fit is well conditioned at response_scale = 1. A tiny floor
  # keeps every reference residual nonzero (positive log(e^2) for the oracle).

  # Exact-zero-residual variants at b_ref: forcing w1 to the fitted value on a
  # row makes e there exactly zero, y exactly zero, while the remaining
  # positive-response rows keep full column rank.

  # Rank-deficient positive-response rows: the only positive responses sit on a
  # collinear block of the design (intercept plus one shared direction), so
  # rank(X[y > 0, ]) = 2 < ncol(x_mat) and existence cannot be certified.

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
