# Offline crossing-geometry and tail-classifier checks for the median (LAD)
# log-variance estimator and its fitted-response domain guard. The oracles
# use quantreg directly and pass when installed; classifier/witness/guard/scale
# checks fail closed. Tail coordinate M = -2 log(|e_i| / e_scale_ref); the direct
# oracle sets z_i = -M directly (exp(-M/2) underflows before M = 10000). Assumed
# return shapes are asserted inline. Run: Rscript <this file> from the root.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))

paper_source_once(paper_path("log_variance", "estimators", "lad", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "fit.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "crossing_domain.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
have_qr <- requireNamespace("quantreg", quietly = TRUE)
qr_check <- .test$optional_check(
  have_qr,
  "quantreg unavailable"
)

# Direct-response path: for each M set z_i = -M, read the br (intercept, slope).
lad_dir_path <- function(pcr, i, ms) {
  x <- cbind("(Intercept)" = 1, pcr = pcr)
  t(vapply(ms, function(m) {
    z <- rep(0, length(pcr))
    z[i] <- -m
    suppressWarnings(quantreg::rq.fit(x, z, tau = 0.5, method = "br")$coef)
  }, numeric(2L)))
}
# A per-path trace in the classifier's shape (scalar or per-point status/sig).
lad_trace <- function(m, coef, status = "ok", sig = "s") {
  fill <- function(v) if (length(v) == length(m)) v else rep(v, length(m))
  list(m = m, coef = coef, fit_status = fill(status), active_signature = fill(sig))
}
# Engine-shaped superset context plus the crossing geometry the witness reads;
# check_feasible handles the single-constraint ball fixtures used here.
lad_ctx <- function(qs, w1, w2, x_mat, esc) {
  delta <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, delta)
  cf <- function(b) {
    g <- (drop(t(b) %*% qs$A_i[[1]] %*% b) + sum(qs$b_i[[1]] * b) + qs$c_i[1]) / omega[1]
    list(feasible = g <= 1e-10, max_violation = g)
  }
  list(
    schema_version = "1.0.0", check_feasible = cf,
    b_scales = list(delta = delta, omega = omega), cache = new.env(parent = emptyenv()),
    budget_state = logvar_budget_state(Inf), precheck = NULL,
    w1 = w1, w2 = w2, x_mat = x_mat, e_scale_ref = esc
  )
}

cx_fx <- local({
  n <- 40L
  d <- paper_test_pc_design(n, 4L, 414L)
  pcr <- d$pcr
  x_mat <- d$x_mat
  w2 <- matrix(rnorm(n * 3L), n, 3L)
  b_ref <- c(0.3, -0.2, 0.15)
  e_ref <- 1.4 * sample(c(-1, 1), n, TRUE) + rnorm(n, sd = 0.3)
  w1 <- drop(w2 %*% b_ref) + e_ref
  # K = 2 witness design (b_cross interior to ||b - c|| <= 0.5, row 1 crossing)
  # plus tangent, simultaneous-second-crossing, and thin near-point variants
  w2k <- matrix(rnorm(30 * 2L), 30L, 2L)
  b_cross <- c(0.2, -0.15)
  qsk <- list(A_i = list(diag(2)), b_i = list(-2 * b_cross), c_i = sum(b_cross^2) - 0.25)
  btabk <- data.frame(
    coef = c("b1", "b2"), set_lower = b_cross - 0.5, set_upper = b_cross + 0.5,
    status = "bounded"
  )
  w1k <- drop(w2k %*% b_cross) + 1.3 * sample(c(-1, 1), 30L, TRUE)
  w1k[1] <- drop(w2k[1, ] %*% b_cross)
  esck <- stats::median(abs(w1k - drop(w2k %*% b_cross)))
  w2t <- w2k
  w2t[1, ] <- c(1, 0)
  w1t <- w1k
  w1t[1] <- drop(w2t[1, ] %*% (b_cross + c(0.5, 0)))
  w2s <- w2k
  w2s[2, ] <- c(0, 1)
  w1s <- w1k
  w1s[2] <- drop(w2s[2, ] %*% b_cross)
  qthin <- list(A_i = list(diag(2)), b_i = list(-2 * b_cross), c_i = sum(b_cross^2) - 1e-18)
  wit <- function(i, qs, w1, w2) {
    logvar_lad_crossing_witness(i, qs, btabk, lad_ctx(qs, w1, w2, x_mat, esck))
  }
  environment()
})
ms_oracle <- seq(10, 10000, length.out = 80L)

# Pure oracles: the ordinary-leverage crossing keeps every coefficient constant;
# the high-leverage crossing has slope tracking -M/100; and an actual-residual
# high-leverage path (w2 = ones, scalar b) tracks -1/leverage over an M span
# >= 12 with representable ratios strictly above 1e-12, never reaching M = 10000.
qr_check("lad direct and actual-residual oracles track the leverage slope", {
  ord <- lad_dir_path(c(-3, -2, -1, 0, 1, 2, 3), 4L, ms_oracle)
  hi <- lad_dir_path(c(100, -1, 0, 1, 2), 1L, ms_oracle)
  x <- cbind("(Intercept)" = 1, pcr = c(80, -2, -1, -0.5, 0, 0.5, 1, 1.5, 2))
  w1 <- c(5, 1.1, -0.7, 0.4, -1.3, 0.9, -0.2, 1.7, -1)
  esc <- stats::median(abs(w1 - stats::median(w1)))
  bs <- w1[1] - exp(-seq(11, 26, length.out = 8L) / 2) * esc
  sl <- vapply(bs, function(b) {
    suppressWarnings(
      quantreg::rq.fit(x, 2 * log(abs(w1 - b)), tau = 0.5, method = "br")$coef[2]
    )
  }, numeric(1))
  mm <- -2 * log(abs(w1[1] - bs) / esc)
  max(abs(ord)) < 1e-8 && all(diff(hi[, 2]) < 0) &&
    abs(unname(stats::coef(stats::lm(hi[, 2] ~ ms_oracle))[2]) + 1 / 100) < 1e-4 &&
    abs(unname(stats::coef(stats::lm(sl ~ mm))[2]) + 1 / 80) < 5e-3 &&
    diff(range(mm)) >= 12 && min(abs(w1[1] - bs) / esc) > 1e-12 && max(mm) < 40
})

# The classifier reads the ordinary trace as stable and maps only the divergent
# high-leverage slope to its lower endpoint, never contaminating the intercept.
qr_check("lad tail classifier: ordinary stable, high-leverage lower unbounded", {
  ord <- logvar_lad_tail_classify(lad_trace(
    ms_oracle, lad_dir_path(c(-3, -2, -1, 0, 1, 2, 3), 4L, ms_oracle)
  ))
  hi <- logvar_lad_tail_classify(lad_trace(
    ms_oracle, lad_dir_path(c(100, -1, 0, 1, 2), 1L, ms_oracle)
  ))
  all(vapply(ord$coef, function(z) identical(z$status, "stable_finite"), logical(1))) &&
    identical(hi$coef[[2]]$status, "persistent_divergent_evidence") &&
    identical(hi$coef[[2]]$endpoint, "lower") &&
    identical(hi$coef[[1]]$status, "stable_finite")
})

# Negative controls (slow-finite, oscillating basis, guard-stopped) stay unresolved.
qr_check("lad tail classifier calls slow, oscillating, guard-stopped unresolved", {
  m <- seq(10, 24, length.out = 12L)
  sg <- rep(c("a", "b"), length.out = 12L)
  st <- c(rep("ok", 8L), rep("domain_failure", 4L))
  slow <- logvar_lad_tail_classify(lad_trace(m, cbind(0, -1 + exp(-(m - 10) / 3))))
  osc <- logvar_lad_tail_classify(lad_trace(m, cbind(0, 0.3 * sin(seq_along(m))), sig = sg))
  gst <- logvar_lad_tail_classify(lad_trace(m, cbind(0, -0.1 * m), status = st))
  identical(slow$coef[[2]]$status, "unresolved") &&
    identical(osc$coef[[2]]$status, "unresolved") &&
    identical(gst$coef[[2]]$status, "unresolved")
})

# Domain guard plus scale reference: the scale is positive, equivariant, and fails
# closed without a positive residual; an exact zero is outside the log domain (NA
# coef); a guarded near-zero is unresolved; the 1e-12 guard separates 1e-11/1e-13.
qr_check("lad domain guard and scale reference partition residual states", {
  s1 <- logvar_lad_scale_reference(cx_fx$e_ref)
  bad <- inherits(try(logvar_lad_scale_reference(rep(0, 8L)), silent = TRUE), "try-error")
  base <- drop(cx_fx$w2[1, ] %*% cx_fx$b_ref)
  at <- function(off) {
    w1r <- cx_fx$w1
    w1r[1] <- base + off
    logvar_lad_fit(cx_fx$b_ref, w1r, cx_fx$w2, cx_fx$x_mat, s1)
  }
  z <- at(0)
  is.finite(s1) && s1 > 0 && bad &&
    abs(logvar_lad_scale_reference(7 * cx_fx$e_ref) - 7 * s1) < 1e-8 * max(1, s1) &&
    identical(z$diagnostics$domain_state, "exact_domain_failure") && all(is.na(z$coef)) &&
    identical(
      at(1e-13 * s1)$diagnostics$domain_state, "numerically_unresolved_near_crossing"
    ) && identical(at(1e-11 * s1)$fit_status, "ok")
})

# Witness coverage in one pass: a census hit yields a verified witness (hyperplane
# <= 1e-13, feasible anchors, constraint <= 1e-8); a tangent hit has one feasible
# side; simultaneous crossings enumerate multiple sign cones; thin -> unresolved.
qr_check("lad witness coverage: verified, tangent, simultaneous, thin", {
  w <- cx_fx$wit(1L, cx_fx$qsk, cx_fx$w1k, cx_fx$w2k)
  wt <- cx_fx$wit(1L, cx_fx$qsk, cx_fx$w1t, cx_fx$w2t)
  ws <- cx_fx$wit(1L, cx_fx$qsk, cx_fx$w1s, cx_fx$w2s)
  wth <- cx_fx$wit(1L, cx_fx$qthin, cx_fx$w1k, cx_fx$w2k)
  identical(w$status, "verified") && length(w$anchors) >= 1L &&
    abs(drop(cx_fx$w1k[1] - cx_fx$w2k[1, ] %*% w$b_cross)) / cx_fx$esck <= 1e-13 &&
    isTRUE(w$constraint_violation <= 1e-8) && isTRUE(wt$n_feasible_sides == 1L) &&
    isTRUE(length(ws$sign_cones) >= 2L) && identical(wth$status, "unresolved_witness")
})

.test$finish()
