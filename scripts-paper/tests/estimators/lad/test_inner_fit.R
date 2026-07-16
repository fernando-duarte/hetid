# Offline inner-fit checks for the median (LAD) log-variance estimator: the
# br-selected response fit and its nonuniqueness probe
# (scripts-paper/log_variance/estimators/lad/fit.R and estimator.R). A standalone entry
# point mirroring the Harvey suite: source the map/engine/log-OLS layers,
# source the LAD modules, define check() plus the quantreg-gated qr_check()
# wrapper, then run the inner checks. The pure br-vs-LP and br-vs-fn oracles use quantreg
# directly and pass when it is installed. The file prints a skip line per
# quantreg-dependent check when quantreg is unavailable. Run from the worktree
# root:
#   Rscript scripts-paper/tests/estimators/lad/test_inner_fit.R
#
# Stable interfaces exercised here:
#   logvar_lad_fit_response(z, x_mat) returns the estimator contract list with a
#     named length-5 coef, fit_status/converged/objective, score_norm = NA,
#     warm_start = NULL, and diagnostics$multiple_solution_sensitive.
#   logvar_lad_fit(b, w1, w2, x_mat, e_scale_ref) forms e = w1 - w2 b and
#     z = 2 log|e| before delegating; x_mat already carries the intercept.
#   logvar_lad_scale_reference(e_ref) freezes the positive residual scale.
#   logvar_lad_nonunique_probe(fit, z, x_mat) exposes objective_br, objective_fn,
#     coef_max_diff, and multiple_solution_sensitive.
#   logvar_lad_estimator(w1, w2, pcr, qtr) aligns by qtr; $fit_at_b(b) returns
#     the length-5 coef.

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("support", "identification", "profile_solver_core.R"))
source(paper_path("support", "identification", "profile_bounds_api.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))

source(paper_path("log_variance", "estimators", "lad", "estimator.R"))
source(paper_path("log_variance", "estimators", "lad", "fit.R"))

.pass <- 0L
.fail <- 0L
.skip <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}
have_qr <- requireNamespace("quantreg", quietly = TRUE)
# Run a quantreg-dependent check, forcing the condition inside a tryCatch so a
# missing LAD module fails the check closed instead of aborting the suite; when
# quantreg is absent, print a skip line and move on.
qr_check <- function(label, cond) {
  if (!have_qr) {
    .skip <<- .skip + 1L
    cat(sprintf("SKIP  %s (quantreg unavailable)\n", label))
    return(invisible())
  }
  check(label, isTRUE(tryCatch(cond, error = function(e) FALSE)))
}

# Direct linear-program minimizer: the br vertex equals an exact fit through p
# rows minimizing the summed absolute deviation, so enumerate the p-subsets.
lad_lp <- function(x, y) {
  cb <- utils::combn(nrow(x), ncol(x))
  best <- NULL
  obj <- Inf
  for (cc in seq_len(ncol(cb))) {
    co <- tryCatch(solve(x[cb[, cc], , drop = FALSE], y[cb[, cc]]),
      error = function(e) NULL
    )
    if (is.null(co)) next
    o <- sum(abs(y - x %*% co))
    if (o < obj - 1e-12) {
      obj <- o
      best <- co
    }
  }
  list(coef = best, obj = obj)
}

lad_fx <- local({
  set.seed(20260714L)
  # a tiny exactly-identified design where the br vertex equals the LP fit
  x_tiny <- cbind("(Intercept)" = 1, pcr = c(0, 1, 2, 3, 5))
  z_tiny <- c(0.2, 1.1, 1.9, 3.2, 40)
  # a moderate design for the response shift/scale identities
  n <- 40L
  pcr <- scale(matrix(rnorm(n * 4L), n, 4L), center = TRUE, scale = FALSE)
  colnames(pcr) <- paste0("l.pc", 1:4)
  x_mat <- cbind("(Intercept)" = 1, pcr)
  qtr <- seq_len(n)
  w2 <- matrix(rnorm(n * 3L), n, 3L)
  b_ref <- c(0.3, -0.2, 0.15)
  e_ref <- 1.4 * sample(c(-1, 1), n, TRUE) + rnorm(n, sd = 0.3)
  w1 <- drop(w2 %*% b_ref) + e_ref
  z_mod <- 2 * log(abs(e_ref))
  # an intercept-only even sample: the median is any value in [z2, z3], so the
  # br vertex and the fn centroid tie on the objective yet select apart
  x_nu <- matrix(1, 4L, 1L, dimnames = list(NULL, "(Intercept)"))
  z_nu <- c(1, 2, 4, 8)
  environment()
})

# Pure oracle: the br vertex reproduces the enumerated LP minimizer.
qr_check("lad br equals the direct LP minimizer on the tiny design", {
  fit <- quantreg::rq.fit(lad_fx$x_tiny, lad_fx$z_tiny, tau = 0.5, method = "br")
  max(abs(unname(fit$coef) - lad_lp(lad_fx$x_tiny, lad_fx$z_tiny)$coef)) < 1e-6
})

# Pure oracle: the even intercept-only sample is genuinely nonunique.
qr_check("lad br vertex and fn centroid tie yet differ on a nonunique design", {
  br <- suppressWarnings(
    quantreg::rq.fit(lad_fx$x_nu, lad_fx$z_nu, tau = 0.5, method = "br")
  )
  fn <- quantreg::rq.fit(lad_fx$x_nu, lad_fx$z_nu, tau = 0.5, method = "fn")
  ob <- sum(abs(lad_fx$z_nu - br$coef[1]))
  of <- sum(abs(lad_fx$z_nu - fn$coef[1]))
  abs(ob - of) < 1e-8 && abs(br$coef[1] - fn$coef[1]) > 1e-3
})

# The response fit reproduces the LP vertex through the estimator contract.
qr_check("lad fit_response matches the direct LP vertex", {
  fit <- logvar_lad_fit_response(lad_fx$z_tiny, lad_fx$x_tiny)
  max(abs(unname(fit$coef) - lad_lp(lad_fx$x_tiny, lad_fx$z_tiny)$coef)) < 1e-6
})

# Adding a constant to every response shifts only the intercept.
qr_check("lad adding a constant to z shifts only the intercept", {
  f1 <- logvar_lad_fit_response(lad_fx$z_mod, lad_fx$x_mat)
  f2 <- logvar_lad_fit_response(lad_fx$z_mod + 3.7, lad_fx$x_mat)
  abs((f2$coef[1] - f1$coef[1]) - 3.7) < 1e-6 &&
    max(abs(f2$coef[-1] - f1$coef[-1])) < 1e-6
})

# Multiplying residuals by c adds 2 log|c| to the intercept (z = 2 log|e|).
qr_check("lad scaling residuals by c adds 2 log|c| to the intercept", {
  cc <- 2.5
  esc <- logvar_lad_scale_reference(lad_fx$e_ref)
  f1 <- logvar_lad_fit(lad_fx$b_ref, lad_fx$w1, lad_fx$w2, lad_fx$x_mat, esc)
  f2 <- logvar_lad_fit(
    lad_fx$b_ref, cc * lad_fx$w1, cc * lad_fx$w2, lad_fx$x_mat, cc * esc
  )
  abs((f2$coef[1] - f1$coef[1]) - 2 * log(abs(cc))) < 1e-6 &&
    max(abs(f2$coef[-1] - f1$coef[-1])) < 1e-6
})

# Row reordering before the qtr join leaves the aligned point fit unchanged.
qr_check("lad a row reorder before the qtr join leaves the fit unchanged", {
  o <- sample(lad_fx$n)
  pcs <- lad_fx$pcr[o, ]
  colnames(pcs) <- colnames(lad_fx$pcr)
  ea <- logvar_lad_estimator(lad_fx$w1, lad_fx$w2, lad_fx$pcr, lad_fx$qtr)
  eb <- logvar_lad_estimator(
    lad_fx$w1[o], lad_fx$w2[o, , drop = FALSE], pcs, lad_fx$qtr[o]
  )
  max(abs(unname(ea$fit_at_b(lad_fx$b_ref)$coef) -
    unname(eb$fit_at_b(lad_fx$b_ref)$coef))) < 1e-8
})

# The fn probe ties on the objective, differs on coefficients, and fires the
# multiple-solution diagnostic on the nonunique design.
qr_check("lad nonunique probe fires multiple_solution_sensitive", {
  fit <- logvar_lad_fit_response(lad_fx$z_nu, lad_fx$x_nu)
  pr <- logvar_lad_nonunique_probe(fit, lad_fx$z_nu, lad_fx$x_nu)
  abs(pr$objective_br - pr$objective_fn) <= 1e-8 * max(1, abs(pr$objective_br)) &&
    pr$coef_max_diff > 1e-4 && isTRUE(pr$multiple_solution_sensitive)
})

# The response fit carries the full estimator contract: a named length-5 coef, the
# non-comparable LAD objective, score_norm = NA, no warm start, and the slim
# diagnostics list holding the nonuniqueness flag.
qr_check("lad fit_response returns the estimator-engine contract fields", {
  fit <- logvar_lad_fit_response(lad_fx$z_mod, lad_fx$x_mat)
  length(fit$coef) == 5L && !is.null(names(fit$coef)) &&
    identical(fit$fit_status, "ok") && isTRUE(fit$converged) &&
    is.na(fit$score_norm) && is.null(fit$warm_start) &&
    is.numeric(fit$objective) &&
    !is.null(fit$diagnostics$multiple_solution_sensitive)
})

cat(sprintf("\n%d passed, %d failed, %d skipped\n", .pass, .fail, .skip))
if (.fail > 0L) quit(status = 1L)
