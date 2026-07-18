# Coverage-gate and pilot checks for the PPML driver helpers: the atomic
# union/demotion apply (a primary miss unioned with a coverage find and
# demoted; a failed coverage run demoting without fabricating a candidate;
# a status mismatch demoting while keeping the primary value), the pilot's
# instability triggers, and the theta_var coefficient axis on a fail-closed
# engine schema. Sourced by test_ppml.R after the engine checks,
# whose peng_* fixtures this file reuses.

# a synthetic primary/coverage engine-result pair over two taus: builders
# keep every schema column the apply step rewrites
pcov_schema <- function(lower, upper, lo_st, up_st, tau) {
  n <- length(lower)
  s <- data.frame(
    coef = paste0("t", seq_len(n) - 1L), lower = lower, upper = upper,
    lower_status = lo_st, upper_status = up_st,
    lower_fit_status = "ok", upper_fit_status = "ok",
    fit_failure_count = 0L, lower_constraint_residual = 0,
    upper_constraint_residual = 0, estimator = "ppml",
    target_functional = "theta_var", sample_id = "s", tau = tau,
    lower_provenance = "grid", upper_provenance = "polish",
    stringsAsFactors = FALSE
  )
  s$arg_lower <- I(rep(list(c(0, 0)), n))
  s$arg_upper <- I(rep(list(c(0.1, 0.1)), n))
  s
}
pcov_res <- function(schema) {
  list(
    schema = schema, table = data.frame(
      coef = schema$coef, set_lower = schema$lower, set_upper = schema$upper,
      status = "bounded", stringsAsFactors = FALSE
    ), n_cross = NA_integer_, n_feasible = 10L, domain_info = NULL,
    diagnostics = list()
  )
}
pcov_key <- paper_tau_key(0.05)
pcov_primary <- setNames(
  list(pcov_res(pcov_schema(c(-1, -2), c(1, 2), "bounded", "bounded", 0.05))),
  pcov_key
)

# a coverage find beyond the primary lower endpoint: the union keeps the
# more extreme certified value with coverage provenance and the moved side
# demotes to unreliable, never silently narrowing
pcov_find <- setNames(list(list(ok = TRUE, res = pcov_res(
  pcov_schema(c(-1.5, -2), c(1, 2), "bounded", "bounded", 0.05)
))), pcov_key)
pcov_find[[pcov_key]]$res$diagnostics$selector <- list(
  selector_id = LOGVAR_PPML_COVERAGE_PROTOCOL$selector_id,
  traversal = LOGVAR_PPML_COVERAGE_PROTOCOL$traversal,
  n_selector_input = 20L,
  n_selector_output = 10L
)
pcov_a <- logvar_ppml_apply_coverage(
  pcov_primary, pcov_find,
  selector_protocol = LOGVAR_PPML_COVERAGE_PROTOCOL
)
pcov_row <- pcov_a$audit[pcov_a$audit$coef == "t0" & pcov_a$audit$side == "lower", ]
check(
  "a coverage-found more-extreme endpoint is unioned and demoted",
  identical(pcov_a$results[[pcov_key]]$schema$lower[1], -1.5) &&
    identical(pcov_row$final_status, "unreliable") &&
    identical(pcov_row$reason, "endpoint_moved") &&
    identical(pcov_row$source, "coverage") &&
    grepl("coverage", pcov_a$results[[pcov_key]]$schema$lower_provenance[1])
)
check(
  "an agreeing side stays bounded with its primary value",
  identical(pcov_a$results[[pcov_key]]$schema$upper[2], 2) &&
    identical(
      pcov_a$audit[pcov_a$audit$coef == "t1" &
        pcov_a$audit$side == "upper", ]$final_status, "bounded"
    )
)

# a failed coverage run demotes every bounded primary side without
# fabricating a candidate value
pcov_fail <- setNames(list(list(ok = FALSE, error = "budget")), pcov_key)
pcov_b <- logvar_ppml_apply_coverage(
  pcov_primary, pcov_fail,
  selector_protocol = LOGVAR_PPML_COVERAGE_PROTOCOL
)
check(
  "a failed coverage run demotes bounded sides and keeps primary values",
  all(pcov_b$results[[pcov_key]]$schema$lower_status == "unreliable") &&
    all(pcov_b$results[[pcov_key]]$schema$upper_status == "unreliable") &&
    identical(pcov_b$results[[pcov_key]]$schema$lower, c(-1, -2)) &&
    all(pcov_b$audit$reason == "coverage_run_failed")
)
# a status mismatch (primary bounded, coverage unbounded) demotes and keeps
# the primary value; a non-bounded primary side keeps its own status word
pcov_mix <- pcov_schema(
  c(-Inf, -2), c(1, 2), c("unbounded", "bounded"),
  c("bounded", "bounded"), 0.05
)
pcov_c <- logvar_ppml_apply_coverage(
  setNames(list(pcov_res(pcov_mix)), pcov_key),
  setNames(list(list(ok = TRUE, res = pcov_res(
    pcov_schema(c(-1, -2), c(1, 2), "bounded", "bounded", 0.05)
  ))), pcov_key)
)
check(
  "a status mismatch demotes to unreliable without value fabrication",
  identical(pcov_c$results[[pcov_key]]$schema$lower_status[1], "unreliable") &&
    identical(
      pcov_c$audit[pcov_c$audit$coef == "t0" &
        pcov_c$audit$side == "lower", ]$reason, "status_mismatch"
    )
)

# the pilot flips scaling on when a fit trips a ratified trigger and freezes
# the median positive anchor response as the one fixed scale
pcov_pilot <- local({
  n <- 60L
  set.seed(31)
  w2p <- matrix(rnorm(n * 2L), n, 2L)
  pcrp <- scale(matrix(rnorm(n * 4L), n, 4L,
    dimnames = list(NULL, paste0("l.pc", 1:4))
  ), center = TRUE, scale = FALSE)
  xp <- cbind(1, pcrp)
  w1p <- drop(w2p %*% c(0.3, -0.2)) + rnorm(n, sd = 1.5)
  stable <- logvar_ppml_pilot(w1p, w2p, xp, c(0.1, 0.1), NULL)
  # a nonconvergent anchor fit must trip the trigger: monkey-patch the fit
  # to a nonconverged verdict for one evaluation
  old <- logvar_ppml_fit
  assign("logvar_ppml_fit", function(b, w1, w2, x_mat, ...) {
    out <- old(b, w1, w2, x_mat, ...)
    out$converged <- FALSE
    out
  }, envir = globalenv())
  on.exit(assign("logvar_ppml_fit", old, envir = globalenv()), add = TRUE)
  tripped <- logvar_ppml_pilot(w1p, w2p, xp, c(0.1, 0.1), NULL)
  list(stable = stable, tripped = tripped, w1 = w1p, w2 = w2p)
})
check(
  "a stable pilot leaves scaling dormant at response_scale = 1",
  !pcov_pilot$stable$scale_needed && pcov_pilot$stable$response_scale == 1
)
check(
  "a tripped pilot freezes the median positive anchor response as the scale",
  pcov_pilot$tripped$scale_needed &&
    isTRUE(all.equal(
      pcov_pilot$tripped$response_scale,
      stats::median(drop(pcov_pilot$w1 - pcov_pilot$w2 %*% c(0.1, 0.1))^2)
    ))
)

# a fail-closed engine result keeps the theta_var coefficient axis: an
# unbounded news box propagates through the top-level gate with the
# estimator's labels, never a generic coefK axis
pcov_est <- logvar_ppml_estimator(
  peng_w1, peng_w2, peng_pcr, peng_qtr,
  b_point = NULL, scale_anchor_b = c(0, 0), scale_anchor_source = "test"
)
pcov_unb <- logvar_engine_set_at_tau(
  pcov_est, peng_qs,
  data.frame(
    coef = c("x1", "x2"), set_lower = c(-1, -1), set_upper = c(1, 1),
    status = c("unbounded", "bounded")
  ),
  grid_n = 5L, grid_floor = 1L, cold_start_check = FALSE
)
check(
  "a fail-closed schema keeps the theta_var coefficient axis",
  identical(
    pcov_unb$schema$coef,
    c("(Intercept)", paste0("l.pc", 1:4))
  ) && all(pcov_unb$table$status == "unbounded")
)
