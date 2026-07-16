# Record assembly, notes, the composing evaluate(), and the status manifest for
# the base-R log-variance dynamics gate (dynamics-gate protocol). Definitions only;
# sourced by gate_core.R after the gate primitives, and it
# chain-sources the descriptive sensitivity module. Kept separate from the core
# primitives so every gate file stays below the repository line cap. Base R
# only -- no heavy dependency is named anywhere in the gate.

# attach the b_N coordinate names when the design supplies them
.logvar_gate_name_b <- function(b, nm) {
  if (!is.null(nm) && length(nm) == length(b)) stats::setNames(b, nm) else b
}

# the fixed diagnostic caveats plus the verdict-specific stop/continue note. The
# approximate-diagnostic and (on a non-rejection) insufficient-evidence wording
# is mandatory, and both directions of the caveat are stated.
logvar_gate_notes <- function(verdict, reason = "ok") {
  base <- c(
    paste(
      "Ljung-Box p-values ignore mean-equation estimation uncertainty; treat",
      "this as an approximate specification diagnostic, not a formal",
      "model-selection test."
    ),
    paste(
      "A rejection can also reflect mean misspecification, omitted regressors,",
      "timing errors, or breaks; a non-rejection does not establish absence of",
      "variance dynamics."
    )
  )
  tail <- switch(verdict,
    non_reject = paste(
      "The predeclared lag-4 screen found insufficient evidence of quarterly",
      "serial correlation to open the dynamics workstream; stopping with this",
      "diagnostic."
    ),
    reject = paste(
      "The predeclared lag-4 screen rejected at the 5 percent level; the",
      "changed-estimand and dependency approvals gate any downstream dynamic",
      "fitting."
    ),
    unreliable = paste0(
      "The benchmark log-variance residual could not be computed honestly at ",
      "the tau = 0 point (", reason, "); the gate is unreliable and the ",
      "workstream stops."
    )
  )
  c(base, tail)
}

# the full section-2.4 gate record on a computable series (verdict reject or
# non_reject): every field in the pinned order, the ACF through lag 8, and the
# full xi_hat series keyed by qtr
logvar_gate_record <- function(con, dec, sample_id, benchmark_commit,
                               b_point, n, sens) {
  k <- which.max(abs(con$xi_hat))
  list(
    schema_version = "1.0.0", sample_id = sample_id,
    benchmark_commit = benchmark_commit, b_point = b_point, n = as.integer(n),
    tested_lags = dec$tested_lags, q_stats = dec$q_stats,
    p_values = dec$p_values, gate_lag = dec$gate_lag,
    gate_alpha = dec$gate_alpha, verdict = dec$verdict,
    min_abs_eps = con$min_abs_eps, crossing_status = con$crossing_status,
    crossing_qtr = con$crossing_qtr, max_abs_xi = abs(con$xi_hat[k]),
    max_abs_xi_qtr = con$qtr[k], acf = dec$acf,
    xi_hat = data.frame(qtr = con$qtr, xi_hat = con$xi_hat),
    sensitivity = sens, notes = logvar_gate_notes(dec$verdict, con$reason)
  )
}

# the fail-closed record: same field order and types, NA where the Ljung-Box
# quantities are undefined, verdict "unreliable"
logvar_gate_unreliable_record <- function(con, sample_id, benchmark_commit,
                                          b_point, n, sens, tested_lags,
                                          gate_lag, alpha) {
  nm <- .logvar_gate_lag_names(tested_lags)
  na_lag <- stats::setNames(rep(NA_real_, length(tested_lags)), nm)
  acf_na <- stats::setNames(rep(NA_real_, 8L), .logvar_gate_lag_names(seq_len(8L)))
  empty_qtr <- if (!is.null(con$qtr)) con$qtr[0L] else integer(0)
  list(
    schema_version = "1.0.0", sample_id = sample_id,
    benchmark_commit = benchmark_commit, b_point = b_point, n = as.integer(n),
    tested_lags = as.integer(tested_lags), q_stats = na_lag,
    p_values = na_lag, gate_lag = as.integer(gate_lag), gate_alpha = alpha,
    verdict = "unreliable", min_abs_eps = con$min_abs_eps,
    crossing_status = con$crossing_status, crossing_qtr = con$crossing_qtr,
    max_abs_xi = NA_real_, max_abs_xi_qtr = empty_qtr[NA_integer_], acf = acf_na,
    xi_hat = data.frame(qtr = empty_qtr, xi_hat = numeric(0)),
    sensitivity = sens, notes = logvar_gate_notes("unreliable", con$reason)
  )
}

# compose the gate: name the point, run the descriptive sensitivity set (never
# able to reverse the primary verdict), construct xi_hat, and branch to the
# computable or fail-closed record. The sensitivity set is independent of the
# tau = 0 construction, so it is evaluated on either branch.
logvar_gate_evaluate <- function(inputs, b_point, proj, table_point, schema,
                                 b_seed, sample_id, benchmark_commit,
                                 tested_lags = c(1L, 4L, 8L), gate_lag = 4L,
                                 alpha = 0.05, tie_tol = 1e-10) {
  b_point <- .logvar_gate_name_b(b_point, colnames(inputs$w2))
  n <- length(inputs$w1)
  sens <- logvar_gate_sensitivity(
    inputs, proj, schema, b_seed, tested_lags, gate_lag, alpha
  )
  con <- logvar_gate_construct(inputs, b_point, proj, table_point, tie_tol)
  if (identical(con$status, "unreliable")) {
    return(logvar_gate_unreliable_record(
      con, sample_id, benchmark_commit, b_point, n, sens,
      tested_lags, gate_lag, alpha
    ))
  }
  dec <- logvar_gate_decide(con$xi_hat, tested_lags, gate_lag, alpha)
  logvar_gate_record(con, dec, sample_id, benchmark_commit, b_point, n, sens)
}

# the always-written status manifest: the gate verdict and its decision inputs,
# whether a downstream approval is now pending, and the gate-record path. The
# routing layer rewrites this on every terminal branch.
logvar_gate_status_manifest <- function(record) {
  reject <- identical(record$verdict, "reject")
  list(
    schema_version = "1.0.0", stage = "dynamics_gate", plan = "logvar-egarch-x",
    sample_id = record$sample_id, benchmark_commit = record$benchmark_commit,
    gate_verdict = record$verdict, gate_lag = record$gate_lag,
    gate_alpha = record$gate_alpha, gate_q_lag4 = unname(record$q_stats[["lag4"]]),
    gate_p_lag4 = unname(record$p_values[["lag4"]]),
    min_abs_eps = record$min_abs_eps, crossing_status = record$crossing_status,
    decision_pending = reject,
    workstream_status = if (reject) "gate_rejected" else "stopped_after_diagnostic",
    gate_record_path = artifact_path("dynamics_gate")
  )
}

# the descriptive sensitivity set over representative feasible b_N witnesses
source(paper_path("log_variance", "diagnostics", "dynamics", "gate_sensitivity.R"))
