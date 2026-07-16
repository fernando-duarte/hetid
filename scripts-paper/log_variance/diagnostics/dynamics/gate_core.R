# Base-R residual-dynamics gate for the log-variance equation (dynamics-gate protocol).
# The predeclared lag-4 Ljung-Box screen on the tau = 0 benchmark log-variance
# residual xi_hat decides whether any downstream volatility-dynamics workstream
# opens: it ships regardless of any approval and uses base R only (no heavy
# dependency is referenced here). This file holds the pure gate functions --
# construction, decision, and their arithmetic helpers -- and sources the
# record/sensitivity/manifest module. Definitions only; sourced by
# run_gate.R at pipeline time and by the offline gate test.
#
# Contract (finalized plan section 2): given the frozen benchmark objects, form
#   e_star  = w1 - w2 %*% b_point                (Lewbel tau = 0 residual)
#   log_e2  = 2 * log(abs(e_star))               (avoids squaring underflow)
#   theta_hat = proj %*% log_e2                  (the two-step log-variance map)
#   xi_hat  = log_e2 - x_mat %*% theta_hat       (the benchmark residual, eq. 2)
# fail closed (verdict "unreliable") on a missing Lewbel point, an exact residual
# zero, or any nonfinite xi_hat; lag 4 is the sole decision at alpha = 0.05, with
# lags 1 and 8 printed as sensitivity context only.

# lag column labels shared by the Q, p, and ACF vectors
.logvar_gate_lag_names <- function(lags) sprintf("lag%d", as.integer(lags))

# canonical qtr order for every series, with proj's columns (one per
# observation) permuted the same way, so the gate is a qtr join and never a
# row-position alignment: theta_hat = sum_t proj[, t] log_e2[t] is invariant to
# the permutation while xi_hat comes out in calendar order
.logvar_gate_order <- function(w1, w2, pcr, qtr, proj) {
  o <- order(qtr)
  list(
    w1 = w1[o], w2 = w2[o, , drop = FALSE], pcr = pcr[o, , drop = FALSE],
    qtr = qtr[o], proj = proj[, o, drop = FALSE]
  )
}

# the two-step arithmetic on an aligned residual vector: log squared residual
# via 2 log|e| (immune to squaring underflow), the projected coefficients, and
# the regression residual xi_hat on x_mat = (1, PC_R)
.logvar_gate_resid <- function(e_star, pcr, proj) {
  log_e2 <- 2 * log(abs(e_star))
  theta_hat <- drop(proj %*% log_e2)
  x_mat <- cbind("(Intercept)" = 1, pcr)
  list(
    log_e2 = log_e2, theta_hat = theta_hat,
    xi_hat = drop(log_e2 - x_mat %*% theta_hat)
  )
}

# Ljung-Box Q and p at each tested lag, fitdf = 0 (the p-values ignore the
# mean-equation estimation step, so they are an approximate specification
# diagnostic rather than a formal model-selection test; a fitdf = 5 with a
# lag = 4 request would be a fatal R error)
.logvar_gate_box <- function(xi_hat, lags) {
  nm <- .logvar_gate_lag_names(lags)
  bt <- lapply(as.integer(lags), function(l) {
    stats::Box.test(xi_hat, lag = l, type = "Ljung-Box", fitdf = 0L)
  })
  list(
    q_stats = stats::setNames(vapply(bt, \(b) unname(b$statistic), numeric(1)), nm),
    p_values = stats::setNames(vapply(bt, \(b) b$p.value, numeric(1)), nm)
  )
}

# construct xi_hat at the tau = 0 Lewbel point, or a fail-closed unreliable
# signal. The tie-back stopifnot proves theta_hat is the benchmark's stored
# log-variance point (log_var_eq$table$point), so recomputing proj is a fixed
# linear-algebra recompute and not a re-estimation on a drifted sample.
logvar_gate_construct <- function(inputs, b_point, proj, table_point,
                                  tie_tol = 1e-10) {
  bad <- function(reason, status, min_eps, cq, qtr = NULL) {
    list(
      status = "unreliable", reason = reason, crossing_status = status,
      min_abs_eps = min_eps, crossing_qtr = cq, xi_hat = NULL,
      qtr = qtr, theta_hat = NULL, e_star = NULL
    )
  }
  if (anyNA(b_point)) {
    return(bad("no Lewbel point", "no_lewbel_point", NA_real_, NULL))
  }
  ord <- .logvar_gate_order(inputs$w1, inputs$w2, inputs$pcr, inputs$qtr, proj)
  e_star <- drop(ord$w1 - ord$w2 %*% b_point)
  zero_idx <- which(e_star == 0)
  if (length(zero_idx)) {
    return(bad(
      "exact residual zero", "crossing_at_point", 0, ord$qtr[zero_idx], ord$qtr
    ))
  }
  r <- .logvar_gate_resid(e_star, ord$pcr, ord$proj)
  # tie-back: xi_hat is definitionally the paper's log-variance residual
  stopifnot(max(abs(r$theta_hat - table_point)) < tie_tol)
  min_eps <- min(abs(e_star))
  bad_xi <- !is.finite(r$xi_hat)
  if (any(bad_xi)) {
    return(bad(
      "nonfinite residual", "nonfinite_residual", min_eps, ord$qtr[bad_xi], ord$qtr
    ))
  }
  list(
    status = "ok", reason = "ok", crossing_status = "none",
    min_abs_eps = min_eps, crossing_qtr = NULL, xi_hat = r$xi_hat,
    qtr = ord$qtr, theta_hat = r$theta_hat, e_star = e_star
  )
}

# the pure decision on a prescribed finite series: Ljung-Box Q/p at the tested
# lags, the residual ACF through lag 8, and the single lag-4 verdict. Lags 1 and
# 8 are context, never additional chances to pass the gate.
logvar_gate_decide <- function(xi_hat, tested_lags = c(1L, 4L, 8L),
                               gate_lag = 4L, alpha = 0.05, acf_max = 8L) {
  stopifnot(
    all(is.finite(xi_hat)), length(xi_hat) > gate_lag,
    is.numeric(tested_lags), as.integer(gate_lag) %in% as.integer(tested_lags)
  )
  box <- .logvar_gate_box(xi_hat, tested_lags)
  acf_v <- drop(stats::acf(xi_hat, lag.max = acf_max, plot = FALSE)$acf)[-1L]
  names(acf_v) <- .logvar_gate_lag_names(seq_len(acf_max))
  gate_name <- sprintf("lag%d", as.integer(gate_lag))
  list(
    tested_lags = as.integer(tested_lags), q_stats = box$q_stats,
    p_values = box$p_values, acf = acf_v, gate_lag = as.integer(gate_lag),
    gate_alpha = alpha,
    verdict = if (box$p_values[[gate_name]] < alpha) "reject" else "non_reject"
  )
}

# the record assembly, descriptive sensitivity set, unreliable-record builder,
# composing evaluate(), and status-manifest builder
source(paper_path("log_variance", "diagnostics", "dynamics", "gate_record.R"))
