# Crossing-domain module for the median (LAD) log-variance estimator: the
# constrained crossing-witness verifier, the geometric approach probe, and (via
# the sourced siblings) the constrained solves and the M-scale tail classifier.
# The census only claims a hyperplane meets the feasible set; this module either
# freshly verifies a witness with feasible approach anchors or returns unresolved,
# never turning a census hit straight into a divergence label. No residual floor,
# no epsilon inside a log; statuses stay operational (verified / unresolved_witness
# / domain_unavailable / not_a_crossing), never a certified LP recession claim.

# Sourced by the crossing tests and the LAD driver; load the sibling solves and
# the tail classifier into the same environment when not already present.
if (!exists(".lad_domain_geom")) {
  source(paper_path(
    "log_variance", "estimators", "lad", "domain_witness.R"
  ))
}
if (!exists("logvar_lad_tail_classify")) {
  source(paper_path(
    "log_variance", "estimators", "lad", "domain_tail.R"
  ))
}

# Canonical active-basis / residual-sign signature of an inner fit, used to gate
# tail eligibility. Reads the fit's own signature when present, else the residual-
# sign counts, else a neutral token.
.lad_active_signature <- function(fit) {
  d <- fit$diagnostics
  if (!is.null(d$active_signature)) {
    return(as.character(d$active_signature)[1L])
  }
  if (!is.null(d$residual_sign_counts)) {
    return(paste(d$residual_sign_counts, collapse = "/"))
  }
  "na"
}

# Verify a crossing witness for row i against the feasible set carried by ctx
# (geometry members w1/w2/x_mat/e_scale_ref plus check_feasible/b_scales). Returns
# a fully audited record (status "verified" with b_cross, anchors, feasible-side
# count, sign cones, constraint violation) or an explicit unresolved/degenerate
# status. Witness via nloptr::slsqp minimizing the squared normalized row residual
# under the normalized set constraints, accepted only at abs(e_i)/e_scale_ref <=
# 1e-13 and normalized violation <= 1e-8.
logvar_lad_crossing_witness <- function(i, qs, b_tab, ctx) {
  geom <- .lad_domain_geom(i, qs, ctx)
  esc <- geom$esc
  nrm <- sqrt(sum(geom$normal^2))
  if (nrm <= 1e-12 * max(1, max(abs(ctx$w2)))) {
    if (abs(geom$rhs) <= 1e-12 * esc) {
      return(list(
        status = "domain_unavailable", row = i, reason =
          "zero row normal with zero residual: H_i covers the feasible domain"
      ))
    }
    return(list(
      status = "not_a_crossing", row = i, reason =
        "zero row normal with nonzero residual"
    ))
  }
  best <- .lad_witness_min(geom, .lad_witness_starts(geom, b_tab))
  if (is.null(best)) {
    return(list(
      status = "unresolved_witness", row = i, reason =
        "no feasible candidate minimizing the row residual"
    ))
  }
  b_cross <- .lad_project_metric(geom, best)
  absres <- abs(geom$resid(b_cross)) / esc
  viol <- max(geom$gvals(b_cross))
  if (!(is.finite(absres) && absres <= 1e-13 && is.finite(viol) && viol <= 1e-8)) {
    return(list(
      status = "unresolved_witness", row = i, reason =
        "candidate fails the hyperplane or feasibility acceptance tolerance"
    ))
  }
  anchors <- Filter(Negate(is.null), lapply(c(-1, 1), function(s) {
    .lad_anchor(geom, b_cross, s)
  }))
  if (length(anchors) == 0L) {
    return(list(
      status = "unresolved_witness", row = i, reason =
        "no feasible approach anchor bounded off H_i (thin or disconnected)"
    ))
  }
  cone <- .lad_sign_cones(geom, b_cross, ctx, i)
  list(
    status = "verified", row = i, b_cross = b_cross, anchors = anchors,
    n_feasible_sides = length(anchors), sign_cones = cone$cones,
    simultaneous_rows = cone$simultaneous_rows,
    constraint_violation = viol, abs_residual = absres
  )
}

# Probe a verified segment from a feasible anchor toward b_cross, halving the
# distance each step (at most 40) and refitting cold at each geometric point via
# ctx$evaluate_fit. Records M_s = -2 log(|e_i,s| / e_scale_ref), coefficients,
# feasibility slack, active signature, and guard distance. Feasibility is checked
# at every point; the walk never steps blindly along a row normal. The punctured
# crossing point is not a domain sample: the walk stops at the numerical guard or
# any non-ok / non-finite fit BEFORE recording it, so the trace carries only valid
# punctured-domain fits and the tail classifier's all-fits-ok gate holds at source.
logvar_lad_crossing_probe <- function(witness, path_id, ctx) {
  if (!identical(witness$status, "verified")) {
    stop("logvar_lad_crossing_probe requires a verified witness")
  }
  anchor <- witness$anchors[[path_id]]
  b_cross <- witness$b_cross
  i <- witness$row
  normal <- as.numeric(ctx$w2[i, ])
  rhs <- ctx$w1[i]
  esc <- ctx$e_scale_ref
  ms <- numeric(0)
  coefs <- list()
  slack <- numeric(0)
  sigs <- character(0)
  guard <- numeric(0)
  statuses <- character(0)
  for (s in seq_len(40L)) {
    b <- b_cross + 0.5^(s - 1L) * (anchor$b - b_cross)
    feas <- ctx$check_feasible(b)
    if (!isTRUE(feas$feasible)) break
    e_is <- rhs - sum(normal * b)
    if (abs(e_is) <= 1e-12 * esc) break
    fit <- ctx$evaluate_fit(b, phase = "probe", use_cache = TRUE)
    st <- if (is.null(fit$fit_status)) "ok" else fit$fit_status
    if (!identical(st, "ok") || any(!is.finite(fit$coef))) break
    ms <- c(ms, -2 * log(abs(e_is) / esc))
    coefs[[length(coefs) + 1L]] <- fit$coef
    slack <- c(slack, -feas$max_violation)
    sigs <- c(sigs, .lad_active_signature(fit))
    guard <- c(guard, abs(e_is) / esc)
    statuses <- c(statuses, st)
  }
  coef_mat <- if (length(coefs)) do.call(rbind, coefs) else matrix(numeric(0))
  list(
    path_id = path_id, row = i, m = ms, coef = coef_mat, fit_status = statuses,
    active_signature = sigs, feasibility_slack = slack, guard_distance = guard
  )
}
