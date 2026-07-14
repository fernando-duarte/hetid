# analyze_domain hooks for the median (LAD) log-variance estimator: the census
# reuse + witness verification precheck, the verified-path probe protocol sides
# phase, and the exact/guarded claim rule. Split from log_var_eq_lad.R for the
# repository line cap; each builder closes over the frozen geometry (w1, w2,
# x_mat, e_scale_ref) and adds it to the engine's hook context via
# logvar_lad_augment_ctx before calling the domain module. The census reuses the
# benchmark logvar_crossing_census (from log_var_eq_map.R); witnesses, anchors
# and probes come from log_var_eq_lad_domain.R, both resolved at call time.
# Definitions only; sourced by log_var_eq_lad.R.

# Re-raise a budget-exhausted condition (so the engine fails the tau closed with
# counts) while turning any other witness/probe error into a fallback value. This
# MUST be the tryCatch's only handler: a sibling error handler would catch the
# stop(e) below and swallow the budget signal before it ever reaches the engine.
logvar_lad_pass_budget <- function(e, fallback) {
  if (inherits(e, "logvar_budget_exhausted")) {
    stop(e)
  }
  fallback
}

# precheck: locate candidate crossing rows with the shared census, then verify a
# constrained witness and feasible anchors for every one. A verified witness is
# kept for the sides phase; an unverified witness, or a census row the functional
# solve could not certify, becomes an unresolved entry so the engine fails closed
# over every endpoint the region could hide. n_flagged reports the crossing count
# (the engine forwards it as n_cross when the precheck passes); geom collects the
# per-tau cached evaluator and box degeneracy for coef_objective.
logvar_lad_precheck_hook <- function(w1, w2, x_mat, e_scale_ref, geom) {
  function(qs, b_tab, ctx) {
    geom$evaluate_fit <- ctx$evaluate_fit
    geom$degenerate <- isTRUE(all(b_tab$set_lower == b_tab$set_upper))
    geom$center <- (b_tab$set_lower + b_tab$set_upper) / 2
    census <- logvar_crossing_census(qs, b_tab$set_lower, b_tab$set_upper, w1, w2)
    ctx_g <- logvar_lad_augment_ctx(ctx, w1, w2, x_mat, e_scale_ref)
    witnesses <- list()
    unresolved <- character(0)
    for (i in census$cross) {
      wit <- tryCatch(
        logvar_lad_crossing_witness(i, qs, b_tab, ctx_g),
        error = function(e) {
          logvar_lad_pass_budget(e, list(status = "unresolved_witness"))
        }
      )
      if (identical(wit$status, "verified")) {
        wit$row <- i
        witnesses[[as.character(i)]] <- wit
      } else {
        unresolved <- c(unresolved, sprintf("crossing_%d", i))
      }
    }
    if (length(census$unresolved) > 0L) {
      unresolved <- c(unresolved, sprintf("census_%d", census$unresolved))
    }
    list(
      unresolved = unresolved, n_flagged = length(census$cross),
      info = list(
        cross = census$cross, witnesses = witnesses,
        census_unresolved = census$unresolved
      )
    )
  }
}

# coef_objective: NULL on a full-dimensional box, so the engine builds its own
# budgeted, exhaustion-aware objective. On a degenerate box (empty interior) it
# pins the objective to the box center through the same cached evaluator (geom,
# filled by the precheck each tau), so the derivative-free COBYLA polish cannot
# exploit the 1e-4 feasibility slack to widen a measure-zero set.
logvar_lad_coef_objective <- function(geom) {
  function(j) {
    if (!isTRUE(geom$degenerate) || is.null(geom$evaluate_fit)) {
      return(NULL)
    }
    force(j)
    list(
      fn = function(b) {
        fit <- geom$evaluate_fit(geom$center, phase = "polish")
        if (!logvar_fit_ok(fit)) NaN else unname(fit$coef[[j]])
      },
      gr = NULL
    )
  }
}

# sides: probe each verified path from precheck through the cached, budget
# debiting evaluator, classify the coefficient traces in the M coordinate, and
# fold each verdict into the side state through logvar_lad_side_update (whose
# comment states the per-verdict rules).
logvar_lad_sides_hook <- function(w1, w2, x_mat, e_scale_ref) {
  function(qs, b_tab, scan, ctx) {
    labels <- colnames(x_mat)
    n_coef <- length(labels)
    st <- list(
      lower_unb = stats::setNames(rep(FALSE, n_coef), labels),
      upper_unb = stats::setNames(rep(FALSE, n_coef), labels),
      unresolved = character(0), closure = list()
    )
    witnesses <- ctx$precheck$info$witnesses
    ctx_g <- logvar_lad_augment_ctx(ctx, w1, w2, x_mat, e_scale_ref)
    for (wid in names(witnesses)) {
      wit <- witnesses[[wid]]
      for (path_id in seq_len(length(wit$anchors))) {
        cls <- tryCatch(
          logvar_lad_tail_classify(
            logvar_lad_crossing_probe(wit, path_id, ctx_g)
          ),
          error = function(e) logvar_lad_pass_budget(e, NULL)
        )
        st <- logvar_lad_side_update(st, cls, labels, wid, path_id)
      }
    }
    list(
      lower_unbounded = st$lower_unb, upper_unbounded = st$upper_unb,
      unresolved_endpoints = unique(st$unresolved),
      closure_diagnostics = if (length(st$closure) == 0L) NULL else st$closure,
      info = list(
        cross_all = ctx$precheck$info$cross, method = "lad_crossing_probe",
        n_witnesses = length(witnesses)
      )
    )
  }
}

# fold one classified path into the running side state: divergence sets the named
# endpoint unbounded, a stable finite one-sided limit appends a closure record (the
# classifier sub-result stored verbatim, provenance labelled), an uninformative
# (too-thin) probe is skipped as missing data, and every other verdict marks both
# of that coefficient's endpoints unresolved -> wired to unreliable by the engine.
# Skipping the uninformative case is what stops one degenerate probe path from
# failing an otherwise stably-classified coefficient closed.
logvar_lad_side_update <- function(st, cls, labels, wid, path_id) {
  both <- function(j) c(sprintf("%s:min", labels[j]), sprintf("%s:max", labels[j]))
  if (is.null(cls) || is.null(cls$coef)) {
    st$unresolved <- c(st$unresolved, unlist(lapply(seq_along(labels), both)))
    return(st)
  }
  for (j in seq_along(labels)) {
    cj <- cls$coef[[j]]
    if (identical(cj$status, "persistent_divergent_evidence")) {
      if (identical(cj$endpoint, "lower")) {
        st$lower_unb[j] <- TRUE
      } else if (identical(cj$endpoint, "upper")) {
        st$upper_unb[j] <- TRUE
      } else {
        st$unresolved <- c(st$unresolved, both(j))
      }
    } else if (identical(cj$status, "stable_finite")) {
      st$closure[[length(st$closure) + 1L]] <- list(
        coef = labels[j], witness = wid, path_id = path_id,
        classification = cj, provenance = "one-sided crossing-limit approximation"
      )
    } else if (identical(cj$status, "uninformative")) {
      next
    } else {
      st$unresolved <- c(st$unresolved, both(j))
    }
  }
  st
}

# claim_failure: distinguish an exact residual zero (mathematically outside the
# log domain) from a guarded near-crossing (available but numerically unresolved).
# Implicated rows are recomputed from the geometry so the rule never depends on
# the fit's internal fields. Either is claimed only when every implicated row is
# a verified census crossing; an off-census domain failure is left unclaimed and
# the engine fails the tau closed.
logvar_lad_claim_hook <- function(w1, w2, e_scale_ref) {
  function(b, fit, precheck, ctx) {
    if (!identical(fit$fit_status, "domain_failure")) {
      return(list(
        claimed = FALSE, domain_state = NA_character_,
        reason = "not a domain failure", probe_targets = NULL
      ))
    }
    e <- drop(w1 - w2 %*% b)
    guard <- logvar_lad_guard_ratio * e_scale_ref
    exact <- which(e == 0)
    rows <- sort(c(exact, which(abs(e) <= guard & e != 0)))
    census_rows <- if (is.null(precheck)) integer(0) else precheck$info$cross
    on_census <- length(rows) > 0L && all(rows %in% census_rows)
    ds <- if (length(exact) > 0L) {
      "exact_domain_failure"
    } else {
      "numerically_unresolved_near_crossing"
    }
    reason <- if (!on_census) {
      "domain failure off the verified census: fail closed"
    } else if (identical(ds, "exact_domain_failure")) {
      "certified exclusion at an exact residual zero on a census crossing"
    } else {
      "guarded near-crossing on a census row: available but numerically unresolved"
    }
    list(
      claimed = on_census, domain_state = ds, reason = reason,
      probe_targets = if (on_census) rows else NULL
    )
  }
}
