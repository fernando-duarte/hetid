# analyze_domain hooks for the median (LAD) log-variance estimator: the census
# reuse + witness verification precheck, the verified-path probe sides phase, and
# the exact/guarded claim rule. Split from estimator.R for the line cap; each
# builder closes over the frozen geometry (w1, w2, x_mat, e_scale_ref) and adds it
# to the hook context via logvar_lad_augment_ctx before calling the domain module.
# The census reuses logvar_crossing_census (residual_map.R); witnesses, anchors
# and probes come from crossing_domain.R. Sourced by estimator.R.

# Re-raise a budget-exhausted condition (engine then fails the tau closed with
# counts); any other witness/probe error becomes a fallback value. MUST be the
# tryCatch's ONLY handler, else a sibling catches the stop(e) and eats the signal.
logvar_lad_pass_budget <- function(e, fallback) {
  if (inherits(e, "logvar_budget_exhausted")) {
    stop(e)
  }
  fallback
}

# precheck: locate candidate crossing rows with the shared census, then verify a
# constrained witness and feasible anchors for every one. A verified witness is
# kept for the sides phase; an unverified witness or an uncertified census row
# becomes an unresolved entry so the engine fails closed over every endpoint the
# region could hide. n_flagged reports the crossing count (forwarded as n_cross
# when the precheck passes); geom collects the cached evaluator and box degeneracy.
logvar_lad_precheck_hook <- function(
  w1,
  w2,
  x_mat,
  e_scale_ref,
  geom,
  control
) {
  function(qs, b_tab, ctx) {
    geom$evaluate_fit <- ctx$evaluate_fit
    geom$degenerate <- isTRUE(all(b_tab$set_lower == b_tab$set_upper))
    geom$center <- (b_tab$set_lower + b_tab$set_upper) / 2
    census <- logvar_crossing_census(qs, b_tab$set_lower, b_tab$set_upper, w1, w2)
    ctx_g <- logvar_lad_augment_ctx(
      ctx,
      w1,
      w2,
      x_mat,
      e_scale_ref,
      control
    )
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

# coef_objective: NULL on a full-dimensional box (engine builds its own budgeted
# objective). On a degenerate box (empty interior) it pins the objective to the box
# center through the cached evaluator (geom, filled by the precheck each tau), so
# the derivative-free COBYLA polish cannot exploit the 1e-4 slack to widen it.
logvar_lad_coef_objective <- function(geom) {
  function(j) {
    if (!isTRUE(geom$degenerate) || is.null(geom$evaluate_fit)) {
      return(NULL)
    }
    force(j)
    list(
      fn = function(b) {
        fit <- geom$evaluate_fit(geom$center, phase = LOGVAR_ENGINE_PHASES[["polish"]])
        if (!logvar_fit_ok(fit)) NaN else unname(fit$coef[[j]])
      },
      gr = NULL
    )
  }
}

paper_source_once(paper_path(
  "log_variance", "estimators", "lad", "engine_side_hooks.R"
))
