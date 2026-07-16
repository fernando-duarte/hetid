# Per-tau mapping helpers for the median (LAD) log-variance driver: the engine
# scan over the box, the verify-or-rerun five-start sensitivity gate (a more
# extreme candidate re-enters through the engine, an unreproducible side is
# demoted, the gate never patches an endpoint value), and the deterministic fn
# nonuniqueness demotion that runs last. Each tau is mapped in a single engine
# pass with no extra starts: the median map is nonsmooth, so every extra start is
# another derivative-free COBYLA polish on all ten endpoints, and both the
# second-pass refinement lattice and the cross-tau warm chain flood that phase
# (D3.1). Dropping them also makes each tau independent of its predecessor, so the
# five-start gate is a genuinely independent replication of the same box. The
# engine and the committed LAD fit modules are consumed, never modified.
# Definitions only; sourced by the driver before the guarded orchestration.

# The five-start re-polish on a fresh cache and fresh budget: an independent
# verification of the same box. A failed rerun returns NULL and the comparison
# then demotes every bounded primary side.
logvar_lad_gate_repolish <- function(est, qs, b_tab, tau, cfg) {
  gcache <- new.env(parent = emptyenv())
  gbs <- logvar_budget_state(cfg$fit_budget, cfg$phase_caps)
  tryCatch(
    logvar_engine_set_at_tau(
      est, qs, b_tab,
      b_seed = cfg$b_seed, max_grid_points = cfg$grid_cap,
      max_fit_evals = cfg$fit_budget, starts_per_side = 5L, cache = gcache,
      budget_state = gbs, cold_start_check = TRUE, tau = tau
    ),
    error = function(e) NULL
  )
}

# Compare bounded sides at 1e-4 relative. A gate side strictly more extreme yields
# a better candidate arg (fed back through the engine); a bounded side the gate
# reproduces materially less extreme, or fails to hold bounded, is sensitive and is
# demoted. A NULL gate cannot verify anything, so every bounded side is sensitive.
logvar_lad_gate_compare <- function(primary, gate, tol = 1e-4) {
  ps <- primary$schema
  tag <- function(coef, side) sprintf("%s:%s", coef, if (side == "lower") "min" else "max")
  better <- list()
  sensitive <- character(0)
  gs <- if (is.null(gate)) NULL else gate$schema
  for (j in seq_len(nrow(ps))) {
    gj <- if (is.null(gs)) NA_integer_ else match(ps$coef[j], gs$coef)
    for (side in c("lower", "upper")) {
      if (!identical(ps[[paste0(side, "_status")]][j], "bounded")) next
      pval <- ps[[side]][j]
      tolj <- tol * max(1, abs(pval))
      gstat <- if (is.na(gj)) NA_character_ else gs[[paste0(side, "_status")]][gj]
      if (!identical(gstat, "bounded")) {
        sensitive <- c(sensitive, tag(ps$coef[j], side))
        next
      }
      gval <- gs[[side]][gj]
      garg <- gs[[paste0("arg_", side)]][[gj]]
      more <- if (side == "lower") gval < pval - tolj else gval > pval + tolj
      if (isTRUE(more) && !anyNA(garg)) {
        better[[length(better) + 1L]] <- unname(garg)
      } else if (abs(gval - pval) > tolj) {
        sensitive <- c(sensitive, tag(ps$coef[j], side))
      }
    }
  }
  list(better = better, sensitive = unique(sensitive))
}

# Recompute the table status from the schema sides after a demotion.
logvar_lad_refresh_status <- function(res) {
  lo <- res$schema$lower_status
  up <- res$schema$upper_status
  res$table$status <- ifelse(
    lo == "unreliable" | up == "unreliable", "unreliable",
    ifelse(lo == "unbounded" | up == "unbounded", "unbounded", "bounded")
  )
  res
}

# Status-only demotion of the sensitive sides: the endpoint value and arg are
# preserved (never patched), only the side status flips to unreliable.
logvar_lad_apply_downgrades <- function(res, sensitive) {
  for (t in sensitive) {
    parts <- strsplit(t, ":", fixed = TRUE)[[1]]
    j <- match(parts[1], res$schema$coef)
    if (is.na(j) || length(parts) != 2L) next
    side <- if (parts[2] == "min") "lower" else "upper"
    res$schema[[paste0(side, "_status")]][j] <- "unreliable"
  }
  logvar_lad_refresh_status(res)
}

# Whole-tau demotion: every side unreliable (values kept), used when the fn
# schedule finds an endpoint-relevant nonunique or cap-truncated point.
logvar_lad_demote_all <- function(res) {
  res$schema$lower_status <- rep("unreliable", nrow(res$schema))
  res$schema$upper_status <- rep("unreliable", nrow(res$schema))
  logvar_lad_refresh_status(res)
}

# One tau, end to end: the single-pass map, the verify-or-rerun gate, the per-side
# downgrade of unreproducible sides, then the last-word nonuniqueness demotion.
# Only the gate's strictly-more-extreme args ever enter as extra starts, and only
# on the re-run they trigger. Returns the final engine result, the shared per-tau
# budget, and the gate/nonunique audit.
logvar_lad_map_tau <- function(est, qs, b_tab, tau, cfg, cache) {
  bs <- logvar_budget_state(cfg$fit_budget, cfg$phase_caps)
  run <- function(extra) {
    logvar_engine_set_at_tau(
      est, qs, b_tab,
      b_seed = cfg$b_seed, max_grid_points = cfg$grid_cap,
      max_fit_evals = cfg$fit_budget, starts_per_side = 3L, cache = cache,
      budget_state = bs, extra_starts = extra, cold_start_check = TRUE, tau = tau
    )
  }
  res <- run(NULL)
  gate <- logvar_lad_gate_repolish(est, qs, b_tab, tau, cfg)
  cmp <- logvar_lad_gate_compare(res, gate)
  if (length(cmp$better) > 0L) res <- run(cmp$better)
  cmp2 <- logvar_lad_gate_compare(res, gate)
  res <- logvar_lad_apply_downgrades(res, cmp2$sensitive)
  nd <- logvar_lad_nonunique_demote(est, res, cfg, cmp$better)
  if (isTRUE(nd$tau_unreliable)) res <- logvar_lad_demote_all(res)
  list(
    res = res, budget = bs,
    witnesses = res$domain_info$precheck$info$witnesses,
    audit = list(
      n_better = length(cmp$better), sensitive_sides = cmp2$sensitive,
      gate_ran = !is.null(gate), nonunique = nd
    )
  )
}
