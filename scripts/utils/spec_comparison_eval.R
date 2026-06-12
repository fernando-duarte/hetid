# Spec-comparison evaluators: per-cell moments, fixed/optimized/I x J width
# evaluation, and the per-group row builder for the (mode, n_pcs, components,
# gamma, tau) grid. Moved verbatim from spec_comparison.R (stage 05), its only
# consumer, which sources this file after spec_comparison_design.R; the
# functions resolve that script's globals (design grids, n_starts_opt, SEED)
# at call time.

# --- moments per (mode, n_pcs, components) ---
# Pure (no shared cache): each group has a unique key, so a within-process cache
# never produced a cross-group hit, and forked workers cannot share one anyway.
spec_moments <- function(mode, n_pcs, components) {
  inp <- load_identification_inputs(
    n_pcs = n_pcs, mode = mode,
    factors = if (mode == "factors") components else DEFAULT_ID_FACTORS,
    maturities = if (mode == "maturities") components else DEFAULT_ID_MATURITIES
  )
  resid <- compute_identification_residuals(
    inp$data,
    n_pcs = n_pcs, mode = mode,
    factors = if (mode == "factors") components else DEFAULT_ID_FACTORS,
    maturities = if (mode == "maturities") components else DEFAULT_ID_MATURITIES
  )
  # The grid is a PC-design artifact: its n_pcs axis and applicability
  # rules (vfci only at n_pcs = 4) describe PC instrument counts. A custom
  # HETID_Z_SOURCE may only feed groups whose width matches; mismatched
  # groups fail loudly (caught per group) instead of producing rows whose
  # n_pcs label would be false.
  if (ncol(resid$pcs_aligned) != n_pcs) {
    stop(
      "custom Z has ", ncol(resid$pcs_aligned), " columns but this grid ",
      "group is the n_pcs = ", n_pcs, " design cell; the spec grid runs ",
      "only width-matching groups under HETID_Z_SOURCE -- use the stage ",
      "04/05 single-spec scripts for arbitrary-width analysis"
    )
  }
  mom <- compute_identification_moments(resid$w1, resid$w2, resid$pcs_aligned)
  list(moments = mom, n_comp = ncol(resid$w2), gamma_rf = resid$gamma_rf)
}

# --- evaluate one (gamma, tau): point at tau=0, else the set ---
eval_fixed <- function(gamma, mom, n_comp, tau) {
  qs <- suppressMessages(build_quadratic_system(gamma, rep(tau, n_comp), mom))
  if (tau == 0) {
    pt <- solve_point_identification(qs$components)
    if (is.null(pt)) {
      return(list(width = NA_real_, bounded = FALSE, kind = "point-failed", cond = NA))
    }
    return(list(width = 0, bounded = TRUE, kind = "point", cond = pt$cond))
  }
  b <- solve_all_profile_bounds(qs$quadratic)
  list(
    width = sum(b$width),
    bounded = all(b$bounded_lower & b$bounded_upper),
    kind = "set", cond = NA
  )
}
eval_opt <- function(seed, mom, n_comp, tau) {
  r <- suppressMessages(
    run_gamma_optimization(seed, mom, rep(tau, n_comp), n_starts = n_starts_opt, seed = SEED)
  )
  list(
    width = r$objective_final, bounded = is.finite(r$objective_final),
    kind = "set(opt)", cond = NA
  )
}
# --- separate I x J: each PC is its own instrument; intersection of I*J
# single-instrument constraints, summed profile width (tau>0 only) ---
# Honest 3-state (mirrors compute_identification_ixj.R): a certified finite box
# (every side bounded AND valid) reports its summed width; a certified unbounded
# set reports Inf; otherwise the SLSQP bounds are unreliable/crossed -> width NA
# (no-certified-bound), never a stray finite/negative number reported as bounded.
eval_ixj <- function(mom, n_comp, tau) {
  n_inst <- nrow(mom$r_i_0)
  qs <- build_ixj_quadratic_system(mom, matrix(tau, nrow = n_inst, ncol = n_comp))
  b <- solve_all_profile_bounds(qs$quadratic)
  bounded_all <- all(b$bounded_lower & b$bounded_upper)
  valid_all <- all(b$valid_lower & b$valid_upper)
  width <- if (bounded_all && valid_all) {
    sum(b$width)
  } else if (!bounded_all && valid_all) {
    Inf
  } else {
    NA_real_
  }
  list(width = width, bounded = bounded_all && valid_all, kind = "set(ixj)", cond = NA)
}

# --- all rows for one group: every (tau, method) combination ---
compute_group_rows <- function(mode, n_pcs, components) {
  clabel <- paste(components, collapse = "-")
  sm <- spec_moments(mode, n_pcs, components)
  mom <- sm$moments
  nc <- sm$n_comp
  methods <- c(if (n_pcs == 4) "vfci", if (mode == "factors") "reduced_form")
  rows <- list()
  add_row <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)
  for (tau in tau_grid) {
    # fixed-gamma methods
    for (meth in methods) {
      gamma <- if (meth == "vfci") {
        get_baseline_gamma("vfci", n_pcs = n_pcs, n_components = nc)
      } else {
        sm$gamma_rf
      }
      if (is.null(gamma)) next
      r <- tryCatch(eval_fixed(gamma, mom, nc, tau), error = function(e) NULL)
      if (!is.null(r)) {
        add_row(
          mode = mode, n_pcs = n_pcs, components = clabel, n_comp = nc,
          gamma = meth, tau = tau, width = r$width,
          bounded = r$bounded, kind = r$kind, cond = r$cond
        )
      }
    }
    # optimized (tau>0 only; tau=0 point is degenerate for any full-rank gamma)
    if (tau > 0) {
      seed <- if (n_pcs == 4) {
        get_baseline_gamma("vfci", n_pcs = n_pcs, n_components = nc)
      } else if (!is.null(sm$gamma_rf)) {
        sm$gamma_rf
      } else {
        # No structural/reduced-form seed available -> deterministic random start.
        # set.seed here makes the fallback reproducible despite per-worker RNG
        # streams under fork (mclapply sets a distinct stream per child).
        n_inst <- nrow(mom$r_i_0)
        set.seed(SEED)
        matrix(stats::rnorm(n_inst * nc), n_inst, nc)
      }
      r <- tryCatch(eval_opt(seed, mom, nc, tau), error = function(e) NULL)
      if (!is.null(r)) {
        add_row(
          mode = mode, n_pcs = n_pcs, components = clabel, n_comp = nc,
          gamma = "optimized", tau = tau, width = r$width,
          bounded = r$bounded, kind = r$kind, cond = r$cond
        )
      }
      # separate-instrument I x J scheme (no gamma; tau>0 only)
      r <- tryCatch(eval_ixj(mom, nc, tau), error = function(e) NULL)
      if (!is.null(r)) {
        add_row(
          mode = mode, n_pcs = n_pcs, components = clabel, n_comp = nc,
          gamma = "separate", tau = tau, width = r$width,
          bounded = r$bounded, kind = r$kind, cond = r$cond
        )
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}
