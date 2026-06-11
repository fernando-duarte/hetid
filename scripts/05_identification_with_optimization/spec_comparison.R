# Comprehensive specification / instrument / tau comparison for the identified set.
# Benchmark = the tau=0 POINT identification (smallest width = 0); all other
# specifications (tau>0 set ID across instruments) are reported alongside.
#
# Dimensions enumerated:
#   mode        : factors (level/slope/curvature ...) and maturities
#   n_pcs       : number of PC instruments (VFCI loading is fixed at n_pcs=4)
#   components  : factor subset (factors mode) or maturity subset (maturities)
#   gamma method: vfci (n_pcs=4), reduced_form (factors mode), optimized (tau>0),
#                 and separate -- the I x J scheme where each PC is its own
#                 instrument (no gamma; tau>0 only, the tau=0 point is overdetermined)
#   tau         : 0 (point) and a set-ID grid
#
# Parallel + resumable: each (mode, n_pcs, components) GROUP is an independent
# unit of work dispatched across cores via parallel::mclapply. Every finished
# group writes a per-group RDS checkpoint, so a crash/kill loses only the groups
# still in flight -- a re-launch reads the existing checkpoints and only computes
# the missing ones. The checkpoint files (one per group) are the authoritative
# progress signal; `ls` the checkpoint dir to see how far a run got. Optimizer
# accuracy is identical to a serial run (n_starts=12, slsqp defaults preserved).
#
# Env: HETID_SPEC_QUICK=1 runs a small subgrid (for validation). The two
# profiles write to separate checkpoint dirs and profile-suffixed artifacts
# (spec_comparison_<profile>.csv/.rds), so a quick run never resumes from or
# clobbers a full run (and vice versa).

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/ixj_identification.R"))
source(here::here("scripts/05_identification_with_optimization/spec_comparison_design.R"))

quick <- nzchar(Sys.getenv("HETID_SPEC_QUICK"))
profile <- if (quick) "quick" else "full"
cli_h1("Specification / instrument / tau comparison ({profile} profile)")

design <- spec_comparison_design(profile)
tau_grid <- design$tau_grid
npcs_grid <- design$npcs_grid
factor_sets <- design$factor_sets
mat_sets <- design$mat_sets
n_starts_opt <- design$n_starts_opt

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

out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(out_dir, paste0("spec_comparison_", profile, ".csv"))
ckpt_dir <- file.path(out_dir, paste0("spec_comparison_ckpt_", profile))
dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)

ckpt_path <- function(key) {
  file.path(ckpt_dir, paste0(gsub("[|]", "__", key), ".rds"))
}

# One unit of work: resume from checkpoint if present, else compute + checkpoint.
# A group that errors returns NULL and writes NO checkpoint, so a re-launch
# retries it (rather than silently treating it as done).
compute_group <- function(group) {
  key <- paste(group$mode, group$n_pcs, paste(group$components, collapse = "-"), sep = "|")
  ckpt <- ckpt_path(key)
  if (file.exists(ckpt)) {
    cli_alert("resume (checkpoint exists): {key}")
    return(readRDS(ckpt))
  }
  res <- tryCatch(
    compute_group_rows(group$mode, group$n_pcs, group$components),
    error = function(e) {
      cli_alert_warning("group failed: {key}: {conditionMessage(e)}")
      NULL
    }
  )
  if (!is.null(res)) {
    saveRDS(res, ckpt)
    cli_alert("done {key} ({nrow(res)} rows)")
  }
  res
}

# Enumerate every group across both modes.
groups <- c(
  unlist(lapply(npcs_grid, function(p) {
    lapply(factor_sets, function(cs) list(mode = "factors", n_pcs = p, components = cs))
  }), recursive = FALSE),
  unlist(lapply(npcs_grid, function(p) {
    lapply(mat_sets, function(cs) list(mode = "maturities", n_pcs = p, components = cs))
  }), recursive = FALSE)
)
n_done <- sum(vapply(groups, function(g) {
  file.exists(ckpt_path(paste(g$mode, g$n_pcs, paste(g$components, collapse = "-"), sep = "|")))
}, logical(1)))
cli_alert_info(
  "Dispatching {.val {length(groups)}} groups across {.val {N_CORES}} cores ({.val {n_done}} already checkpointed)"
)

# mc.preschedule = FALSE: fork one process per group so the heavy small-tau
# groups load-balance across free cores and a single crash is isolated to its
# group (the others keep their checkpoints).
results <- parallel::mclapply(
  groups, compute_group,
  mc.cores = N_CORES, mc.preschedule = FALSE
)

row_dfs <- Filter(is.data.frame, results)
n_missing <- length(results) - length(row_dfs)
if (n_missing > 0) {
  cli_alert_warning(
    "{.val {n_missing}} group(s) produced no rows (failed/crashed); re-run to resume them via checkpoints"
  )
}
grid <- if (length(row_dfs)) do.call(rbind, row_dfs) else NULL

if (is.null(grid) || !nrow(grid)) {
  cli_alert_danger("No rows produced -- nothing to report. Check checkpoint dir: {.path {ckpt_dir}}")
} else {
  utils::write.csv(grid, csv_path, row.names = FALSE)

  # Benchmark = the tau=0 point-ID specs (width 0). Report them by conditioning.
  bench <- grid[grid$kind == "point", ]
  bench <- bench[order(bench$cond), ]
  cli_h2("Benchmark: tau=0 point identification (width 0) — best-conditioned first")
  print(utils::head(bench[, c("mode", "n_pcs", "components", "gamma", "cond")], 10), digits = 5)
  write.csv(
    bench,
    file.path(out_dir, paste0("spec_comparison_benchmark_points_", profile, ".csv")),
    row.names = FALSE
  )

  # Other specs = tau>0; tightest bounded sets first.
  others <- grid[grid$tau > 0, ]
  others_bounded <- others[others$bounded, ]
  others_bounded <- others_bounded[order(others_bounded$width), ]
  cli_h2("Other specs (tau>0): tightest BOUNDED sets")
  print(utils::head(
    others_bounded[, c("mode", "n_pcs", "components", "gamma", "tau", "width")], 12
  ), digits = 5)
  cli_alert_info(
    "Bounded tau>0 specs: {.val {nrow(others_bounded)}} of {.val {nrow(others)}}; unbounded: {.val {sum(!others$bounded)}}"
  )

  saveRDS(grid, file.path(out_dir, paste0("spec_comparison_", profile, ".rds")))
  cli_alert_success(
    "Saved spec_comparison_{profile}.csv/.rds + benchmark_points_{profile}.csv to {.path {out_dir}}"
  )
}
