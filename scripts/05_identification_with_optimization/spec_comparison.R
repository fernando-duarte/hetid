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
# Env: HETID_SPEC_QUICK=1 runs a small subgrid (for validation).

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/ixj_identification.R"))

quick <- nzchar(Sys.getenv("HETID_SPEC_QUICK"))
cli_h1("Specification / instrument / tau comparison")

tau_grid <- if (quick) c(0, 0.05, 0.2) else c(0, 0.01, 0.05, 0.1, 0.2)
npcs_grid <- if (quick) c(4) else 2:6
factor_sets <- if (quick) list(c(1, 2, 3)) else list(c(1, 2), c(1, 2, 3), c(1, 2, 3, 4))
mat_sets <- list(c(2, 5, 9))
n_starts_opt <- if (quick) 6L else 12L

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
  b <- solve_all_profile_bounds(symmetrize_quadratic_system(qs)$quadratic)
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
eval_ixj <- function(mom, n_comp, n_pcs, tau) {
  qs <- build_ixj_quadratic_system(mom, matrix(tau, nrow = n_pcs, ncol = n_comp))
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
        set.seed(SEED)
        matrix(stats::rnorm(n_pcs * nc), n_pcs, nc)
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
      r <- tryCatch(eval_ixj(mom, nc, n_pcs, tau), error = function(e) NULL)
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

out_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(out_dir, "spec_comparison_full.csv")
ckpt_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized", "spec_comparison_ckpt")
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
  write.csv(bench, file.path(out_dir, "spec_comparison_benchmark_points.csv"), row.names = FALSE)

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

  saveRDS(grid, file.path(OUTPUT_TEMP_DIR, "identification_optimized", "spec_comparison.rds"))
  cli_alert_success("Saved spec_comparison_full.csv + benchmark_points.csv to {.path {out_dir}}")
}
