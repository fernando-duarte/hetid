# Comprehensive specification / instrument / tau comparison for the identified set.
# Benchmark = the tau=0 POINT identification (smallest width = 0); all other
# specifications (tau>0 set ID across instruments) are reported alongside.
#
# Dimensions enumerated:
#   n_pcs       : number of PC instruments (VFCI loading is fixed at n_pcs=4)
#   components  : maturity subset of the SDF-news bonds
#   gamma method: vfci (n_pcs=4), optimized (tau>0), and separate -- the I x J
#                 scheme where each PC is its own instrument (no gamma; tau>0
#                 only, the tau=0 point is overdetermined)
#   tau         : 0 (point) and a set-ID grid
#
# Parallel + resumable: each (n_pcs, components) GROUP is an independent
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
mat_sets <- design$mat_sets
n_starts_opt <- design$n_starts_opt

source(here::here("scripts/utils/spec_comparison_eval.R"))

out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(out_dir, paste0("spec_comparison_", profile, ".csv"))
ckpt_dir <- file.path(out_dir, paste0("spec_comparison_ckpt_", profile))
dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)

ckpt_path <- function(key) {
  file.path(ckpt_dir, paste0(gsub("[|]", "__", key), ".rds"))
}

# Checkpoint key. N_Y1_LAGS and the news-projection mode are part of the spec:
# a lag change or an estimate-B/impose-B=0 flip must invalidate old checkpoints
# rather than silently reuse results built under a different W1/W2 residual.
news_mode_token <- if (impose_news_projection_zero()) "impose_b_zero" else "estimate_b"
group_key <- function(g) {
  paste(
    g$n_pcs, paste(g$components, collapse = "-"), N_Y1_LAGS, news_mode_token,
    sep = "|"
  )
}

# One unit of work: resume from checkpoint if present, else compute + checkpoint.
# A group that errors returns NULL and writes NO checkpoint, so a re-launch
# retries it (rather than silently treating it as done).
compute_group <- function(group) {
  key <- group_key(group)
  ckpt <- ckpt_path(key)
  if (file.exists(ckpt)) {
    cli_alert("resume (checkpoint exists): {key}")
    return(readRDS(ckpt))
  }
  res <- tryCatch(
    compute_group_rows(group$n_pcs, group$components),
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

# Enumerate every (n_pcs, maturity-set) group.
groups <- unlist(lapply(npcs_grid, function(p) {
  lapply(mat_sets, function(cs) list(n_pcs = p, components = cs))
}), recursive = FALSE)
n_done <- sum(vapply(groups, function(g) {
  file.exists(ckpt_path(group_key(g)))
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
  print(utils::head(bench[, c("n_pcs", "components", "gamma", "cond")], 10), digits = 5)
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
    others_bounded[, c("n_pcs", "components", "gamma", "tau", "width")], 12
  ), digits = 5)
  cli_alert_info(
    "Bounded tau>0 specs: {.val {nrow(others_bounded)}} of {.val {nrow(others)}}; unbounded: {.val {sum(!others$bounded)}}"
  )

  saveRDS(grid, file.path(out_dir, paste0("spec_comparison_", profile, ".rds")))
  cli_alert_success(
    "Saved spec_comparison_{profile}.csv/.rds + benchmark_points_{profile}.csv to {.path {out_dir}}"
  )
}
