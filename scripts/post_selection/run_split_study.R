# Post-Selection Split Study (real data): select width-minimizing
# weights on the EARLY temporal block, evaluate the identified set on
# the LATE block. NOT part of the default pipeline (not registered in
# run_all_scripts.R). Requires the merged dataset from stage 01:
#   Rscript scripts/run_all_scripts.R   # once; writes temp/data.rds
#   Rscript scripts/post_selection/run_split_study.R
#
# Outputs (temp/identification_postsel/): split_study.rds (full
# machine-readable results; no timestamps, byte-stable across reruns)
# and split_study_grid.csv. postsel_report.R renders the human-facing
# summary with the caveats; the affirmative selection-honest wording
# is gated there on the full-simulation verdict, never asserted here.
#
# Honesty: the full-sample optimized arm keeps the existing
# vocabulary (width-minimizing computational benchmark, not a
# confidence statement). The split arm's validity argument is the
# spec's fixed-Lambda inclusion applied to the evaluation block; the
# boundary-dependence and maintained-bound caveats are carried into
# every artifact by the report layer.

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/postsel_split_utils.R"))
source(here::here("scripts/post_selection/postsel_study_arms.R"))

cli_h1("Post-selection split study (split-selected weights)")

SPLIT_PROP <- 0.5
SPLIT_GAP <- as.integer(Sys.getenv("HETID_SPLIT_GAP", "4"))
SPLIT_TAU <- as.numeric(
  Sys.getenv("HETID_SPLIT_TAU", as.character(BASELINE_TAU))
)
# Stage-05 budget so the full-sample arm is the same benchmark the
# pipeline publishes (and cross-checks identical below)
SPLIT_N_STARTS <- 30L
SPLIT_MAXEVAL <- 500L

inp <- load_identification_inputs()
lookup <- inp$lookup
n_comp <- nrow(lookup)
tau <- rep(SPLIT_TAU, n_comp)
gamma_baseline <- get_baseline_gamma("vfci", n_components = n_comp)

blocks <- split_block_rows(
  nrow(inp$data),
  prop = SPLIT_PROP, gap = SPLIT_GAP
)

cli_h2("Blocks (deterministic, contiguous, no RNG)")
windows <- list(
  full = block_moments(inp$data, seq_len(nrow(inp$data))),
  s = block_moments(inp$data, blocks$s_rows),
  e = block_moments(inp$data, blocks$e_rows)
)
for (w in names(windows)) {
  rng <- paste(format(windows[[w]]$date_range), collapse = " .. ")
  cli_alert_info(paste0(
    "window ", w, ": T_eval = ", windows[[w]]$n_obs, " (", rng, ")"
  ))
}
cli_alert_info(paste0(
  "gap rows excluded: ", length(blocks$gap_rows),
  "; tau = ", SPLIT_TAU, " on BOTH blocks (fixed ex ante)"
))

cli_h2("Weight selection per window (seeded multistart slsqp)")
selection <- lapply(windows, function(w) {
  run_lambda_optimization(
    gamma_baseline, w$moments, tau,
    n_starts = SPLIT_N_STARTS, seed = SEED, maxeval = SPLIT_MAXEVAL
  )
})
for (w in names(selection)) {
  cli_alert_info(paste0(
    "select on ", w, ": honest objective ",
    round(selection[[w]]$objective_start, 4), " -> ",
    round(selection[[w]]$objective_final, 4),
    " (best start ", selection[[w]]$best_index, ")"
  ))
}

cli_h2("Evaluating arms (profile bounds, three-state vocabulary)")
arm_specs <- postsel_arm_specs()
arm_grid <- evaluate_arm_grid(
  arm_specs, gamma_baseline, selection, windows, tau,
  lookup$component_label
)
evaluations <- arm_grid$evaluations
grid <- arm_grid$grid
print(grid[, c(
  "arm", "eval_window", "t_eval", "component_label",
  "lower", "upper", "state", "system_state"
)])

# Determinism cross-check against the pipeline's stage-05 result:
# identical moments construction, identical seed/budget, and the
# K = 1 seeded equivalence of run_lambda_optimization with the
# legacy optimizer make the full-sample arm reproduce stage 05
# exactly. Diagnostic only; skipped when stage 05 is absent or tau
# was overridden.
crosscheck <- NULL
opt_rds <- file.path(
  OUTPUT_TEMP_DIR, "identification_optimized",
  "optimized_identification_results.rds"
)
if (file.exists(opt_rds) && identical(SPLIT_TAU, BASELINE_TAU)) {
  stage05 <- readRDS(opt_rds)
  crosscheck <- list(stage05_gamma_identical = identical(
    do.call(cbind, selection$full$lambda_optimized),
    unname(stage05$gamma_optimized)
  ))
  cli_alert_info(paste0(
    "full-sample arm vs stage-05 optimized gamma: ",
    if (crosscheck$stage05_gamma_identical) "identical" else "DIFFERS"
  ))
}

results <- list(
  settings = list(
    prop = SPLIT_PROP, gap = SPLIT_GAP, tau = SPLIT_TAU,
    n_starts = SPLIT_N_STARTS, maxeval = SPLIT_MAXEVAL, seed = SEED,
    maturities = lookup$bond_maturity,
    n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS
  ),
  blocks = blocks,
  windows = lapply(windows, function(w) {
    list(n_obs = w$n_obs, date_range = w$date_range)
  }),
  selection = lapply(selection, function(s) {
    list(
      lambda_optimized = s$lambda_optimized,
      objective_start = s$objective_start,
      objective_final = s$objective_final,
      best_index = s$best_index,
      duplicate_directions = s$duplicate_directions
    )
  }),
  evaluations = evaluations,
  grid = grid,
  crosscheck = crosscheck
)

out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_postsel")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(results, file.path(out_dir, "split_study.rds"))
write.csv(
  grid, file.path(out_dir, "split_study_grid.csv"),
  row.names = FALSE
)
cli_alert_success("Saved split study to {.path {out_dir}}")
