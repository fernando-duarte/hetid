# Simulation validation for the post-selection split (the rigor
# gate). Synthetic DGP with known theta0 in which the moment bound
# holds UNIFORMLY over weight directions at a closed-form level
# (postsel_sim_dgp.R), so any coverage loss under data-selected
# weights is a finite-sample selection effect by construction.
# Demonstrates:
#   selection effect  full-sample width-minimizing selection
#                     degrades coverage of theta0 as instruments
#                     multiply
#   split repair      selecting on one block and evaluating on the
#                     other restores coverage close to the
#                     fixed-weights benchmark at the same T_eval
# NOT part of the default pipeline. Heavy (budgeted + checkpointed):
#   smoke: HETID_SIM_QUICK=1 Rscript scripts/post_selection/split_simulation.R
#   pilot: HETID_SIM_REPS=25 Rscript scripts/post_selection/split_simulation.R
#   full:  Rscript scripts/post_selection/split_simulation.R
# One checkpoint rds per K cell (reps count in the name) supports
# resume; remove the checkpoint directory to force a fresh compute.

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/postsel_split_utils.R"))
source(here::here("scripts/utils/postsel_sim_dgp.R"))
source(here::here("scripts/post_selection/postsel_sim_rep.R"))

QUICK <- nzchar(Sys.getenv("HETID_SIM_QUICK"))
SIM_SEED <- 20260612L
SIM_T <- 240L
# Adopted Stage-P winner of the K4 rescope round (config: uniform
# shocks, tau_sim 0.40, rho_target 0.03; frozen winner rule, see the
# pilot log and docs/postsel-sim-k4-preregistration.md). The DGP
# test's default pin and membership tau literal move WITH these.
SIM_TAU <- 0.40
SIM_SHOCK_DIST <- "uniform"
SIM_RHO_TARGET <- 0.03
SIM_PHI <- 0.5
SIM_PROP <- 0.5
SIM_GAP <- 4L
# Registered K grid (K4 rescope round,
# docs/postsel-sim-k4-preregistration.md): the validation claim is
# scoped to K <= 4 -- application parity (DEFAULT_N_PCS = 4). The
# K = 8 premise failure stays on record in the pilot log and the
# failure report; quick mode runs the same registered grid at smoke
# size. Cell order fixes the seed arithmetic: K = 2 and K = 4 keep
# exactly the rep seeds the prior registration assigned them.
SIM_K_GRID <- c(2L, 4L)
SIM_REPS <- as.integer(
  Sys.getenv("HETID_SIM_REPS", if (QUICK) "8" else "200")
)
WIDTH_REPS <- min(if (QUICK) 2L else 50L, SIM_REPS)
SIM_N_STARTS <- 5L
SIM_MAXEVAL <- 200L
SIM_CORES <- max(1L, as.integer(
  Sys.getenv("HETID_SIM_CORES", as.character(N_CORES))
))

# Everything the worker needs, passed explicitly (no lexical capture
# of script-level constants)
cfg <- list(
  sim_seed = SIM_SEED, t_obs = SIM_T, tau = SIM_TAU,
  prop = SIM_PROP, gap = SIM_GAP, n_starts = SIM_N_STARTS,
  maxeval = SIM_MAXEVAL, width_reps = WIDTH_REPS
)

out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_postsel")
ckpt_dir <- file.path(
  out_dir, if (QUICK) "sim_ckpt_quick" else "sim_ckpt"
)
dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)

cli_h1(paste0(
  "Post-selection split simulation (",
  if (QUICK) "quick smoke grid" else "full grid", ")"
))
cells <- lapply(seq_along(SIM_K_GRID), function(ci) {
  k <- SIM_K_GRID[ci]
  params <- postsel_dgp_params(
    k,
    phi = SIM_PHI, rho_target = SIM_RHO_TARGET,
    shock_dist = SIM_SHOCK_DIST
  )
  # Uniform-validity certificate: the constant-in-direction relative
  # correlation must sit well inside the slack the study uses, or
  # the premise (pure selection effect) is wrong. Fail loudly.
  stopifnot(max(params$rho) <= 0.6 * SIM_TAU)
  # Checkpoint fingerprint (K4 round): a checkpoint is reusable ONLY
  # under the identical worker cfg and DGP parameters -- the file
  # name carries just K and reps, so without the stamp a
  # post-adoption run could silently reuse stale pre-adoption cells
  # and fake determinism. Legacy raw-data.frame checkpoints carry no
  # stamp and recompute automatically.
  stamp <- list(cfg = cfg, params = params)
  ck <- file.path(
    ckpt_dir, paste0("cell_k", k, "_r", SIM_REPS, ".rds")
  )
  if (file.exists(ck)) {
    ckpt <- readRDS(ck)
    if (identical(ckpt$stamp, stamp)) {
      cli_alert_info(paste0("cell K = ", k, ": checkpoint reused"))
      return(ckpt$rows)
    }
    cli_alert_warning(paste0(
      "cell K = ", k,
      ": checkpoint fingerprint differs -- recomputing"
    ))
  }
  cli_h2(paste0(
    "cell K = ", k, ": rho = ",
    paste(round(params$rho, 4), collapse = ", "),
    ", sigma_nu = ", round(params$sigma_nu, 3)
  ))
  t_start <- Sys.time()
  rows <- parallel::mclapply(
    seq_len(SIM_REPS), run_one_rep,
    cell_idx = ci, params = params, cfg = cfg,
    mc.cores = SIM_CORES
  )
  ok <- vapply(rows, is.data.frame, logical(1))
  if (!all(ok)) {
    first_bad <- rows[[which(!ok)[1]]]
    stop(
      "simulation reps failed in cell K = ", k, ": ",
      conditionMessage(attr(first_bad, "condition"))
    )
  }
  cell <- do.call(rbind, rows)
  elapsed <- as.numeric(
    difftime(Sys.time(), t_start, units = "mins")
  )
  cli_alert_success(paste0(
    "cell K = ", k, ": ", SIM_REPS, " reps in ",
    round(elapsed, 1), " min"
  ))
  saveRDS(list(stamp = stamp, rows = cell), ck)
  cell
})

res <- list(
  results = do.call(rbind, cells),
  settings = list(
    quick = QUICK, sim_seed = SIM_SEED, t_obs = SIM_T,
    tau = SIM_TAU, phi = SIM_PHI, k_grid = SIM_K_GRID,
    reps = SIM_REPS, width_reps = WIDTH_REPS,
    n_starts = SIM_N_STARTS, maxeval = SIM_MAXEVAL,
    prop = SIM_PROP, gap = SIM_GAP,
    shock_dist = SIM_SHOCK_DIST,
    kappa_eta = if (identical(SIM_SHOCK_DIST, "uniform")) 1.8 else 3,
    rho_target = SIM_RHO_TARGET,
    dgp_family = paste0(
      "lognormal-het, kappa_eta shocks; amended D10a",
      " (post-stop, user-approved); K grid rescoped to the",
      " registered K <= 4 round"
    )
  ),
  dgp = lapply(
    SIM_K_GRID, postsel_dgp_params,
    phi = SIM_PHI, rho_target = SIM_RHO_TARGET,
    shock_dist = SIM_SHOCK_DIST
  )
)
suffix <- if (QUICK) "_quick" else ""
saveRDS(
  res, file.path(out_dir, paste0("sim_results", suffix, ".rds"))
)
write.csv(
  res$results,
  file.path(out_dir, paste0("sim_results", suffix, ".csv")),
  row.names = FALSE
)

cli_h2("Coverage by cell and arm (membership of theta0)")
print(aggregate(covered ~ k_inst + arm, data = res$results, FUN = mean))
cli_alert_success("Saved simulation results to {.path {out_dir}}")
