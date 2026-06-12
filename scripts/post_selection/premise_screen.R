# Premise screen for the post-selection split simulation -- K4
# rescope round (Stage P). Verifies, BEFORE any optimizer run, that
# fixed ex ante weights cover theta0 with high probability at
# T = 240 -- the baseline the acceptance margins presuppose. THIS
# ROUND'S REGISTERED K GRID IS {2, 4} (application parity:
# DEFAULT_N_PCS = 4; docs/postsel-sim-k4-preregistration.md). The
# K = 8 screen of 2026-06-12 failed its gate and stays on record in
# docs/postsel-sim-pilot-log.md and
# docs/postsel-sim-failure-report.md -- nothing here erases it.
# Membership only (covers_theta0; no profile solves).
#   Rscript scripts/post_selection/premise_screen.R
# Gate: a configuration passes iff, at BOTH K in {2, 4},
# fixed_full coverage >= 0.80 AND fixed_e coverage >= 0.75. Winner =
# argmax of min-K fixed_e among passers (ties: lower tau_sim, then
# gaussian shocks -- the least family change); adopt its values as
# the defaults in postsel_dgp_params and the SIM_* constants. Record
# every screened cell in docs/postsel-sim-pilot-log.md as it runs.
# Seed arithmetic is byte-frozen from the prior registration, so the
# K = 2 rows must reproduce the logged 2026-06-12 values exactly
# (environment-drift check). Acceptance margins are NOT touched here
# or anywhere (D9).

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/postsel_split_utils.R"))
source(here::here("scripts/utils/postsel_sim_dgp.R"))

SCREEN_SEED_BASE <- 20260612L + 100000L # disjoint from main cells
SCREEN_T <- 240L
SCREEN_REPS <- 100L
SCREEN_K <- c(2L, 4L) # registered K4-round grid
SCREEN_PROP <- 0.5
SCREEN_GAP <- 4L

# Pre-registered grid (D10a): 8 configurations
configs <- expand.grid(
  shock_dist = c("uniform", "gaussian"),
  tau_sim = c(0.30, 0.40),
  rho_target = c(0.03, 0.05),
  stringsAsFactors = FALSE
)

rows <- list()
for (ci in seq_len(nrow(configs))) {
  cfg <- configs[ci, ]
  for (k in SCREEN_K) {
    params <- postsel_dgp_params(
      k,
      rho_target = cfg$rho_target, shock_dist = cfg$shock_dist
    )
    # Certificate per configuration: rho well inside tau, always
    stopifnot(max(params$rho) <= 0.6 * cfg$tau_sim)
    lam0 <- equal_weight_lambda(k, length(params$a))
    tau <- rep(cfg$tau_sim, length(params$a))
    blocks <- split_block_rows(
      SCREEN_T,
      prop = SCREEN_PROP, gap = SCREEN_GAP
    )
    cov_full <- 0L
    cov_e <- 0L
    for (r in seq_len(SCREEN_REPS)) {
      seed_rep <- SCREEN_SEED_BASE + 1000L * ci + r
      dat <- draw_postsel_data(params, SCREEN_T, seed = seed_rep)
      m_full <- sim_window_moments(dat, seq_len(SCREEN_T))
      m_e <- sim_window_moments(dat, blocks$e_rows)
      cov_full <- cov_full +
        covers_theta0(lam0, tau, m_full, params$theta0)
      cov_e <- cov_e +
        covers_theta0(lam0, tau, m_e, params$theta0)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      config = ci, shock_dist = cfg$shock_dist,
      tau_sim = cfg$tau_sim, rho_target = cfg$rho_target,
      k_inst = k,
      fixed_full = cov_full / SCREEN_REPS,
      fixed_e = cov_e / SCREEN_REPS,
      stringsAsFactors = FALSE
    )
  }
}
screen <- do.call(rbind, rows)
print(screen)

out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_postsel")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  screen, file.path(out_dir, "premise_screen_k4.csv"),
  row.names = FALSE
)

# Gate + winner selection (D10a)
by_cfg <- split(screen, screen$config)
passing <- Filter(function(g) {
  min(g$fixed_full) >= 0.80 && min(g$fixed_e) >= 0.75
}, by_cfg)
if (length(passing) == 0L) {
  cli_alert_danger(paste0(
    "PREMISE GATE FAILED for every pre-registered configuration: ",
    "follow the documented descope path (plan, Task 5 / D10a); ",
    "do not proceed to the directional pilot or Task 6"
  ))
  quit(save = "no", status = 1)
}
rank_df <- do.call(rbind, lapply(passing, function(g) {
  data.frame(
    config = g$config[1], score = min(g$fixed_e),
    tau_sim = g$tau_sim[1],
    gaussian_first =
      as.integer(identical(g$shock_dist[1], "gaussian")),
    stringsAsFactors = FALSE
  )
}))
rank_df <- rank_df[
  order(-rank_df$score, rank_df$tau_sim, -rank_df$gaussian_first),
]
best <- by_cfg[[as.character(rank_df$config[1])]]
cli_alert_success(paste0(
  "Premise gate PASSED; winner config ", best$config[1],
  ": shock_dist = ", best$shock_dist[1],
  ", tau_sim = ", best$tau_sim[1],
  ", rho_target = ", best$rho_target[1],
  " (min-K fixed_e = ", min(best$fixed_e), ")"
))
cli_alert_info(paste0(
  "Adopt as defaults: rho_target/shock_dist in postsel_dgp_params, ",
  "SIM_TAU/SIM_SHOCK_DIST/SIM_RHO_TARGET in split_simulation.R, ",
  "and the DGP test's membership tau literal; log every cell to ",
  "docs/postsel-sim-pilot-log.md"
))
