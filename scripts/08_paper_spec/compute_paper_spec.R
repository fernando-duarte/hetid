# Stage 08: compute the single paper specification and save the results bundle
# the table renderer consumes. The spec: Y1 = consumption growth; Y2 = ONE
# principal component of all the SDF news (I = 1); Z = the actual de-meaned VFCI
# as a single instrument (J = 1); common design X_t = (1, four return PCs, four
# consumption lags). All residual/estimator machinery lives in
# scripts/utils/paper_spec_*.R. Output stays in temp/; the for_paper tables are
# written by render_paper_tables.R.
source(here::here("scripts/utils/common_settings.R"))
set_analysis_seed()

# News-PC scaling: "correlation" standardizes each maturity's SDF news to unit
# variance before the PCA (so the single news factor is not dominated by the
# long-maturity news); "covariance" keeps native units. Flip here for the
# sensitivity run -- it is a deliberate, single-line choice, not a global env var.
PAPER_NEWS_PC_SCALE <- "correlation"

# Reproducibility stamp for the paper spec (distinct from the stage-04 baseline
# stamp, which only records y1_lags / news-mode / z_source). Captions read this
# from the saved RDS, never from live env vars.
current_paper_spec <- function(used_maturities) {
  git_sha <- tryCatch(
    trimws(system2("git", c("rev-parse", "--short", "HEAD"),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) NA_character_
  )
  list(
    news_pc_scale = PAPER_NEWS_PC_SCALE,
    n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
    y1_lags = N_Y1_LAGS,
    step = NEWS_STEP,
    tau_set = BASELINE_TAU,
    instrument = "vfci_dm (de-meaned VFCI, J = 1)",
    gamma = "unit (1x1)",
    n_news_maturities = length(used_maturities),
    news_maturity_range = range(used_maturities),
    seed = SEED,
    git_sha = if (length(git_sha)) git_sha[[1]] else NA_character_
  )
}

cli_h1("Stage 08: paper specification (I = 1 news PC, J = 1 de-meaned VFCI)")

# Scope the VFCI Z-hook to this computation only (no global env poisoning).
out <- withr::with_envvar(
  c(HETID_Z_SOURCE = here::here("scripts/z_sources/vfci_demeaned.R")),
  {
    data <- as.data.frame(readRDS(DATA_RDS_PATH))
    resid <- compute_paper_spec_residuals(data, news_pc_scale = PAPER_NEWS_PC_SCALE)
    est <- compute_paper_spec_estimator(resid)
    cli_alert_info("Bootstrapping the identification frontier (this takes a minute)...")
    boot <- compute_paper_spec_bootstrap(resid)
    list(resid = resid, est = est, boot = boot)
  }
)

results <- list(
  resid = out$resid, est = out$est, boot = out$boot,
  spec = current_paper_spec(out$resid$used_maturities),
  built_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z")
)

paper_dir <- file.path(OUTPUT_TEMP_DIR, "paper_spec")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(results, file.path(paper_dir, "paper_spec_results.rds"))

cli_alert_success(
  "Saved paper-spec results to {.path {file.path(paper_dir, 'paper_spec_results.rds')}}"
)
cli_alert_info(
  "theta status: {out$est$set_status}; tau* = {signif(out$est$tau_star, 3)}; N = {out$est$n_obs}"
)
