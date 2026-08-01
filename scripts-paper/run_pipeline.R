# Run the scripts-paper pipeline in its established dependency order.
# Run from the package root: Rscript scripts-paper/run_pipeline.R

# Bootstrap the path layer from the required package-root working directory.
source(normalizePath(
  file.path("scripts-paper", "config", "paths.R"),
  mustWork = TRUE
))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis.R"))

# A full run overwrites tracked LaTeX artifacts. A draft bootstrap size would do
# that silently and leave the paper carrying numbers no one meant to publish, so
# a reduced HETID_BOOT_REPS has to be acknowledged rather than merely set.
# HETID_ALLOW_DRAFT_RUN=1 is the acknowledgement.
if (boot_reps != 10000L && !nzchar(Sys.getenv("HETID_ALLOW_DRAFT_RUN"))) {
  stop(sprintf(paste0(
    "refusing to run the publishing pipeline at B = %d.\n",
    "  This run rewrites tracked LaTeX artifacts, and a draft bootstrap ",
    "size would publish numbers that cannot be reproduced.\n",
    "  Unset HETID_BOOT_REPS for a real run, or set HETID_ALLOW_DRAFT_RUN=1 ",
    "to accept draft output."
  ), boot_reps), call. = FALSE)
}
paper_source_once(paper_path("support", "data", "acm_inputs.R"))
# Frozen external inputs are verified before anything is created or cleaned, so
# a missing snapshot or a cache that drifted off its pin stops the run with the
# output tree untouched.
paper_source_once(paper_path("support", "data", "frozen_inputs.R"))
paper_verify_frozen_inputs()
create_artifact_directories()
conditional_cleanup_audits <- stats::setNames(
  lapply(
    PAPER_CONDITIONAL_ARTIFACT_STATUSES,
    cleanup_conditional_artifacts
  ),
  PAPER_CONDITIONAL_ARTIFACT_STATUSES
)
logvar_lad_cleanup_audit <-
  conditional_cleanup_audits[[
    PAPER_ARTIFACT_STATUS$conditional_lad
  ]]
logvar_egarch_cleanup_audit <-
  conditional_cleanup_audits[[
    PAPER_ARTIFACT_STATUS$conditional_egarch
  ]]
quarterly_acm_inputs <- paper_load_quarterly_acm(all_mats)

# patch quantmod's FRED download (HTTP/2 stream errors, stalls) before tq_get
paper_source_once(paper_path("data_preparation", "fred_download_patch.R"))

# Pipeline -----------------------------------------------------------------
paper_source_once(paper_path("data_preparation", "build_sdf_series.R"))
paper_source_once(paper_path("data_preparation", "build_consumption_growth.R"))
paper_source_once(paper_path("data_preparation", "build_yield_volatility.R"))
paper_source_once(paper_path("data_preparation", "build_asset_return_pcs.R"))
paper_source_once(paper_path("data_preparation", "build_sdf_pcs.R"))
paper_source_once(paper_path("mean_equation", "fit_ols.R"))
paper_source_once(paper_path("mean_equation", "estimate_identified_set.R"))
paper_source_once(paper_path("mean_equation", "variance_shares", "compute_variance_shares.R"))
paper_source_once(paper_path("mean_equation", "variance_shares", "render_variance_share_table.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "run.R"))
paper_source_once(paper_path("mean_equation", "inference", "compute_bounds_by_tau.R"))
# The PPML set map needs the warm-refined display-tau boxes and must register
# its figure entry before the bounds-by-tau driver renders the registry.
paper_source_once(paper_path("log_variance", "estimators", "ppml", "run_sets.R"))
# shared SE scaffolding both estimators route through; before either *_se module
paper_source_once(paper_path("log_variance", "inference", "standard_error_estimators.R"))
# analytic PPML QMLE standard errors for the point columns; must run after the
# frozen PPML object exists and before either table renders it
paper_source_once(paper_path("log_variance", "estimators", "ppml", "standard_errors.R"))
# Harvey sets and analytic standard errors (the wrapper keeps this to one source
# line). Its dedicated table publishes below, after the bootstrap stage.
paper_source_once(paper_path("log_variance", "estimators", "harvey", "run.R"))
# joint-null theta_R = 0 distance diagnostic: math, search, stability, then the
# guarded driver (the log-OLS orchestrator supplies inputs and named parents
# source their child modules), before the panels table appends its note
paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "distance_objective.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "search.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "stability.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "run.R"))
# Joint moment compatibility: one ordered wrapper sources the stacked Lewbel
# and variance moments and runs graph replication under the no-answer default.
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "run.R"))
# base-R residual-dynamics gate for the log-variance equation: the predeclared
# protocol-owned Ljung-Box screen on the tau = 0 benchmark residual decides whether any
# downstream volatility-dynamics workstream opens. Base R only, always runs, and
# writes the gate record plus the status manifest. The core chain-sources its
# record/sensitivity modules; the exists()-guarded driver runs after log_var_eq
# and set_id_mean_eq are built.
paper_source_once(paper_path("log_variance", "diagnostics", "dynamics", "gate_core.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "dynamics", "run_gate.R"))
# EGARCH-X decision core and the unconditionally-sourced router driver: validate
# the committed scope decision against the freshly regenerated gate record, route
# every ladder branch, rewrite the status manifest with the routing outcome, and
# expose logvar_egarch_run_dynamic. A non-rejecting gate closes the dynamic
# workstream without sourcing an extension estimator.
paper_source_once(paper_path("log_variance", "extensions", "egarch", "decision_core.R"))
paper_source_once(paper_path("log_variance", "extensions", "egarch", "run_route.R"))
# median (LAD) map: gated on the quantreg dependency decision. Source the tri-state
# DCF reader, then source the driver only when the decision is approved and the
# package is installed at the recorded version. A missing, declined, or unanswered
# decision sources no LAD code and adds no registry stub (pre-LAD path unchanged);
# an approved decision with quantreg absent or version-mismatched hard-fails in the
# reader. Runs after the joint-null driver and before the panels table.
paper_source_once(paper_path("log_variance", "estimators", "lad", "dependency_gate.R"))
lad_available <- requireNamespace("quantreg", quietly = TRUE)
lad_gate <- logvar_lad_gate_read(
  paper_path("config", "decisions", "lad.dcf"),
  available = lad_available,
  installed_version = if (lad_available) {
    as.character(utils::packageVersion("quantreg"))
  } else {
    ""
  }
)
if (isTRUE(lad_gate$source_lad)) {
  paper_source_once(paper_path("log_variance", "estimators", "lad", "run_sets.R"))
}
# One late bootstrap stage shares each primary resample and system estimate
# between mean and volatility inference. It owns both index families and the
# unified all-or-nothing draw cache.
paper_source_once(paper_path("inference", "run_bootstrap_stage.R"))
# the alternative specification's mean draws, on the same index family, so the
# comparison carries intervals rather than full-sample sets alone
paper_source_once(paper_path("mean_equation", "inference", "spec_comparison.R"))
# Publication of every log-variance table follows the stage, because each one
# reports a bootstrap tau = 0 statistic. Only publication is deferred: the PPML
# and Harvey estimates and their analytic standard errors are already frozen
# above, and the stage reads them without mutating them.
# the merged mean-over-PPML two-panel table; consumes the mean-set estimate, its
# endpoint bootstrap, and the PPML set-endpoint envelope, so it runs after the
# structural table is frozen
paper_source_once(paper_path("log_variance", "tables", "render_combined_inference_table.R"))
# one page per estimator: mean equation above each, that estimator's notes below
paper_source_once(paper_path("log_variance", "tables", "render_estimator_pages.R"))
# the log-variance figures consume mean_eq_bounds_tau and the registry, so
# this runs after both producers
paper_source_once(paper_path("log_variance", "figures", "render_bounds_by_tau.R"))
# consume the completed estimator caches only after their existing figures
paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "run.R"))
# the same envelope across the contract's slack sweep, plus the combined
# nesting panels; reuses the estimator caches run.R has already warmed
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "run_tau_sweep.R"
))
# the median (LAD) fitted-|residual| envelope: grid-sampled inner hull, rendered
# only when the quantreg-gated LAD map ran (guarded on log_var_eq_lad)
paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "run_lad.R"))
paper_source_once(paper_path("mean_equation", "figures", "prepare_region_geometry.R"))
paper_source_once(paper_path("mean_equation", "figures", "render_projections.R"))
paper_source_once(paper_path("mean_equation", "figures", "render_region_3d.R"))
paper_source_once(paper_path(
  "mean_equation",
  "diagnostics",
  "heteroskedasticity",
  "render_table.R"
))
# per-maturity SDF-news variance bounds: compute once from the offline ACM load,
# then render the log-scale figure and the summary-statistics table
paper_source_once(paper_path("variance_bounds", "compute_bounds.R"))
# regenerate and assert the paper's quoted approximation-error numbers before
# the renderers so a broken quote halts the stage with no artifact overwritten
paper_source_once(paper_path("variance_bounds", "quoted", "run.R"))
paper_source_once(paper_path("variance_bounds", "figures", "render_bounds.R"))
paper_source_once(paper_path("variance_bounds", "tables", "render_summary_table.R"))
paper_source_once(paper_path("reports", "build_descriptive_statistics.R"))
# The router grants permission to run; this graph has no EGARCH producer yet.
# Execution remains false until such a producer runs and writes its full manifest.
logvar_conditional_route_status <- build_conditional_route_status(
  lad_gate = lad_gate,
  egarch_route = logvar_egarch_route_result,
  cleanup_audits = conditional_cleanup_audits,
  egarch_producer_ran = FALSE
)
paper_write_exact_rds(
  logvar_conditional_route_status,
  artifact_path("conditional_route_status"),
  "conditional_route_status"
)
clean_latex_sidecars(out_dir)
