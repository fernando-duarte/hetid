# Run the scripts-paper pipeline in its established dependency order.
# Run from the package root: Rscript scripts-paper/run_pipeline.R

# Bootstrap the path layer from the required package-root working directory.
source(normalizePath(
  file.path("scripts-paper", "config", "paths.R"),
  mustWork = TRUE
))
source(paper_path("config", "artifacts.R"))
source(paper_path("config", "analysis.R"))
create_artifact_directories()

# patch quantmod's FRED download (HTTP/2 stream errors, stalls) before tq_get
source(paper_path("data_preparation", "fred_download_patch.R"))

# Pipeline -----------------------------------------------------------------
source(paper_path("data_preparation", "build_sdf_series.R"))
source(paper_path("data_preparation", "build_consumption_growth.R"))
source(paper_path("data_preparation", "build_yield_volatility.R"))
source(paper_path("data_preparation", "build_asset_return_pcs.R"))
source(paper_path("data_preparation", "build_sdf_pcs.R"))
source(paper_path("mean_equation", "fit_ols.R"))
source(paper_path("mean_equation", "estimate_identified_set.R"))
source(paper_path("mean_equation", "inference", "run_bootstrap.R"))
source(paper_path("mean_equation", "tables", "render_structural_equation_table.R"))
source(paper_path("mean_equation", "variance_shares", "compute_variance_shares.R"))
source(paper_path("mean_equation", "variance_shares", "render_variance_share_table.R"))
source(paper_path("log_variance", "estimators", "log_ols", "run.R"))
source(paper_path("mean_equation", "inference", "compute_bounds_by_tau.R"))
# The PPML set map needs the warm-refined display-tau boxes and must register
# its figure entry before the bounds-by-tau driver renders the registry.
source(paper_path("log_variance", "estimators", "ppml", "run_sets.R"))
# shared SE scaffolding both estimators route through; before either *_se module
source(paper_path("log_variance", "inference", "standard_error_estimators.R"))
# analytic PPML QMLE standard errors for the point columns; must run after the
# frozen PPML object exists and before either table renders it
source(paper_path("log_variance", "estimators", "ppml", "standard_errors.R"))
# the primary table consumes the completed PPML hulls; the combined table then
# adds the mean-log robustness panel without recomputing either estimator
source(paper_path("log_variance", "tables", "render_ppml_table.R"))
# Harvey sets and dedicated table (the wrapper keeps this to one source line)
source(paper_path("log_variance", "estimators", "harvey", "run.R"))
# joint-null theta_R = 0 distance diagnostic: math, search, stability, then the
# guarded driver (the log-OLS orchestrator supplies inputs and named parents
# source their child modules), before the panels table appends its note
source(paper_path("log_variance", "diagnostics", "joint_null", "distance_objective.R"))
source(paper_path("log_variance", "diagnostics", "joint_null", "search.R"))
source(paper_path("log_variance", "diagnostics", "joint_null", "stability.R"))
source(paper_path("log_variance", "diagnostics", "joint_null", "run.R"))
# Joint moment compatibility: one ordered wrapper sources the stacked Lewbel
# and variance moments and runs graph replication under the no-answer default.
source(paper_path("log_variance", "diagnostics", "joint_gmm", "run.R"))
# unconditional base-R cleanup of the four dynamic-only EGARCH-X artifacts before
# the gate runs, so a stale pilot / CSV / RDS / bounds PDF can never masquerade
# as a fresh dynamic result behind a non-rejecting gate. The audit of
# existed/deleted flags is stored in the routing status manifest; it never
# touches the decision file, the gate record, or the status manifest.
source(paper_path("log_variance", "extensions", "egarch", "cleanup.R"))
logvar_egarch_cleanup_audit <- logvar_egarch_cleanup()
# base-R residual-dynamics gate for the log-variance equation: the predeclared
# lag-4 Ljung-Box screen on the tau = 0 benchmark residual decides whether any
# downstream volatility-dynamics workstream opens. Base R only, always runs, and
# writes the gate record plus the status manifest. The core chain-sources its
# record/sensitivity modules; the exists()-guarded driver runs after log_var_eq
# and set_id_mean_eq are built.
source(paper_path("log_variance", "diagnostics", "dynamics", "gate_core.R"))
source(paper_path("log_variance", "diagnostics", "dynamics", "run_gate.R"))
# EGARCH-X decision core and the unconditionally-sourced router driver: validate
# the committed scope decision against the freshly regenerated gate record, route
# every ladder branch, rewrite the status manifest with the routing outcome, and
# expose logvar_egarch_run_dynamic. A non-rejecting gate closes the dynamic
# workstream without sourcing an extension estimator.
source(paper_path("log_variance", "extensions", "egarch", "decision_core.R"))
source(paper_path("log_variance", "extensions", "egarch", "run_route.R"))
# median (LAD) map: gated on the quantreg dependency decision. Source the tri-state
# DCF reader, then source the driver only when the decision is approved and the
# package is installed at the recorded version. A missing, declined, or unanswered
# decision sources no LAD code and adds no registry stub (pre-LAD path unchanged);
# an approved decision with quantreg absent or version-mismatched hard-fails in the
# reader. Runs after the joint-null driver and before the panels table.
source(paper_path("log_variance", "estimators", "lad", "dependency_gate.R"))
lad_available <- requireNamespace("quantreg", quietly = TRUE)
lad_gate <- logvar_lad_gate_read(
  "docs/execution-ledgers/2026-07-13-logvar-lad-gate.dcf",
  available = lad_available,
  installed_version = if (lad_available) {
    as.character(utils::packageVersion("quantreg"))
  } else {
    ""
  }
)
if (isTRUE(lad_gate$source_lad)) {
  source(paper_path("log_variance", "estimators", "lad", "run_sets.R"))
}
# standalone median (LAD) panel table (theta^0.5): its own .tex + PDF, guarded on
# log_var_eq_lad so it renders only when the LAD map ran; main's combined panels
# table below stays untouched
source(paper_path("log_variance", "tables", "render_lad_table.R"))
source(paper_path("log_variance", "tables", "render_panels.R"))
# vol set-endpoint bootstrap: reads the frozen PPML/Harvey caches and the lagged
# asset-return PCs, re-runs the whole set map per resample, and writes the outer
# confidence-envelope state and diagnostics to their typed output groups
# L'Ecuyer-CMRG only for the forked vol bootstrap (mclapply mc.set.seed); the
# serial structural bootstrap above and the region figures below stay under the
# default RNG so their published numbers do not move
logvar_boot_rng <- RNGkind("L'Ecuyer-CMRG")
source(paper_path("log_variance", "inference", "run_set_bootstrap.R"))
RNGkind(logvar_boot_rng[1L], logvar_boot_rng[2L])
# The inference variant retains the combined panels and labels while threading
# the bootstrap envelope beneath the PPML and Harvey set cells.
source(paper_path("log_variance", "tables", "render_inference_panels.R"))
# the log-variance figures consume mean_eq_bounds_tau and the registry, so
# this runs after both producers
source(paper_path("log_variance", "figures", "render_bounds_by_tau.R"))
# consume the completed estimator caches only after their existing figures
source(paper_path("log_variance", "figures", "fitted_volatility", "run.R"))
# the median (LAD) fitted-|residual| envelope: grid-sampled inner hull, rendered
# only when the quantreg-gated LAD map ran (guarded on log_var_eq_lad)
source(paper_path("log_variance", "figures", "fitted_volatility", "run_lad.R"))
source(paper_path("mean_equation", "figures", "prepare_region_geometry.R"))
source(paper_path("mean_equation", "figures", "render_projections.R"))
source(paper_path("mean_equation", "figures", "render_region_3d.R"))
source(paper_path("mean_equation", "diagnostics", "heteroskedasticity", "render_table.R"))
# per-maturity SDF-news variance bounds: compute once from the offline ACM load,
# then render the log-scale figure and the summary-statistics table
source(paper_path("variance_bounds", "compute_bounds.R"))
source(paper_path("variance_bounds", "figures", "render_bounds.R"))
source(paper_path("variance_bounds", "tables", "render_summary_table.R"))
source(paper_path("reports", "build_descriptive_statistics.R"))
clean_latex_sidecars(out_dir)
