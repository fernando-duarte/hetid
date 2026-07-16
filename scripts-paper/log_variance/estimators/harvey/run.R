# Harvey run constants and the ordered sources of the sets and table drivers,
# so run_pipeline.R gains exactly one line. The grid cap and per-tau fit budget
# mirror the PPML tunables; the reduced sensitivity gate gets a larger fresh
# budget for its five-start re-polish. The sets driver sources the estimator
# and driver helper modules itself (the benchmark map-module pattern).
logvar_harvey_grid_cap <- 4000L
logvar_harvey_fit_budget <- 20000L
logvar_harvey_sensitivity_fit_budget <- 40000L
source(paper_path("log_variance", "estimators", "harvey", "run_sets.R"))
source(paper_path("log_variance", "estimators", "harvey", "standard_errors.R"))
source(paper_path("log_variance", "tables", "render_harvey_table.R"))
