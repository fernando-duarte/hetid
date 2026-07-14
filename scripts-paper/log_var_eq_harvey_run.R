# Harvey run constants and the single ordered source of the sets driver, so
# run_all.R gains exactly one line. The grid cap and per-tau fit budget mirror
# the PPML tunables; the reduced sensitivity gate gets a larger fresh budget for
# its five-start re-polish. The sets driver sources the estimator and driver
# helper modules itself (the benchmark map-module pattern), so no further
# run_all lines are needed.
logvar_harvey_grid_cap <- 4000L
logvar_harvey_fit_budget <- 20000L
logvar_harvey_sensitivity_fit_budget <- 40000L
source("scripts-paper/log_var_eq_harvey_sets.R")
