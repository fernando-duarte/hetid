# Combined log-variance estimator panels (conservative variant): the PPML +
# log-OLS ordered pair plus the appended Harvey robustness panel, built by the
# shared build_logvar_panels (log_var_eq_panels_build.R) with no bootstrap
# envelope. Writes log_var_eq_panels.tex + compiled standalone; the primary
# log_var_eq.tex is the PPML-only publication table. Run via run_all.R after
# log_var_eq_table.R.

source("scripts-paper/log_var_eq_panels_build.R")

build_logvar_panels("log_var_eq_panels")
