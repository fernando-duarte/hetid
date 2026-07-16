# Combined log-variance estimator panels (conservative variant): the PPML +
# log-OLS ordered pair plus the appended Harvey robustness panel, built by the
# shared build_logvar_panels (panels_builder.R) with no bootstrap
# envelope. Writes log_var_eq_panels.tex + compiled standalone; the primary
# log_var_eq.tex is the PPML-only publication table. Run via run_pipeline.R after
# render_ppml_table.R.

source(paper_path("log_variance", "tables", "panels_builder.R"))

build_logvar_panels("log_var_eq_panels")
