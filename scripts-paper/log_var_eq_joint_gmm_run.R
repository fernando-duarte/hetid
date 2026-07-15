# Single ordered source of the joint moment-compatibility (joint_gmm) stack, so
# run_all.R gains exactly one line (the benchmark map-module pattern). The
# committed decision record and the Stage A/B modules are flat siblings sourced in
# dependency order; the gmm module self-sources the artifacts, projection, and
# guarded pipeline driver, and that driver runs at source time when the pipeline
# objects exist. The panels-note builder is sourced here too, so the table
# script's exists()-guarded seam finds logvar_joint_gmm_panel_note. Runs after the
# joint-null driver and before the panels table.
source("scripts-paper/log_var_eq_joint_decision.R")
source("scripts-paper/logvar_joint_gmm_decision.R")
source("scripts-paper/log_var_eq_moments.R")
source("scripts-paper/log_var_eq_joint_basis.R")
source("scripts-paper/log_var_eq_joint_profiles.R")
source("scripts-paper/log_var_eq_joint_identity.R")
source("scripts-paper/log_var_eq_joint_budget.R")
source("scripts-paper/log_var_eq_joint_candidates.R")
source("scripts-paper/log_var_eq_joint_epigraph.R")
source("scripts-paper/log_var_eq_joint_gmm.R")
source("scripts-paper/log_var_eq_joint_gmm_note.R")
