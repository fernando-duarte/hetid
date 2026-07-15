# Inference variant of the combined log-variance panels: build_logvar_panels
# (log_var_eq_panels_build.R) with the moving-block bootstrap outer confidence
# envelope (log_var_eq_set_boot, from log_var_eq_set_bootstrap.R) threaded
# beneath each PPML and Harvey set cell and the set-boot notes appended to both.
# The log-OLS panel is point identified, so it carries no envelope. Shares the
# conservative panels' per-panel labels so every \ref stays valid whichever of
# the two files the manuscript \input's. Writes log_var_eq_panels_inference.tex
# + compiled standalone. Run via run_all.R after log_var_eq_set_bootstrap.R.

source("scripts-paper/log_var_eq_panels_build.R")
source("scripts-paper/log_var_eq_set_inference_notes.R")

build_logvar_panels(
  "log_var_eq_panels_inference",
  ppml_caption_suffix =
    ", with a bootstrap outer confidence envelope beneath each set cell.",
  envelope_ppml = log_var_eq_set_boot$ppml,
  envelope_harvey = log_var_eq_set_boot$harvey,
  extra_notes = build_logvar_set_inference_notes(log_var_eq_set_boot)
)
