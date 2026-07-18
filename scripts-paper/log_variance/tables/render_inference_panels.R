# Inference variant of the combined log-variance panels: build_logvar_panels
# (panels_builder.R) with the moving-block bootstrap outer confidence
# envelope (log_var_eq_set_boot, from run_set_bootstrap.R) threaded
# beneath each PPML and Harvey set cell and the set-boot notes appended to both.
# The log-OLS panel is point identified, so it carries no envelope. Shares the
# conservative panels' per-panel labels so every \ref stays valid whichever of
# the two files the manuscript \input's. Writes log_var_eq_panels_inference.tex
# + compiled standalone. Run via run_pipeline.R after run_set_bootstrap.R.

paper_source_once(paper_path("log_variance", "tables", "panels_builder.R"))
paper_source_once(paper_path("log_variance", "tables", "set_inference_caption.R"))

build_logvar_panels(
  artifact_variant_id("logvar_panels", "inference"),
  ppml_caption_suffix =
    ", with a bootstrap outer confidence envelope beneath each set cell.",
  envelope_ppml = log_var_eq_set_boot$ppml,
  envelope_harvey = log_var_eq_set_boot$harvey,
  extra_notes = build_logvar_set_inference_notes(log_var_eq_set_boot)
)
