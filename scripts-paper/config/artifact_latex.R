# Manifest extensions for LaTeX publication triples and semantic labels.

PAPER_LATEX_LABELS <- c(
  heteroskedasticity = "tab:sdf_news_hetero_tests",
  logvar_ppml = "tab:log_var_eq_set_id",
  logvar_harvey = "tab:log_var_eq_harvey",
  panel_lad = "tab:log_var_eq_panel_lad",
  panel_logols = "tab:log_var_eq_panel_logols",
  panel_ppml = "tab:log_var_eq_panel_ppml",
  panel_harvey = "tab:log_var_eq_panel_harvey",
  structural = "tab:structural_eq_set_id",
  variance_share = "tab:var_share",
  variance_bound = "tab:variance_bounds_summary"
)

.latex_spec <- function(fragment_id, standalone_id, pdf_id) {
  paste(fragment_id, standalone_id, pdf_id, sep = "|")
}

.latex_publication_specs <- c(
  .latex_spec(
    "heteroskedasticity_table",
    "heteroskedasticity_standalone_tex",
    "heteroskedasticity_standalone_pdf"
  ),
  .latex_spec(
    "log_variance_ppml_table",
    "log_variance_ppml_standalone_tex",
    "log_variance_ppml_standalone_pdf"
  ),
  .latex_spec(
    "log_variance_harvey_table",
    "log_variance_harvey_standalone_tex",
    "log_variance_harvey_standalone_pdf"
  ),
  .latex_spec(
    "log_variance_lad_table",
    "log_variance_lad_standalone_tex",
    "log_variance_lad_standalone_pdf"
  ),
  .latex_spec(
    "log_variance_panels_table",
    "log_variance_panels_standalone_tex",
    "log_variance_panels_standalone_pdf"
  ),
  .latex_spec(
    "log_variance_inference_table",
    "log_variance_inference_standalone_tex",
    "log_variance_inference_standalone_pdf"
  ),
  .latex_spec(
    "structural_equation_inference_table",
    "structural_equation_inference_standalone_tex",
    "structural_equation_inference_standalone_pdf"
  ),
  .latex_spec(
    "structural_var_inference_table",
    "structural_var_inference_standalone_tex",
    "structural_var_inference_standalone_pdf"
  ),
  .latex_spec(
    "variance_share_table",
    "variance_share_standalone_tex",
    "variance_share_standalone_pdf"
  ),
  .latex_spec(
    "variance_bound_summary_table",
    "variance_bound_summary_standalone_tex",
    "variance_bound_summary_standalone_pdf"
  )
)
.latex_publication_specs <- do.call(
  rbind,
  strsplit(.latex_publication_specs, "|", fixed = TRUE)
)
artifact_latex_publications <- data.frame(
  fragment_id = .latex_publication_specs[, 1L],
  standalone_id = .latex_publication_specs[, 2L],
  pdf_id = .latex_publication_specs[, 3L],
  stringsAsFactors = FALSE
)

.latex_label_specs <- c(
  "heteroskedasticity_table|table|heteroskedasticity",
  "log_variance_ppml_table|table|logvar_ppml",
  "log_variance_harvey_table|table|logvar_harvey",
  "log_variance_lad_table|table|panel_lad",
  "log_variance_panels_table|logols|panel_logols",
  "log_variance_panels_table|ppml|panel_ppml",
  "log_variance_panels_table|harvey|panel_harvey",
  "log_variance_inference_table|logols|panel_logols",
  "log_variance_inference_table|ppml|panel_ppml",
  "log_variance_inference_table|harvey|panel_harvey",
  "structural_equation_inference_table|table|structural",
  "structural_var_inference_table|structural|structural",
  "structural_var_inference_table|ppml|panel_ppml",
  "variance_share_table|table|variance_share",
  "variance_bound_summary_table|table|variance_bound"
)
.latex_label_specs <- do.call(
  rbind,
  strsplit(.latex_label_specs, "|", fixed = TRUE)
)
artifact_latex_labels <- data.frame(
  artifact_id = .latex_label_specs[, 1L],
  component = .latex_label_specs[, 2L],
  label_key = .latex_label_specs[, 3L],
  stringsAsFactors = FALSE
)
artifact_latex_labels$latex_label <- unname(
  PAPER_LATEX_LABELS[artifact_latex_labels$label_key]
)

publication_ids <- unlist(
  artifact_latex_publications,
  use.names = FALSE
)
stopifnot(
  !anyDuplicated(artifact_latex_publications$fragment_id),
  !anyDuplicated(publication_ids),
  all(publication_ids %in% artifact_manifest$id),
  all(artifact_latex_labels$artifact_id %in% artifact_manifest$id),
  !anyNA(artifact_latex_labels$latex_label),
  !anyDuplicated(paste(
    artifact_latex_labels$artifact_id,
    artifact_latex_labels$component,
    sep = "/"
  ))
)

artifact_latex_publication <- function(fragment_id) {
  fragment_id <- .artifact_scalar(fragment_id, "LaTeX fragment ID")
  row <- artifact_latex_publications[
    artifact_latex_publications$fragment_id == fragment_id, ,
    drop = FALSE
  ]
  if (nrow(row) != 1L) {
    stop(
      sprintf("Artifact is not a LaTeX publication: %s", fragment_id),
      call. = FALSE
    )
  }
  row
}

artifact_latex_label <- function(fragment_id, component = "table") {
  fragment_id <- .artifact_scalar(fragment_id, "LaTeX fragment ID")
  component <- .artifact_scalar(component, "LaTeX component")
  row <- artifact_latex_labels[
    artifact_latex_labels$artifact_id == fragment_id &
      artifact_latex_labels$component == component, ,
    drop = FALSE
  ]
  if (nrow(row) != 1L) {
    stop(
      sprintf("Unknown LaTeX label: %s/%s", fragment_id, component),
      call. = FALSE
    )
  }
  row$latex_label[[1L]]
}

rm(
  .latex_spec,
  .latex_publication_specs,
  .latex_label_specs,
  publication_ids
)
