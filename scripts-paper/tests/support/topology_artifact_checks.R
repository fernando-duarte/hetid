# Artifact-schema and output-tree topology checks.

paper_source_once(paper_path("config", "reporting.R"))

required_columns <- c(
  "id",
  "basename",
  "group",
  "old_path",
  "new_path",
  "producer",
  "consumer",
  "status",
  "family",
  "variant"
)
if (!identical(names(artifact_manifest), required_columns)) {
  record_problem(
    "Artifact manifest columns do not match the required schema"
  )
}
if (anyDuplicated(artifact_manifest$id) ||
  anyDuplicated(artifact_manifest$basename)) {
  record_problem("Artifact IDs and basenames must be unique")
}
if (!all(
  basename(artifact_manifest$old_path) ==
    artifact_manifest$basename
) ||
  !all(
    basename(artifact_manifest$new_path) ==
      artifact_manifest$basename
  )) {
  record_problem("Artifact path basenames disagree with the manifest")
}
if (!all(dirname(artifact_manifest$old_path) == out_dir)) {
  record_problem(
    "Old artifact paths must describe the flat output layout"
  )
}
expected_new <- file.path(
  out_dir,
  artifact_manifest$group,
  artifact_manifest$basename
)
if (!identical(artifact_manifest$new_path, expected_new)) {
  record_problem(
    "Artifact paths disagree with manifest-owned groups"
  )
}
variant_rows <- nzchar(artifact_manifest$family)
pairs <- paste(
  artifact_manifest$family[variant_rows],
  artifact_manifest$variant[variant_rows],
  sep = "/"
)
if (!identical(
  variant_rows,
  nzchar(artifact_manifest$variant)
) ||
  anyDuplicated(pairs)) {
  record_problem(
    "Artifact family/variant pairs must be complete and unique"
  )
}

if (dir.exists(out_dir)) {
  output_files <- list.files(
    out_dir,
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE
  )
  output_info <- file.info(file.path(out_dir, output_files))
  output_files <- output_files[
    output_info$isdir %in% FALSE
  ]
  output_files <- output_files[
    basename(output_files) != ".DS_Store"
  ]
  sidecar <- paper_latex_sidecar_pattern()
  if (any(grepl(
    sidecar,
    output_files,
    ignore.case = TRUE
  ))) {
    record_problem("LaTeX sidecars found under output")
  }
  if (any(dirname(output_files) == ".")) {
    record_problem("Root-level output files found")
  }
  expected <- sub(
    paste0("^", out_dir, "/"),
    "",
    artifact_manifest$new_path
  )
  # variance_bounds/quoted_numbers.R is a standalone, non-pipeline script; its
  # csv and self-documenting markdown note are not manifest entries.
  standalone_outputs <- file.path(
    "quoted_numbers",
    c(
      "approximation_error_quoted_numbers.csv",
      "approximation_error_quoted_numbers.md"
    )
  )
  expected <- c(expected, standalone_outputs)
  extra <- setdiff(output_files, expected)
  if (length(extra)) {
    record_problem(
      "Unmanifested output files: %s",
      paste(extra, collapse = ", ")
    )
  }
}
