# Reference isolation checks for whole-pipeline table acceptance.

local({
  test_root <- tempfile("clean-validation-reference-")
  matching_output <- file.path(test_root, "matching-output")
  mismatching_output <- file.path(test_root, "mismatching-output")
  dir.create(file.path(matching_output, "tables"), recursive = TRUE)
  dir.create(file.path(mismatching_output, "tables"), recursive = TRUE)
  on.exit(unlink(test_root, recursive = TRUE), add = TRUE)

  fixture_table <- c(
    "\\begin{tabular}{lc}",
    "\\midrule",
    "Estimate & 1.23$^{**}$ \\\\",
    "\\end{tabular}"
  )
  mismatching_table <- sub("1.23", "9.99", fixture_table, fixed = TRUE)
  writeLines(
    fixture_table,
    file.path(matching_output, "tables", "fixture.tex")
  )
  writeLines(
    mismatching_table,
    file.path(mismatching_output, "tables", "fixture.tex")
  )

  matching_reference <- file.path(test_root, "matching-reference.rds")
  mismatching_reference <- file.path(test_root, "mismatching-reference.rds")
  for (capture_spec in list(
    c(matching_output, matching_reference),
    c(mismatching_output, mismatching_reference)
  )) {
    capture <- system2(
      file.path(R.home("bin"), "Rscript"),
      c(
        "--vanilla",
        paper_path("validation", "capture_table_record.R"),
        capture_spec
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    stopifnot(identical(clean_runner_status(capture), 0L))
  }

  candidate_root <- file.path(test_root, "candidate-reference-run")
  seed_candidate <- clean_runner_call(candidate_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_candidate), 0L))
  candidate_reference <- file.path(candidate_root, "candidate.rds")
  file.copy(mismatching_reference, candidate_reference, overwrite = TRUE)
  candidate_hash <- unname(tools::md5sum(candidate_reference))
  candidate_result <- clean_runner_call(candidate_root, candidate_reference)
  candidate_safe <- clean_runner_status(candidate_result) != 0L &&
    identical(unname(tools::md5sum(candidate_reference)), candidate_hash) &&
    any(grepl("displayed values differ", candidate_result, fixed = TRUE))

  source_root <- file.path(test_root, "source-reference-run")
  seed_source <- clean_runner_call(source_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_source), 0L))
  source_reference <- file.path(source_root, "source", "reference.rds")
  file.copy(mismatching_reference, source_reference, overwrite = TRUE)
  source_hash <- unname(tools::md5sum(source_reference))
  source_result <- clean_runner_call(source_root, source_reference)
  source_safe <- clean_runner_status(source_result) != 0L &&
    file.exists(source_reference) &&
    identical(unname(tools::md5sum(source_reference)), source_hash) &&
    any(grepl("displayed values differ", source_result, fixed = TRUE))

  log_root <- file.path(test_root, "log-reference-run")
  seed_log <- clean_runner_call(log_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_log), 0L))
  log_reference <- file.path(log_root, "pipeline.log")
  file.copy(mismatching_reference, log_reference, overwrite = TRUE)
  log_hash <- unname(tools::md5sum(log_reference))
  log_result <- clean_runner_call(log_root, log_reference)
  log_safe <- clean_runner_status(log_result) != 0L &&
    file.exists(log_reference) &&
    identical(unname(tools::md5sum(log_reference)), log_hash) &&
    !file.exists(file.path(log_root, "comparison-passed"))

  symlink_root <- file.path(test_root, "symlink-log-reference-run")
  seed_symlink <- clean_runner_call(symlink_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_symlink), 0L))
  symlink_target <- file.path(symlink_root, "pipeline.log")
  symlink_reference <- file.path(symlink_root, "log-reference.rds")
  file.copy(mismatching_reference, symlink_target, overwrite = TRUE)
  stopifnot(file.symlink(symlink_target, symlink_reference))
  symlink_hash <- unname(tools::md5sum(symlink_reference))
  symlink_result <- clean_runner_call(symlink_root, symlink_reference)
  symlink_safe <- clean_runner_status(symlink_result) != 0L &&
    identical(unname(tools::md5sum(symlink_reference)), symlink_hash) &&
    !file.exists(file.path(symlink_root, "comparison-passed"))

  hardlink_root <- file.path(test_root, "hardlink-log-reference-run")
  seed_hardlink <- clean_runner_call(hardlink_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_hardlink), 0L))
  hardlink_target <- file.path(hardlink_root, "pipeline.log")
  hardlink_reference <- file.path(hardlink_root, "log-reference.rds")
  file.copy(mismatching_reference, hardlink_target, overwrite = TRUE)
  stopifnot(file.link(hardlink_target, hardlink_reference))
  hardlink_hash <- unname(tools::md5sum(hardlink_reference))
  hardlink_result <- clean_runner_call(hardlink_root, hardlink_reference)
  hardlink_safe <- clean_runner_status(hardlink_result) != 0L &&
    identical(unname(tools::md5sum(hardlink_reference)), hardlink_hash) &&
    !file.exists(file.path(hardlink_root, "comparison-passed"))

  marker_root <- file.path(test_root, "marker-reference-run")
  seed_marker <- clean_runner_call(marker_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_marker), 0L))
  marker_target <- file.path(marker_root, "comparison-passed")
  marker_reference <- file.path(marker_root, "marker-reference.rds")
  file.copy(mismatching_reference, marker_target, overwrite = TRUE)
  stopifnot(file.link(marker_target, marker_reference))
  marker_hash <- unname(tools::md5sum(marker_reference))
  marker_result <- clean_runner_call(marker_root, marker_reference)
  marker_safe <- clean_runner_status(marker_result) != 0L &&
    identical(unname(tools::md5sum(marker_target)), marker_hash) &&
    identical(unname(tools::md5sum(marker_reference)), marker_hash)

  candidate_alias_root <- file.path(test_root, "candidate-alias-run")
  seed_candidate_alias <- clean_runner_call(
    candidate_alias_root,
    matching_reference
  )
  stopifnot(identical(clean_runner_status(seed_candidate_alias), 0L))
  candidate_alias_target <- file.path(candidate_alias_root, "candidate.rds")
  candidate_alias_reference <- file.path(
    candidate_alias_root,
    "candidate-reference.rds"
  )
  file.copy(
    mismatching_reference,
    candidate_alias_target,
    overwrite = TRUE
  )
  stopifnot(file.link(candidate_alias_target, candidate_alias_reference))
  candidate_alias_hash <- unname(tools::md5sum(candidate_alias_reference))
  candidate_alias_result <- clean_runner_call(
    candidate_alias_root,
    candidate_alias_reference
  )
  candidate_alias_safe <- clean_runner_status(candidate_alias_result) != 0L &&
    identical(
      unname(tools::md5sum(candidate_alias_reference)),
      candidate_alias_hash
    ) &&
    identical(
      unname(tools::md5sum(candidate_alias_target)),
      candidate_alias_hash
    )

  output_root <- file.path(test_root, "output-reference-run")
  seed_output <- clean_runner_call(output_root, matching_reference)
  stopifnot(identical(clean_runner_status(seed_output), 0L))
  output_reference <- file.path(
    output_root,
    "source",
    "scripts-paper",
    "output",
    "reference.rds"
  )
  file.copy(mismatching_reference, output_reference, overwrite = TRUE)
  output_hash <- unname(tools::md5sum(output_reference))
  output_result <- clean_runner_call(output_root, output_reference)
  output_safe <- clean_runner_status(output_result) != 0L &&
    file.exists(output_reference) &&
    identical(unname(tools::md5sum(output_reference)), output_hash) &&
    !file.exists(file.path(output_root, "comparison-passed"))
  case_alias <- sub("/source/", "/SOURCE/", output_reference, fixed = TRUE)
  case_alias_safe <- TRUE
  if (file.exists(case_alias)) {
    case_result <- clean_runner_call(output_root, case_alias)
    case_alias_safe <- clean_runner_status(case_result) != 0L &&
      identical(unname(tools::md5sum(output_reference)), output_hash)
  }

  reference_results <- c(
    candidate_reference_is_immutable = candidate_safe,
    source_reference_is_immutable = source_safe,
    pipeline_log_reference_is_immutable = log_safe,
    symlink_log_reference_is_immutable = symlink_safe,
    hardlink_log_reference_is_immutable = hardlink_safe,
    comparison_marker_alias_is_immutable = marker_safe,
    candidate_alias_redirected = candidate_alias_safe,
    staged_output_reference_is_immutable = output_safe,
    staged_output_case_alias_rejected = case_alias_safe
  )
  if (!all(reference_results)) {
    stop(
      "clean runner reference regressions failed: ",
      paste(names(reference_results)[!reference_results], collapse = ", "),
      call. = FALSE
    )
  }
})
