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

  reference_results <- c(
    candidate_reference_is_immutable = candidate_safe,
    source_reference_is_immutable = source_safe,
    pipeline_log_reference_is_immutable = log_safe,
    staged_output_reference_is_immutable = output_safe
  )
  if (!all(reference_results)) {
    stop(
      "clean runner reference regressions failed: ",
      paste(names(reference_results)[!reference_results], collapse = ", "),
      call. = FALSE
    )
  }
})
