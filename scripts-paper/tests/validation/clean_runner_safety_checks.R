# Hostile-path checks for isolated whole-pipeline table acceptance.

local({
  test_root <- tempfile("clean-validation-safety-")
  reference_root <- file.path(test_root, "reference-output")
  dir.create(file.path(reference_root, "tables"), recursive = TRUE)
  on.exit(unlink(test_root, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "\\begin{tabular}{lc}",
      "\\midrule",
      "Estimate & 1.23$^{**}$ \\\\",
      "\\end{tabular}"
    ),
    file.path(reference_root, "tables", "fixture.tex")
  )
  reference <- file.path(test_root, "reference.rds")
  capture <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      paper_path("validation", "capture_table_record.R"),
      reference_root,
      reference
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  stopifnot(identical(clean_runner_status(capture), 0L))

  symlink_root <- file.path(test_root, "symlinked-source-run")
  external_source <- tempfile("clean-validation-external-")
  dir.create(symlink_root, recursive = TRUE)
  dir.create(external_source, recursive = TRUE)
  on.exit(unlink(external_source, recursive = TRUE), add = TRUE)
  external_sentinel <- file.path(external_source, "external-sentinel")
  writeLines("must survive rejected staging", external_sentinel)
  stopifnot(file.symlink(
    external_source,
    file.path(symlink_root, "source")
  ))
  symlink_result <- clean_runner_call(symlink_root, reference)
  symlink_safe <- clean_runner_status(symlink_result) != 0L &&
    file.exists(external_sentinel) &&
    !file.exists(file.path(symlink_root, "pipeline.log"))

  invalid_root <- file.path(test_root, "invalid-reference-run")
  invalid_reference <- file.path(test_root, "invalid-reference.rds")
  dir.create(invalid_root, recursive = TRUE)
  file.create(file.path(invalid_root, "comparison-passed"))
  saveRDS(list(schema_version = 2L), invalid_reference)
  invalid_result <- clean_runner_call(invalid_root, invalid_reference)
  invalid_marker_safe <- clean_runner_status(invalid_result) != 0L &&
    !file.exists(file.path(invalid_root, "comparison-passed"))

  git_root <- file.path(test_root, "preexisting-git-run")
  git_metadata <- file.path(git_root, "source", ".git")
  git_sentinel <- file.path(git_metadata, "preexisting-sentinel")
  dir.create(git_metadata, recursive = TRUE)
  writeLines("must survive rejected staging", git_sentinel)
  git_result <- clean_runner_call(git_root, reference)
  git_safe <- clean_runner_status(git_result) != 0L &&
    file.exists(git_sentinel) &&
    !file.exists(file.path(git_root, "pipeline.log"))

  log_root <- file.path(test_root, "symlinked-log-run")
  external_log <- tempfile("clean-validation-external-log-")
  dir.create(log_root, recursive = TRUE)
  writeLines("external log sentinel", external_log)
  stopifnot(file.symlink(
    external_log,
    file.path(log_root, "pipeline.log")
  ))
  log_result <- clean_runner_call(log_root, reference)
  log_safe <- clean_runner_status(log_result) != 0L &&
    identical(readLines(external_log, warn = FALSE), "external log sentinel") &&
    !file.exists(file.path(
      log_root,
      "source",
      "scripts-paper",
      "output",
      "tables",
      "fixture.tex"
    ))

  safety_results <- c(
    symlinked_source_rejected = symlink_safe,
    invalid_reference_clears_marker = invalid_marker_safe,
    preexisting_git_rejected = git_safe,
    symlinked_log_rejected = log_safe
  )
  if (!all(safety_results)) {
    stop(
      "clean runner safety regressions failed: ",
      paste(names(safety_results)[!safety_results], collapse = ", "),
      call. = FALSE
    )
  }
})

rm(
  clean_runner_status,
  clean_runner_inventory,
  clean_runner_call
)
