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

  unowned_root <- file.path(test_root, "unowned-run")
  unowned_sentinel <- file.path(unowned_root, "source", "arbitrary-file")
  dir.create(dirname(unowned_sentinel), recursive = TRUE)
  writeLines("must survive rejected staging", unowned_sentinel)
  file.create(file.path(unowned_root, "comparison-passed"))
  unowned_result <- clean_runner_call(unowned_root, reference)
  unowned_safe <- clean_runner_status(unowned_result) != 0L &&
    identical(
      readLines(unowned_sentinel, warn = FALSE),
      "must survive rejected staging"
    ) &&
    file.exists(file.path(unowned_root, "comparison-passed")) &&
    !file.exists(file.path(unowned_root, "pipeline.log"))

  containment_root <- tempfile(".clean-validation-overlap-", tmpdir = repo_root)
  containment_source <- tempfile("clean-validation-overlap-source-")
  dir.create(containment_root)
  dir.create(containment_source)
  on.exit(unlink(containment_root, recursive = TRUE), add = TRUE)
  on.exit(unlink(containment_source, recursive = TRUE), add = TRUE)
  stopifnot(file.symlink(
    containment_source,
    file.path(containment_root, "source")
  ))
  containment_result <- clean_runner_call(containment_root, reference)
  containment_safe <- clean_runner_status(containment_result) != 0L &&
    any(grepl("must not overlap the repository", containment_result, fixed = TRUE))
  unlink(containment_root, recursive = TRUE)

  implicit_tmpdir <- tempfile(".clean-validation-tmpdir-", tmpdir = repo_root)
  dir.create(implicit_tmpdir)
  on.exit(unlink(implicit_tmpdir, recursive = TRUE), add = TRUE)
  writeLines("must survive", file.path(implicit_tmpdir, "sentinel"))
  implicit_before <- clean_runner_inventory(implicit_tmpdir)
  implicit_result <- clean_runner_implicit_call(
    implicit_tmpdir,
    file.path(test_root, "missing-implicit-reference.rds")
  )
  implicit_after <- clean_runner_inventory(implicit_tmpdir)
  implicit_safe <- clean_runner_status(implicit_result) != 0L &&
    identical(implicit_before, implicit_after) &&
    any(grepl("must not overlap the repository", implicit_result, fixed = TRUE))
  unlink(implicit_tmpdir, recursive = TRUE)

  symlink_root <- file.path(test_root, "symlinked-source-run")
  external_source <- tempfile("clean-validation-external-")
  clean_runner_mark_owned(symlink_root)
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
  clean_runner_mark_owned(invalid_root)
  file.create(file.path(invalid_root, "comparison-passed"))
  saveRDS(list(schema_version = 2L), invalid_reference)
  invalid_result <- clean_runner_call(invalid_root, invalid_reference)
  invalid_marker_safe <- clean_runner_status(invalid_result) != 0L &&
    !file.exists(file.path(invalid_root, "comparison-passed"))

  missing_root <- file.path(test_root, "missing-reference-run")
  clean_runner_mark_owned(missing_root)
  file.create(file.path(missing_root, "comparison-passed"))
  missing_result <- clean_runner_call(
    missing_root,
    file.path(test_root, "missing-reference.rds")
  )
  missing_marker_safe <- clean_runner_status(missing_result) != 0L &&
    !file.exists(file.path(missing_root, "comparison-passed"))

  git_root <- file.path(test_root, "preexisting-git-run")
  git_metadata <- file.path(git_root, "source", ".git")
  git_sentinel <- file.path(git_metadata, "preexisting-sentinel")
  dir.create(git_metadata, recursive = TRUE)
  clean_runner_mark_owned(git_root)
  writeLines("must survive rejected staging", git_sentinel)
  git_result <- clean_runner_call(git_root, reference)
  git_safe <- clean_runner_status(git_result) != 0L &&
    file.exists(git_sentinel) &&
    !file.exists(file.path(git_root, "pipeline.log"))

  log_root <- file.path(test_root, "symlinked-log-run")
  external_log <- tempfile("clean-validation-external-log-")
  clean_runner_mark_owned(log_root)
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
    unowned_root_rejected = unowned_safe,
    repository_containment_rejected = containment_safe,
    implicit_repository_tmpdir_rejected = implicit_safe,
    symlinked_source_rejected = symlink_safe,
    invalid_reference_clears_owned_marker = invalid_marker_safe,
    missing_reference_clears_owned_marker = missing_marker_safe,
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
  clean_runner_call,
  clean_runner_implicit_call,
  clean_runner_mark_owned
)
