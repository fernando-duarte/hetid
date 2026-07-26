# Subprocess checks for isolated whole-pipeline table acceptance.

clean_runner_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) {
    return(0L)
  }
  as.integer(status)
}

clean_runner_inventory <- function(root) {
  paths <- list.files(
    root,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  paths <- sort(paths)
  hashes <- if (length(paths)) {
    unname(tools::md5sum(file.path(root, paths)))
  } else {
    character()
  }
  list(paths = paths, hashes = hashes)
}

clean_runner_call <- function(run_root, reference, fail = FALSE) {
  environment <- c(
    paste0("HETID_VALIDATION_RUN_ROOT=", run_root),
    paste0(
      "HETID_VALIDATION_PIPELINE_SCRIPT=",
      "scripts-paper/tests/validation/fixture_pipeline.R"
    ),
    paste0("HETID_FIXTURE_PIPELINE_FAIL=", if (fail) "1" else "0")
  )
  suppressWarnings(system2(
    "bash",
    c(
      paper_path("validation", "run_clean_validation.sh"),
      reference
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = environment
  ))
}

local({
  test_root <- tempfile("clean-validation-")
  reference_root <- file.path(test_root, "reference-output")
  success_root <- file.path(test_root, "successful-run")
  failure_root <- file.path(test_root, "failed-run")
  dir.create(file.path(reference_root, "tables"), recursive = TRUE)
  dir.create(
    file.path(success_root, "source", "scripts-paper", "output"),
    recursive = TRUE
  )
  on.exit(unlink(test_root, recursive = TRUE), add = TRUE)

  fixture_lines <- c(
    "\\begin{tabular}{lc}",
    "\\midrule",
    "Estimate & 1.23$^{**}$ \\\\",
    "\\end{tabular}"
  )
  writeLines(
    fixture_lines,
    file.path(reference_root, "tables", "fixture.tex")
  )
  writeLines(
    "must be preserved outside staged output",
    file.path(
      success_root,
      "source",
      "scripts-paper",
      "output",
      "stale-sentinel"
    )
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

  original_output <- file.path(repo_root, "scripts-paper", "output")
  original_before <- clean_runner_inventory(original_output)
  success <- clean_runner_call(success_root, reference)
  original_after <- clean_runner_inventory(original_output)

  stopifnot(
    identical(clean_runner_status(success), 0L),
    file.exists(file.path(
      success_root,
      "preexisting-output",
      "stale-sentinel"
    )),
    !file.exists(file.path(
      success_root,
      "source",
      "scripts-paper",
      "output",
      "stale-sentinel"
    )),
    file.exists(file.path(success_root, "candidate.rds")),
    file.exists(file.path(success_root, "comparison-passed")),
    identical(original_before, original_after)
  )

  comparison <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      paper_path("validation", "compare_table_records.R"),
      reference,
      file.path(success_root, "candidate.rds")
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  stopifnot(identical(clean_runner_status(comparison), 0L))

  pipeline_log <- readLines(
    file.path(success_root, "pipeline.log"),
    warn = FALSE
  )
  stopifnot(any(grepl(
    "scripts-paper/tests/validation/fixture_pipeline.R",
    pipeline_log,
    fixed = TRUE
  )))

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

  safety_results <- c(
    symlinked_source_rejected = symlink_safe,
    invalid_reference_clears_marker = invalid_marker_safe,
    preexisting_git_rejected = git_safe
  )
  if (!all(safety_results)) {
    stop(
      "clean runner safety regressions failed: ",
      paste(names(safety_results)[!safety_results], collapse = ", "),
      call. = FALSE
    )
  }

  dir.create(failure_root, recursive = TRUE)
  file.create(file.path(failure_root, "comparison-passed"))
  failure <- clean_runner_call(failure_root, reference, fail = TRUE)
  stopifnot(
    clean_runner_status(failure) != 0L,
    !file.exists(file.path(failure_root, "comparison-passed")),
    any(grepl("requested fixture failure", failure, fixed = TRUE))
  )
})

rm(
  clean_runner_status,
  clean_runner_inventory,
  clean_runner_call
)
