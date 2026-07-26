# Subprocess checks for historical compatibility wrappers.

local({
  compatibility_status <- function(output) {
    status <- attr(output, "status")
    if (is.null(status)) 0L else as.integer(status)
  }

  compatibility_root <- tempfile("compatibility-wrappers-")
  reference_root <- file.path(compatibility_root, "reference")
  comparison_cwd <- file.path(compatibility_root, "caller")
  run_root <- file.path(compatibility_root, "run")
  dir.create(file.path(reference_root, "tables"), recursive = TRUE)
  dir.create(comparison_cwd)
  on.exit(unlink(compatibility_root, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "\\begin{tabular}{lc}",
      "\\midrule",
      "Estimate & 1.23$^{**}$ \\\\",
      "\\end{tabular}"
    ),
    file.path(reference_root, "tables", "fixture.tex")
  )
  reference_record <- file.path(compatibility_root, "reference.rds")
  capture <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      paper_path("validation", "capture_table_record.R"),
      reference_root,
      reference_record
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  stopifnot(identical(compatibility_status(capture), 0L))

  comparator <- repo_path(
    "docs",
    "bootstrap-single-stage-refactor",
    "validation-tools",
    "compare_scientific_objects.R"
  )
  original_cwd <- setwd(comparison_cwd)
  comparison <- tryCatch(
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", comparator, reference_record, reference_record),
      stdout = TRUE,
      stderr = TRUE
    ),
    finally = setwd(original_cwd)
  )
  stopifnot(
    identical(compatibility_status(comparison), 0L),
    any(grepl(
      "schema-3 published table-result comparison passed",
      comparison,
      fixed = TRUE
    ))
  )

  mac_wrapper <- repo_path(
    "docs",
    "bootstrap-single-stage-refactor",
    "validation-tools",
    "run_mac_candidate.sh"
  )
  missing_reference <- system2("bash", mac_wrapper)
  stopifnot(missing_reference != 0L)
  wrapper <- system2(
    "bash",
    c(mac_wrapper, reference_record),
    env = c(
      paste0("HETID_VALIDATION_RUN_ROOT=", run_root),
      paste0(
        "HETID_VALIDATION_PIPELINE_SCRIPT=",
        "scripts-paper/tests/validation/fixture_pipeline.R"
      )
    )
  )
  stopifnot(
    identical(wrapper, 0L),
    file.exists(file.path(run_root, "comparison-passed"))
  )

  unlink(compatibility_root, recursive = TRUE)
})
