# LaTeX publication, table, and console helper checks.

check(
  "LaTeX publication and label relationships are manifest-owned",
  nrow(artifact_latex_publications) == 8L &&
    identical(
      artifact_latex_label("structural_var_inference_table", "structural"),
      "tab:structural_eq_set_id"
    ) &&
    identical(
      artifact_latex_label("heteroskedasticity_w2_table", "table"),
      "tab:sdf_news_hetero_tests_w2"
    ) &&
    identical(
      artifact_latex_publication(
        "structural_var_estimators_table"
      )$pdf_id,
      "structural_var_estimators_standalone_pdf"
    )
)
local({
  original_manifest <- artifact_manifest
  publication <- artifact_latex_publication(
    "variance_bound_summary_table"
  )
  root <- tempfile("paper-latex-publication-")
  dir.create(root)
  on.exit(
    {
      artifact_manifest <<- original_manifest
      unlink(root, recursive = TRUE)
    },
    add = TRUE
  )
  ids <- unname(unlist(publication[1L, ], use.names = FALSE))
  rows <- match(ids, artifact_manifest$id)
  stopifnot(!anyNA(rows))
  test_manifest <- artifact_manifest
  test_manifest$new_path[rows] <- file.path(
    root,
    test_manifest$basename[rows]
  )
  artifact_manifest <<- test_manifest
  publish_latex_artifact(
    "variance_bound_summary_table",
    "fixture fragment",
    compiler = function(tex_path) {
      stopifnot(file.exists(tex_path))
      writeLines(
        "%PDF-1.4 fixture",
        artifact_path(publication$pdf_id)
      )
    }
  )
  published_paths <- vapply(ids, artifact_path, character(1))
  check(
    "LaTeX publisher writes the exact registered artifact triple",
    all(file.exists(published_paths)) &&
      identical(
        readLines(published_paths[[1L]], warn = FALSE),
        "fixture fragment"
      )
  )
})
table_lines <- latex_table_environment(
  tabular_lines = c("\\begin{tabular}{c}", "x", "\\end{tabular}"),
  caption = "Fixture",
  label = "tab:fixture",
  notes = c("first", "second"),
  notes_label = "\\textit{Notes:}"
)
check(
  "table environment helper owns one notes wrapper",
  sum(table_lines == "\\begin{table}[!htbp]") == 1L &&
    sum(table_lines == "\\begin{tablenotes}[flushleft]") == 1L &&
    any(grepl("first second", table_lines, fixed = TRUE))
)
hull <- logvar_hull_text(data.frame(
  set_lower = c(-1, NA_real_),
  set_upper = c(2, NA_real_),
  status = c("bounded", "unreliable"),
  stringsAsFactors = FALSE
))
check(
  "console hull helper formats bounded and status rows",
  identical(hull, c("[-1,2]", "unreliable"))
)
console_fixture <- list(
  sample = list(
    n = 2L,
    span = as.Date(c("2000-01-01", "2000-04-01"))
  ),
  sets = list(tau = data.frame(
    set_lower = -1,
    set_upper = 2,
    status = "bounded"
  )),
  counts = list(tau = list(
    n_attempted = 4L,
    n_evaluated = 3L,
    n_cached = 1L,
    n_failed = 0L
  ))
)
console_lines <- capture.output(logvar_print_map_summary(
  "Fixture map",
  console_fixture,
  0.1,
  census = 7L,
  census_label = "fixture census"
))
check(
  "console summary owns map, budget, and optional census reporting",
  any(grepl("Fixture map: N = 2", console_lines, fixed = TRUE)) &&
    any(grepl(
      "attempted 4 evaluated 3 cached 1 failed 0",
      console_lines
    )) &&
    any(grepl(
      "fixture census: 0.1=7",
      console_lines,
      fixed = TRUE
    ))
)
rm(console_fixture, console_lines)

# The flag is what stops Dropbox putting a deleted sidecar back, so a silent
# no-op here would restore the reappearance this exists to prevent. Read the
# attribute back rather than trusting the write. Off macOS there is nothing to
# assert but the empty return.
local({
  flag_dir <- tempfile("paper-fileprovider-")
  dir.create(flag_dir)
  on.exit(unlink(flag_dir, recursive = TRUE), add = TRUE)
  present <- file.path(flag_dir, "fixture.aux")
  file.create(present)
  absent <- file.path(flag_dir, "never-written.aux")
  flagged <- paper_flag_fileprovider_ignored(c(present, absent))
  darwin <- identical(unname(Sys.info()[["sysname"]]), "Darwin")
  check(
    "a missing path is skipped rather than flagged",
    !(absent %in% flagged)
  )
  if (darwin && nzchar(Sys.which("xattr"))) {
    read_back <- system2(
      "xattr",
      c("-p", shQuote(PAPER_FILEPROVIDER_IGNORE_ATTR), shQuote(present)),
      stdout = TRUE,
      stderr = FALSE
    )
    check(
      "an existing sidecar reads back as file-provider ignored",
      identical(flagged, present) && identical(trimws(read_back), "1")
    )
  } else {
    check(
      "off macOS the flag is a no-op",
      identical(flagged, character(0))
    )
  }
})
