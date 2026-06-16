# Fail-closed invariant for scripts/output/for_paper: it holds EXACTLY the three
# paper-spec tables, each as <stem>.tex / <stem>_standalone.tex /
# <stem>_standalone.pdf / <stem>.csv (12 files, no subdirs, no aux). Used both to
# validate the staging dir before publishing and to re-check for_paper after a run.

# The 12 exact basenames the allowlist permits.
for_paper_allowlist <- function(stems = FOR_PAPER_TABLE_STEMS) {
  as.vector(outer(
    stems,
    c(".tex", "_standalone.tex", "_standalone.pdf", ".csv"),
    paste0
  ))
}

assert_for_paper_allowlist <- function(paper_dir = OUTPUT_PAPER_DIR,
                                       allow = for_paper_allowlist()) {
  # all.files = TRUE catches hidden files; also flag MISSING files and subdirs
  # (a permissive setdiff alone would pass when a table is absent).
  files <- list.files(paper_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  subdirs <- setdiff(list.dirs(paper_dir, recursive = TRUE, full.names = FALSE), "")
  bad <- setdiff(files, allow)
  missing <- setdiff(allow, files)
  if (length(bad) || length(missing) || length(subdirs)) {
    stop(
      "for_paper invariant violated.",
      if (length(bad)) paste0("\n  unexpected: ", paste(bad, collapse = ", ")),
      if (length(missing)) paste0("\n  missing: ", paste(missing, collapse = ", ")),
      if (length(subdirs)) paste0("\n  subdirs: ", paste(subdirs, collapse = ", ")),
      "\nRedirect stray writers to OUTPUT_TEMP_DIR; compile LaTeX aux in a temp dir.",
      call. = FALSE
    )
  }
  invisible(files)
}
