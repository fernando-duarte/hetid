# Manifest-directed fragment, standalone-source, and PDF publication.

publish_latex_artifact <- function(
  fragment_id,
  table_lines,
  landscape = FALSE,
  compiler = compile_latex_pdf
) {
  publication <- artifact_latex_publication(fragment_id)
  ids <- unname(unlist(publication[1L, ], use.names = FALSE))
  paths <- vapply(ids, artifact_path, character(1))
  names(paths) <- names(publication)
  if (length(unique(dirname(paths))) != 1L) {
    stop(
      sprintf("LaTeX publication paths diverge for %s", fragment_id),
      call. = FALSE
    )
  }
  dir.create(
    dirname(paths[["fragment_id"]]),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(table_lines, paths[["fragment_id"]])
  writeLines(
    make_standalone_latex(table_lines, landscape = landscape),
    paths[["standalone_id"]]
  )
  compiler(paths[["standalone_id"]])
  if (!file.exists(paths[["pdf_id"]])) {
    stop(
      sprintf("LaTeX publication PDF is missing for %s", fragment_id),
      call. = FALSE
    )
  }
  invisible(paths)
}
