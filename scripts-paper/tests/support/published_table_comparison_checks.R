# Cross-platform acceptance checks for rendered table numbers.

ptc_write <- function(root, relative, body) {
  path <- file.path(root, "tables", relative)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "\\begin{tabular}{lcc}",
      "\\toprule",
      " & First & Second \\\\",
      "\\midrule",
      body,
      "\\bottomrule",
      "\\end{tabular}"
    ),
    path
  )
}

ptc_compare <- function(reference_root, candidate_root) {
  paper_published_tables_compare(
    paper_published_tables_projection(reference_root),
    paper_published_tables_projection(candidate_root)
  )
}

ptc_reference <- tempfile("paper-table-reference-")
ptc_candidate <- tempfile("paper-table-candidate-")
dir.create(ptc_reference)
dir.create(ptc_candidate)

ptc_write(
  ptc_reference,
  "table.tex",
  c(
    "estimate & 1.234 & 4.56 \\\\",
    "status & 8.88 & unreliable \\\\"
  )
)
ptc_write(
  ptc_candidate,
  "table.tex",
  c(
    "renamed & 1.23 & 4.560 \\\\",
    "changed & unavailable & -- \\\\"
  )
)
stopifnot(isTRUE(ptc_compare(ptc_reference, ptc_candidate)))

ptc_write(
  ptc_candidate,
  "table.tex",
  c(
    "renamed & 1.24 & 4.560 \\\\",
    "changed & unavailable & -- \\\\"
  )
)
stopifnot(!isTRUE(ptc_compare(ptc_reference, ptc_candidate)))

ptc_write(ptc_reference, "table.tex", "estimate & 1.23 & 4.56 \\\\")
ptc_write(ptc_candidate, "table.tex", "estimate & 1.24 & 4.56 \\\\")
stopifnot(!isTRUE(ptc_compare(ptc_reference, ptc_candidate)))

ptc_write(
  ptc_reference,
  "table.tex",
  "estimate & $[1.23,\\,2.34]$ & 4.56 \\\\"
)
ptc_write(
  ptc_candidate,
  "table.tex",
  "estimate & 1.23 & 4.56 \\\\"
)
stopifnot(isTRUE(ptc_compare(ptc_reference, ptc_candidate)))

ptc_write(ptc_reference, "extra.tex", "estimate & 1.23 & 4.56 \\\\")
stopifnot(!isTRUE(ptc_compare(ptc_reference, ptc_candidate)))

ptc_output_root <- file.path("scripts-paper", "output")
ptc_output_paths <- list.files(
  file.path(ptc_output_root, "tables"),
  pattern = "[.]tex$",
  recursive = TRUE
)
ptc_output_projection <- paper_published_tables_projection(
  ptc_output_root
)
stopifnot(identical(
  sort(names(ptc_output_projection)),
  sort(ptc_output_paths)
))

unlink(ptc_reference, recursive = TRUE)
unlink(ptc_candidate, recursive = TRUE)
rm(
  ptc_write,
  ptc_compare,
  ptc_reference,
  ptc_candidate,
  ptc_output_root,
  ptc_output_paths,
  ptc_output_projection
)
