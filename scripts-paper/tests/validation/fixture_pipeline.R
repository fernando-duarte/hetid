#!/usr/bin/env Rscript

output_root <- file.path("scripts-paper", "output")
if (file.exists(file.path(output_root, "stale-sentinel"))) {
  stop("stale output reached fixture pipeline", call. = FALSE)
}
if (identical(Sys.getenv("HETID_FIXTURE_PIPELINE_FAIL"), "1")) {
  stop("requested fixture failure", call. = FALSE)
}
dir.create(file.path(output_root, "tables"), recursive = TRUE)
writeLines(
  c(
    "\\begin{tabular}{lc}",
    "\\midrule",
    "Estimate & 1.23$^{**}$ \\\\",
    "\\end{tabular}"
  ),
  file.path(output_root, "tables", "fixture.tex")
)
