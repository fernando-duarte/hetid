# Stage 08 renderer: turn the paper-spec results bundle into the THREE for_paper
# tables (summary statistics; the structural price-of-risk equation; estimator
# properties). Each ships as <stem>.tex / <stem>_standalone.tex /
# <stem>_standalone.pdf / <stem>.csv. Builds + validates in a temp STAGING dir
# (PDFs compiled in a separate temp -outdir so no aux ever lands in for_paper),
# then atomically replaces scripts/output/for_paper. The exact-filename allowlist
# guard (for_paper_guard.R) enforces the "only three tables" invariant.
source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/08_paper_spec/paper_spec_tables.R"))

results_path <- file.path(OUTPUT_TEMP_DIR, "paper_spec", "paper_spec_results.rds")
if (!file.exists(results_path)) {
  cli_abort("Missing {.path {results_path}}; run scripts/08_paper_spec/compute_paper_spec.R first.")
}
res <- readRDS(results_path)

if (!nzchar(Sys.which("latexmk"))) {
  cli_abort("latexmk not found, but the for_paper PDFs are part of the 3-table invariant.")
}

staging <- file.path(OUTPUT_TEMP_DIR, "paper_spec", "for_paper_staging")
unlink(staging, recursive = TRUE)
dir.create(staging, recursive = TRUE, showWarnings = FALSE)

# Compile a standalone .tex to PDF in a throwaway -outdir, copying back ONLY the
# .pdf so the aux files (.aux/.log/.fls/.fdb_latexmk) never reach the output dir.
compile_pdf <- function(stem) {
  tex <- file.path(staging, paste0(stem, "_standalone.tex"))
  build <- file.path(tempdir(), paste0("texbuild_", stem))
  unlink(build, recursive = TRUE)
  dir.create(build, recursive = TRUE, showWarnings = FALSE)
  status <- system2(
    "latexmk",
    c(
      "-pdf", "-halt-on-error", "-interaction=nonstopmode",
      paste0("-outdir=", build), tex
    ),
    stdout = FALSE, stderr = FALSE
  )
  pdf_src <- file.path(build, paste0(stem, "_standalone.pdf"))
  if (status != 0L || !file.exists(pdf_src)) {
    cli_abort("latexmk failed to produce a PDF for {.val {stem}}.")
  }
  file.copy(pdf_src, file.path(staging, basename(pdf_src)), overwrite = TRUE)
}

emit <- function(tbl, stem) {
  write_latex_table(tbl$lines, staging, stem, landscape = isTRUE(tbl$landscape))
  write.csv(tbl$csv, file.path(staging, paste0(stem, ".csv")), row.names = FALSE)
  compile_pdf(stem)
  cli_alert_success("Built {.val {stem}}")
}

cli_h1("Stage 08: rendering the three for_paper tables")
emit(build_table1_summary(res), "table1_summary_statistics")
emit(build_table2_structural(res), "table2_structural_equation")
emit(build_table3_properties(res), "table3_estimator_properties")

# Validate the staging dir holds EXACTLY the 12 allowlisted files, then
# atomically replace for_paper (so a mid-pipeline failure never empties it).
assert_for_paper_allowlist(staging)
unlink(OUTPUT_PAPER_DIR, recursive = TRUE)
dir.create(OUTPUT_PAPER_DIR, recursive = TRUE, showWarnings = FALSE)
ok <- file.copy(list.files(staging, full.names = TRUE), OUTPUT_PAPER_DIR, overwrite = TRUE)
if (!all(ok)) {
  cli_abort("Failed to copy some staged tables into for_paper.")
}
assert_for_paper_allowlist(OUTPUT_PAPER_DIR)

cli_alert_success(
  "for_paper now holds exactly the 3 paper-spec tables ({length(list.files(OUTPUT_PAPER_DIR))} files)."
)
