# Artifact writers for the spec-comparison report: gt tables (html + tex),
# figures (png + svg), and the booktabs publication panel (+ standalone PDF
# compiled with latexmk when available). Pure consumers of the classified grid
# and derived strings; the coverage suffix in file names is set by the caller.
# Requires common_settings.R, the visualization/web packages, and
# spec_comparison_report_utils.R.

SPEC_OUTCOME_FILL <- c(
  "point" = "#2166AC",
  "point failed" = "#4D4D4D",
  "certified bounded" = "#1B7837",
  "certified unbounded" = "#B2182B",
  "no certified bound" = "#E08214"
)

SPEC_WIDTH_LEGEND <- paste(
  "Width states: 0 = zero-slack point solution; finite = solver-certified",
  "finite bound (summed per-component profile-bound widths); unbounded =",
  "solver-certified unbounded set; no certified bound = fail-closed solver",
  "outcome (evidence of neither)."
)
SPEC_DENOM_NOTE <- paste(
  "Counts are over applicable observed cells: optimized and per-PC schemes",
  "exist only at tau > 0; VFCI weights only at n_pcs = 4. '--' = not",
  "applicable. Optimized weights are a width-minimizing computational",
  "benchmark, not an economically structural weighting."
)

# Text-mode LaTeX needs "_" escaped and ">" set in math mode (OT1 text-mode
# ">" prints as an inverted question mark).
SPEC_COMPACT_CELL_NOTE <- paste(
  "Cells report certified-bounded counts k/n over the applicable observed",
  "cells at each tau; parentheses split the remainder into unb. (certified",
  "unbounded) and no cert. (no certified bound)."
)

escape_latex_text <- function(x) gsub(">", "$>$", gsub("_", "\\\\_", x))

# Outcome data frames (shared tau columns) feeding the gt table and
# the LaTeX panel, grouped under a single "Bond maturities" stratum.
spec_outcome_by_stratum <- function(grid, compact = FALSE,
                                    scheme_labels = SPEC_SCHEME_LABELS) {
  taus <- sort(unique(grid$tau))
  list(
    "Bond maturities" = spec_outcome_matrix(
      grid,
      taus = taus, compact = compact, scheme_labels = scheme_labels
    )
  )
}

write_spec_outcome_table <- function(grid, cov, bl, paper_dir, suffix) {
  by_stratum <- spec_outcome_by_stratum(grid)
  df <- do.call(rbind, lapply(names(by_stratum), function(nm) {
    cbind(Stratum = nm, by_stratum[[nm]])
  }))
  tbl <- gt(df, groupname_col = "Stratum") |>
    tab_header(title = bl$short, subtitle = cov$line) |>
    tab_source_note(SPEC_WIDTH_LEGEND) |>
    tab_source_note(SPEC_DENOM_NOTE)
  base <- file.path(paper_dir, paste0("spec_comparison_outcome_table", suffix))
  gtsave(tbl, paste0(base, ".html"))
  writeLines(as.character(as_latex(tbl)), paste0(base, ".tex"))
}

write_spec_benchmark_table <- function(grid, cov, paper_dir, suffix) {
  bench <- grid[grid$outcome %in% c("point", "point failed"), ]
  if (!nrow(bench)) {
    return(invisible(NULL))
  }
  bench <- bench[order(-bench$cond), ]
  df <- data.frame(
    Specification = spec_stratum_label(bench$components),
    `PC instruments` = bench$n_pcs,
    Weighting = unname(SPEC_SCHEME_LABELS[bench$gamma]),
    `Condition number` = ifelse(
      is.finite(bench$cond),
      formatC(bench$cond, format = "e", digits = 1), "point solve failed"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  title <- sprintf(
    "Zero-slack benchmark (tau = 0): point identification in %d/%d applicable specs",
    sum(bench$outcome == "point"), nrow(bench)
  )
  tbl <- gt(df) |>
    tab_header(title = title, subtitle = cov$line) |>
    tab_source_note(paste(
      "Width is 0 by construction at tau = 0. The condition number of the",
      "linear point solve is a numerical-sensitivity diagnostic, not sampling",
      "uncertainty; rows are sorted from most to least numerically sensitive.",
      "High values motivate the tau > 0 set analysis."
    ))
  base <- file.path(paper_dir, paste0("spec_comparison_benchmark_table", suffix))
  gtsave(tbl, paste0(base, ".html"))
  writeLines(as.character(as_latex(tbl)), paste0(base, ".tex"))
}

compile_standalone_pdf <- function(tex_path) {
  if (!nzchar(Sys.which("latexmk"))) {
    cli_alert_warning("latexmk not found; PDF not built for {.path {tex_path}}")
    return(invisible(NULL))
  }
  old <- setwd(dirname(tex_path))
  on.exit(setwd(old), add = TRUE)
  status <- system2(
    "latexmk", c("-pdf", "-interaction=nonstopmode", "-silent", basename(tex_path)),
    stdout = FALSE, stderr = FALSE
  )
  if (status != 0) cli_alert_warning("latexmk failed for {.path {tex_path}}")
}

write_spec_panel <- function(grid, cov, bl, paper_dir, suffix) {
  panels <- spec_outcome_by_stratum(
    grid,
    compact = TRUE, scheme_labels = SPEC_SCHEME_LABELS_SHORT
  )
  taus <- sort(unique(grid$tau))
  lines <- build_panel_latex_table(
    panels = panels,
    col_headers = format(taus, trim = TRUE),
    caption = escape_latex_text(paste0(bl$short, ". Coverage: ", cov$label, ".")),
    label = "tab:spec_comparison_outcomes",
    notes = escape_latex_text(c(
      SPEC_COMPACT_CELL_NOTE, SPEC_WIDTH_LEGEND, SPEC_DENOM_NOTE,
      paste0(cov$line, "; ", cov$missing_line, ".")
    )),
    col_group_label = "Slack $\\tau$",
    table_format = "1.2"
  )
  # Five tau columns with per-cell breakdowns exceed the text width at the
  # body font; a font-size switch scales the tabular without the
  # siunitx-inside-resizebox grouping breakage.
  tab_start <- grep("^\\\\begin\\{tabular\\}", lines)[1]
  lines <- c(
    lines[seq_len(tab_start - 1)],
    "\\scriptsize",
    lines[-seq_len(tab_start - 1)]
  )
  paths <- write_latex_table(lines, paper_dir, paste0("spec_comparison_panel", suffix))
  compile_standalone_pdf(paths[2])
}
