# Helpers for the spec-comparison report: human labels, outcome classification,
# data-derived coverage, and applicable-denominator outcome counts. The
# aggregation and headline strings live in spec_comparison_report_stats.R.
# No artifact writing happens here.
# Requires common_settings.R and spec_comparison_design.R to be sourced first.

SPEC_SCHEME_LEVELS <- c("vfci", "optimized", "separate")
SPEC_SCHEME_LABELS <- c(
  vfci = "VFCI weights (fixed, rank-1)",
  optimized = "Optimized weights (width-minimizing)",
  separate = "Per-PC instruments (I x J)"
)
SPEC_SCHEME_LABELS_SHORT <- c(
  vfci = "VFCI weights (fixed)",
  optimized = "Optimized weights",
  separate = "Per-PC (I x J)"
)
SPEC_OUTCOME_LEVELS <- c(
  "point", "point failed", "certified bounded", "certified unbounded",
  "no certified bound"
)

spec_components_label <- function(mode, components) {
  vapply(seq_along(components), function(i) {
    idx <- as.integer(strsplit(components[i], "-", fixed = TRUE)[[1]])
    paste(paste0(idx, "y"), collapse = ", ")
  }, "")
}

spec_stratum_label <- function(mode, components) {
  paste0("Bond maturities: ", spec_components_label(mode, components))
}

# Fold the raw (kind, width, bounded) columns into the reader-facing outcome.
# Width semantics are three-state at tau>0: finite+bounded = certified bounded,
# Inf (or an uncertified finite stray) = certified unbounded, NA = no certified
# bound (fail-closed solver outcome -- evidence of neither).
classify_spec_outcomes <- function(grid) {
  out <- character(nrow(grid))
  point <- grid$kind == "point"
  pfail <- grid$kind == "point-failed"
  nocert <- !point & !pfail & is.na(grid$width)
  bnd <- !point & !pfail & !nocert & grid$bounded & is.finite(grid$width)
  out[point] <- "point"
  out[pfail] <- "point failed"
  out[nocert] <- "no certified bound"
  out[bnd] <- "certified bounded"
  out[!point & !pfail & !nocert & !bnd] <- "certified unbounded"
  grid$outcome <- factor(out, levels = SPEC_OUTCOME_LEVELS)
  grid
}

spec_cell_keys <- function(df) {
  paste(df$mode, df$n_pcs, df$components, df$gamma, df$tau, sep = "|")
}

# Coverage is classified from the observed cells only (never env vars or file
# names): exact match to the full design, exact match to the quick design, or
# partial. The suffix keeps quick/partial artifacts from clobbering full ones.
spec_coverage <- function(grid) {
  obs <- unique(spec_cell_keys(grid))
  full_cells <- spec_comparison_design_cells(spec_comparison_design("full"))
  quick_cells <- spec_comparison_design_cells(spec_comparison_design("quick"))
  full_keys <- spec_cell_keys(full_cells)
  label <- if (setequal(obs, full_keys)) {
    "FULL GRID"
  } else if (setequal(obs, spec_cell_keys(quick_cells))) {
    "QUICK SUBGRID"
  } else {
    "PARTIAL GRID"
  }
  fmt_set <- function(x) paste(sort(unique(x)), collapse = ", ")
  n_missing <- sum(!(full_keys %in% obs))
  n_extra <- sum(!(obs %in% full_keys))
  miss <- full_cells[!(full_keys %in% obs), ]
  missing_line <- if (!nrow(miss)) {
    "no cells missing vs the full design"
  } else {
    by_stratum <- sort(
      table(spec_stratum_label(miss$mode, miss$components)),
      decreasing = TRUE
    )
    shown <- utils::head(by_stratum, 4)
    txt <- paste(sprintf("%s (%d)", names(shown), shown), collapse = "; ")
    if (length(by_stratum) > length(shown)) {
      txt <- paste0(
        txt, sprintf("; and %d more strata", length(by_stratum) - length(shown))
      )
    }
    paste0("missing cells by stratum: ", txt)
  }
  line <- sprintf(
    "%s: %d of %d full-design cells observed (tau: %s | n_pcs: %s | %d missing, %d outside design)",
    label, sum(obs %in% full_keys), length(full_keys),
    fmt_set(grid$tau), fmt_set(grid$n_pcs), n_missing, n_extra
  )
  block <- c(
    sprintf("Observed result cells: %d; modes: %s", length(obs), fmt_set(grid$mode)),
    sprintf(
      "n_pcs: %s   [full design: %s]",
      fmt_set(grid$n_pcs), fmt_set(full_cells$n_pcs)
    ),
    sprintf("tau: %s   [full design: %s]", fmt_set(grid$tau), fmt_set(full_cells$tau)),
    sprintf(
      "maturity sets: %s   [full design: %s]",
      fmt_set(grid$components[grid$mode == "maturities"]),
      fmt_set(full_cells$components[full_cells$mode == "maturities"])
    ),
    sprintf(
      "cells missing vs full design: %d; cells outside the design: %d",
      n_missing, n_extra
    ),
    missing_line
  )
  list(
    label = label,
    suffix = switch(label,
      "FULL GRID" = "",
      "QUICK SUBGRID" = "_quick",
      "PARTIAL GRID" = "_partial"
    ),
    line = line, block = block, missing_line = missing_line
  )
}

# One table cell per (scheme, tau): counts over the APPLICABLE OBSERVED cells
# only ("--" when a scheme does not exist at that tau, e.g. optimized at tau=0).
# `compact` (for the narrow LaTeX panel) reports "k/n (j unb., m ncb)";
# the panel notes spell the abbreviations out.
spec_outcome_cell <- function(rows, compact = FALSE) {
  n <- nrow(rows)
  if (n == 0) {
    return("--")
  }
  k <- table(factor(rows$outcome, levels = SPEC_OUTCOME_LEVELS))
  if (all(rows$tau == 0)) {
    txt <- sprintf("point %d/%d", k[["point"]], n)
    if (k[["point failed"]] > 0) {
      txt <- paste0(txt, sprintf(" (%d failed)", k[["point failed"]]))
    }
    return(txt)
  }
  n_unb <- k[["certified unbounded"]]
  n_ncb <- k[["no certified bound"]]
  txt <- sprintf(if (compact) "%d/%d" else "bounded %d/%d", k[["certified bounded"]], n)
  extras <- c(
    if (n_unb > 0) sprintf(if (compact) "%d unb." else "%d unbounded", n_unb),
    if (n_ncb > 0) sprintf(if (compact) "%d no cert." else "%d no cert. bound", n_ncb)
  )
  if (length(extras)) txt <- paste0(txt, " (", paste(extras, collapse = ", "), ")")
  txt
}

# Schemes x tau matrix of outcome-count cells (pre-formatted character).
# Pass `taus` to keep columns aligned across subsets (e.g. by-mode panels).
spec_outcome_matrix <- function(grid, taus = sort(unique(grid$tau)),
                                compact = FALSE,
                                scheme_labels = SPEC_SCHEME_LABELS) {
  schemes <- SPEC_SCHEME_LEVELS[SPEC_SCHEME_LEVELS %in% unique(grid$gamma)]
  cells <- vapply(taus, function(t) {
    vapply(schemes, function(g) {
      spec_outcome_cell(grid[grid$gamma == g & grid$tau == t, ], compact = compact)
    }, "")
  }, character(length(schemes)))
  df <- data.frame(
    Scheme = unname(scheme_labels[schemes]),
    matrix(cells, nrow = length(schemes)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(df) <- c("Scheme", paste0("tau = ", format(taus, trim = TRUE)))
  df
}
