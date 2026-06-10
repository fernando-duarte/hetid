# Builder for the plain-text spec-comparison summary: the open-this-first
# artifact. Every number is computed from the grid at report time; the only
# static prose is definitional (what an object IS, never what the results
# show). Requires spec_comparison_report_utils.R and the artifacts file (for
# SPEC_WIDTH_LEGEND / SPEC_DENOM_NOTE) to be sourced first.

build_spec_summary_lines <- function(grid, cov, bl, agg, suffix) {
  fmt_g <- function(x) formatC(x, format = "g", digits = 3)
  wrap <- function(x, prefix = "") strwrap(x, width = 78, initial = prefix, exdent = 2)
  txt_tbl <- function(df) {
    old <- options(width = 200)
    on.exit(options(old), add = TRUE)
    paste0("  ", utils::capture.output(print(df, row.names = FALSE)))
  }

  by_mode <- spec_outcome_by_mode(grid)
  outcome_lines <- unlist(lapply(names(by_mode), function(nm) {
    c(paste0("  ", nm, ":"), txt_tbl(by_mode[[nm]]), "")
  }))

  bench <- grid[grid$outcome %in% c("point", "point failed"), ]
  bench_lines <- if (nrow(bench)) {
    bench <- utils::head(bench[order(-bench$cond), ], 10)
    txt_tbl(data.frame(
      Specification = spec_stratum_label(bench$mode, bench$components),
      n_pcs = bench$n_pcs,
      Weighting = unname(SPEC_SCHEME_LABELS[bench$gamma]),
      `Condition number` = ifelse(
        is.finite(bench$cond),
        formatC(bench$cond, format = "e", digits = 1), "point solve failed"
      ),
      check.names = FALSE
    ))
  } else {
    "  (no tau = 0 cells observed)"
  }

  pos <- grid[grid$tau > 0, ]
  pos$stratum <- spec_stratum_label(pos$mode, pos$components)
  stratum_lines <- unlist(lapply(split(pos, pos$stratum), function(d) {
    fin <- d[d$outcome == "certified bounded", ]
    if (!nrow(fin)) {
      return(sprintf(
        "  %s: no certified bounded cells (%d unbounded, %d no certified bound)",
        d$stratum[1], sum(d$outcome == "certified unbounded"),
        sum(d$outcome == "no certified bound")
      ))
    }
    best <- fin[which.min(fin$width), ]
    c(
      sprintf(
        "  %s: %d/%d cells certified bounded; width median %s, range %s to %s",
        d$stratum[1], nrow(fin), nrow(d), fmt_g(stats::median(fin$width)),
        fmt_g(min(fin$width)), fmt_g(max(fin$width))
      ),
      sprintf(
        "    tightest: %s, n_pcs %d, tau %s (width %s)",
        SPEC_SCHEME_LABELS[best$gamma], best$n_pcs, format(best$tau), fmt_g(best$width)
      )
    )
  }))

  resc <- agg[agg$scheme != "optimized" & agg$tau > 0, ]
  resc$scheme <- factor(resc$scheme, levels = SPEC_SCHEME_LEVELS)
  resc_lines <- unlist(lapply(split(resc, resc$scheme, drop = TRUE), function(d) {
    lab <- SPEC_SCHEME_LABELS[[as.character(d$scheme[1])]]
    out <- character(0)
    if (sum(d$n_unbounded) > 0) {
      out <- c(out, sprintf(
        "  %s: optimized weights certify a bounded set in %d of the %d cells",
        lab, sum(d$n_rescued_by_optimized), sum(d$n_unbounded)
      ), "    where this scheme is certified unbounded")
    }
    if (sum(d$n_no_certified_bound) > 0) {
      out <- c(out, sprintf(
        "  %s: optimized weights are bounded in %d of the %d cells where this",
        lab, sum(d$n_no_cert_but_optimized_bounded), sum(d$n_no_certified_bound)
      ), "    scheme has no certified bound")
    }
    if (!length(out)) sprintf("  %s: no unbounded or uncertified cells", lab) else out
  }))

  c(
    wrap(bl$long, prefix = "BOTTOM LINE: "),
    wrap(cov$line, prefix = "COVERAGE: "),
    "",
    "SPECIFICATION / INSTRUMENT / TAU COMPARISON FOR THE IDENTIFIED SET OF THETA",
    "===========================================================================",
    "",
    "COVERAGE DETAIL",
    paste0("  ", cov$block),
    "",
    "WHAT THIS COMPARES AND HOW TO READ IT",
    wrap(paste(
      "The triangular system Y1 = theta * Y2 + eps1, Y2 = gamma' Z + eps2 is",
      "identified through heteroskedasticity (Lewbel 2012) via the restriction",
      "E[eps1 * eps2 | Z] = tau * Var(eps2 | Z). At tau = 0 the restriction is",
      "exact and theta is point-identified; tau > 0 weakens it, and the",
      "identified SET collects every theta consistent with slack up to tau.",
      "Each result cell is one specification: an identification mode",
      "(yield-curve factors or bond maturities), a number of",
      "principal-component instruments (n_pcs), a component subset, and a PC",
      "weighting scheme (gamma). Optimized weights are a width-minimizing",
      "computational benchmark, not an economically structural weighting.",
      "Reported width is a scalar diagnostic: the sum of componentwise",
      "profile-bound interval lengths. It is not set volume and should be",
      "compared only within a fixed stratum."
    ), prefix = "  "),
    "",
    wrap(SPEC_WIDTH_LEGEND, prefix = "  "),
    "",
    "CERTIFICATION OUTCOMES BY WEIGHTING SCHEME",
    wrap(SPEC_DENOM_NOTE, prefix = "  "),
    "",
    outcome_lines,
    "ZERO-SLACK BENCHMARK (TAU = 0)",
    wrap(paste(
      "Width is 0 by construction at tau = 0; the condition number of the",
      "linear point solve is a numerical-sensitivity diagnostic, not sampling",
      "uncertainty. High values motivate the tau > 0 set analysis.",
      "Sorted from most to least numerically sensitive (top 10 shown)."
    ), prefix = "  "),
    bench_lines,
    "",
    "WIDTH MAGNITUDES WITHIN STRATA (TAU > 0)",
    wrap(paste(
      "Widths are comparable only within a stratum (mode x component subset);",
      "scales differ by orders of magnitude across strata, so no pooled",
      "statistics are reported. Counts cover all schemes at every tau > 0."
    ), prefix = "  "),
    stratum_lines,
    "",
    "RESCUED CELLS (OPTIMIZED VS FIXED SCHEMES)",
    wrap(paste(
      "Matched-cell comparisons against the optimized scheme; per-cell",
      "geometric-mean width ratios are in the aggregate CSV listed below.",
      "Optimized gamma is a width-minimizing computational benchmark, not an",
      "economically structural weighting."
    ), prefix = "  "),
    resc_lines,
    "",
    "FILES",
    sprintf("  spec_comparison_summary%s.txt        this file", suffix),
    sprintf("  spec_comparison_outcome_table%s.*    headline outcome counts (html/tex)", suffix),
    sprintf("  spec_comparison_benchmark_table%s.*  tau = 0 benchmark by conditioning", suffix),
    sprintf("  spec_comparison_outcomes%s.*         outcome counts figure (png/svg)", suffix),
    sprintf("  spec_comparison_widths%s.*           certified widths by stratum (png/svg)", suffix),
    sprintf("  spec_comparison_panel%s*.tex/.pdf    publication booktabs panel", suffix),
    sprintf("  temp: spec_comparison_agg%s.csv      audited aggregate behind every number", suffix),
    "  temp: spec_comparison_full.csv       raw grid (data dictionary below)",
    "",
    "GLOSSARY",
    "  theta      structural parameter vector (one element per component)",
    "  gamma      weights combining the n_pcs PC instruments into one instrument",
    "             per component; the four schemes differ only in gamma",
    "  tau        slack in the identifying restriction: 0 = exact (point);",
    "             larger tau = weaker assumption = (weakly) larger set",
    "  n_pcs      number of principal-component instruments",
    "  stratum    a mode x component-subset pair (widths comparable only within)",
    "  cond       condition number of the tau = 0 linear solve",
    "",
    "DATA DICTIONARY (temp/identification_optimized/spec_comparison_full.csv)",
    "  mode        'factors' (yield-curve factors) or 'maturities' (bonds)",
    "  n_pcs       number of PC instruments",
    "  components  component subset ('1-2-3' = level/slope/curvature;",
    "              '2-5' = the 2y and 5y maturities)",
    "  n_comp      number of theta components in the system",
    "  gamma       weighting scheme: vfci, reduced_form, optimized, separate",
    "  tau         slack in the identifying restriction",
    "  width       0 / finite / Inf / NA -- the four width states above",
    "  bounded     TRUE only for solver-certified finite widths",
    "  kind        internal solver codes (point, set, set(opt), set(ixj))",
    "  cond        condition number (tau = 0 rows only)"
  )
}
