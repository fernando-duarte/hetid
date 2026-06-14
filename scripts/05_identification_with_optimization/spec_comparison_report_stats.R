# Aggregation and headline strings for the spec-comparison report: the audited
# per-stratum statistics behind every printed number and the computed bottom
# lines. Requires spec_comparison_report_utils.R to be sourced first.

# Audited aggregate behind every printed number: one row per scheme x tau x
# stratum with outcome counts, within-stratum width stats over certified
# bounded cells (never pooled across strata), and the matched-cell comparison
# against the optimized scheme (rescued counts; geometric-mean width ratio).
aggregate_spec_stats <- function(grid) {
  opt_cols <- c("n_pcs", "components", "tau", "width", "outcome")
  opt <- grid[grid$gamma == "optimized", opt_cols]
  names(opt)[names(opt) %in% c("width", "outcome")] <- c("opt_width", "opt_outcome")
  m <- merge(grid, opt, by = c("n_pcs", "components", "tau"), all.x = TRUE)
  m$stratum <- spec_stratum_label(m$components)
  rows <- lapply(split(m, list(m$gamma, m$tau, m$stratum), drop = TRUE), function(d) {
    fw <- d$width[d$outcome == "certified bounded"]
    opt_bnd <- !is.na(d$opt_outcome) & d$opt_outcome == "certified bounded"
    comp <- d$gamma != "optimized" & d$outcome == "certified bounded" & opt_bnd
    ratios <- d$width[comp] / d$opt_width[comp]
    data.frame(
      scheme = d$gamma[1], tau = d$tau[1], stratum = d$stratum[1],
      n_cells = nrow(d),
      n_point = sum(d$outcome == "point"),
      n_point_failed = sum(d$outcome == "point failed"),
      n_bounded = sum(d$outcome == "certified bounded"),
      n_unbounded = sum(d$outcome == "certified unbounded"),
      n_no_certified_bound = sum(d$outcome == "no certified bound"),
      width_median = if (length(fw)) stats::median(fw) else NA_real_,
      width_min = if (length(fw)) min(fw) else NA_real_,
      width_max = if (length(fw)) max(fw) else NA_real_,
      n_comparable_vs_optimized = sum(comp),
      geomean_width_ratio_vs_optimized = if (sum(comp)) exp(mean(log(ratios))) else NA_real_,
      n_rescued_by_optimized = sum(d$outcome == "certified unbounded" & opt_bnd),
      n_no_cert_but_optimized_bounded = sum(d$outcome == "no certified bound" & opt_bnd),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$scheme, out$tau, out$stratum), ]
}

# Computed headline strings; every count is over applicable observed cells.
spec_bottom_line <- function(grid, coverage) {
  cnt <- function(g) {
    d <- grid[grid$tau > 0 & grid$gamma == g, ]
    if (!nrow(d)) {
      return("n/a")
    }
    sprintf("%d/%d", sum(d$outcome == "certified bounded"), nrow(d))
  }
  pt <- grid[grid$tau == 0, ]
  conds <- pt$cond[is.finite(pt$cond)]
  fmt_cond <- function(x) formatC(x, format = "e", digits = 1)
  cond_str <- if (length(conds)) {
    sprintf(
      "condition numbers %s to %s; a numerical-sensitivity diagnostic, not sampling uncertainty",
      fmt_cond(min(conds)), fmt_cond(max(conds))
    )
  } else {
    "no successful point solves"
  }
  long <- sprintf(
    paste0(
      "In the observed %s: tau = 0 yields an algebraic point solution (width 0) in %d/%d",
      " applicable specs (%s). With slack tau > 0, solver-certified bounded sets:",
      " optimized weights %s, fixed VFCI weights %s, per-PC instruments %s."
    ),
    coverage$label, sum(pt$outcome == "point"), nrow(pt), cond_str,
    cnt("optimized"), cnt("vfci"), cnt("separate")
  )
  short <- sprintf(
    "Certified bounded sets at tau > 0: optimized %s, VFCI %s, per-PC %s",
    cnt("optimized"), cnt("vfci"), cnt("separate")
  )
  list(long = long, short = short)
}
