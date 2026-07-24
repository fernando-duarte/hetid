# Definition-only assembly of the structural-equation inference table parts:
# row labels, the OLS / tau = 0 / display-tau columns, the tau column headers,
# and the interior \midrule positions, from the frozen mean-set estimate
# (estimate_identified_set.R) and the unified bootstrap stage.
# Shared by render_structural_equation_table.R (the standalone structural table)
# and render_combined_inference_table.R (Panel A of the merged table) so the two
# cannot drift on formatting. No side effects.

paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))

structural_equation_table_parts <- function(mean, boot, n_pc) {
  # coefficient rows: design block (b_0, b_E) then the news block (b_N); the
  # guard pins the row order the labels below assume
  coef_tab <- rbind(mean$beta1_table, mean$theta_table)
  stopifnot(identical(coef_tab$coef, c(
    PAPER_ANALYSIS_CONTRACT$model$intercept_col,
    PAPER_ANALYSIS_CONTRACT$model$lag_expected_pc_cols,
    PAPER_ANALYSIS_CONTRACT$model$news_pc_cols
  )))
  fmt <- function(x) {
    policy <- PAPER_REPORTING_CONTROL$cells$structural
    paper_format_number(x, policy$digits, policy$numeric_missing)
  }
  # a degenerate interval (exactly equal endpoints, the point-identified case)
  # is left blank: the set equals the tau = 0 point at every displayed tau
  set_cell <- function(lo, hi, status) {
    policy <- PAPER_REPORTING_CONTROL$cells$structural
    paper_format_set_interval(
      lo,
      hi,
      status,
      digits = policy$digits,
      status_mode = policy$status_mode,
      na_as_status = policy$na_as_status,
      infinite_bounds = policy$infinite_bounds,
      degenerate_rtol = policy$degenerate_rtol
    )
  }
  n_obs <- mean$sample$n
  r2 <- summary(mean$ols_fit)$r.squared
  # Newey-West t statistics and significance stars for the OLS column.
  nw <- paper_newey_west_statistics(
    mean$ols_fit,
    coef_tab$ols,
    coef_tab$coef,
    PAPER_REPORTING_CONTROL$mean_ols
  )
  ols_cells <- ifelse(
    nw$stars == "", fmt(coef_tab$ols),
    sprintf("%s$%s$", fmt(coef_tab$ols), nw$stars)
  )
  ols_tstats <- sprintf(
    "(%s)",
    paper_format_number(
      nw$statistic,
      PAPER_REPORTING_CONTROL$cells$statistic_digits,
      "na"
    )
  )
  # sampling uncertainty from the unified endpoint bootstrap:
  # robust nominal 90% intervals under the tau = 0 points, and Stoye-calibrated
  # nominal intervals under the set cells; blank cells stay blank
  stopifnot(identical(boot$point_ci$coef, coef_tab$coef))
  interval_cell <- function(lo, hi, blank) {
    policy <- PAPER_REPORTING_CONTROL$cells$structural
    paper_format_confidence_interval(
      lo,
      hi,
      digits = policy$digits,
      blank = blank,
      brackets = policy$confidence_brackets
    )
  }
  point_ci_cells <- interval_cell(
    boot$point_ci$lower, boot$point_ci$upper,
    !is.finite(coef_tab$point)
  )
  coef_labels <- c(
    "$b_0$",
    sprintf("$b_{%d,E}$", seq_len(n_pc)),
    sprintf("$b_{%d,N}$", seq_len(n_pc))
  )
  row_labels <- c(interleave(coef_labels, ""), "$R^2$", "$N$")
  # per-tau exact cells, interval frames, and integrity guards, computed once
  # for the inference table and its standalone
  set_data <- lapply(names(mean$set_tables), function(nm) {
    st <- mean$set_tables[[nm]]
    tab <- rbind(st$beta1, st$theta)
    stopifnot(identical(tab$coef, coef_tab$coef))
    inf <- boot$inference[[nm]]
    stopifnot(identical(inf$coef, coef_tab$coef))
    list(cells = set_cell(tab$set_lower, tab$set_upper, tab$status), inf = inf)
  })
  set_columns <- lapply(set_data, function(cell_dat) {
    sub <- interval_cell(
      cell_dat$inf$ci_lower, cell_dat$inf$ci_upper, cell_dat$cells == ""
    )
    c(interleave(cell_dat$cells, sub), PAPER_NA_TOKEN, sprintf("%d", n_obs))
  })
  columns <- c(
    list(
      c(
        interleave(ols_cells, ols_tstats),
        paper_format_number(
          r2,
          PAPER_REPORTING_CONTROL$cells$statistic_digits,
          "na"
        ),
        sprintf("%d", n_obs)
      ),
      c(
        interleave(fmt(coef_tab$point), point_ci_cells),
        PAPER_NA_TOKEN, sprintf("%d", n_obs)
      )
    ),
    unname(set_columns)
  )
  list(
    row_labels = row_labels,
    columns = columns,
    headers = paper_tau_col_headers(mean$tau_display),
    rule_after = c(2L * (1L + n_pc), 2L * (1L + 2L * n_pc))
  )
}
