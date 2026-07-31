# Row labels and columns for the log-OLS panel. Split out of the fragment the
# combined panels table used to build, so the per-estimator document can stack
# this panel under the mean equation: every other estimator already exposes
# parts through logvar_estimator_panel_parts, and log-OLS was the only one that
# did not. Definitions only.

paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))

logvar_logols_table_parts <- function(n_obs) {
  tab <- log_var_eq$table
  nw <- paper_newey_west_statistics(
    log_var_eq$fit_ols,
    tab$ols,
    tab$coef,
    PAPER_REPORTING_CONTROL$logvar_logols
  )
  stopifnot(!anyNA(nw$se))
  cells <- ifelse(
    nw$stars == "", fmt(tab$ols),
    sprintf("%s$%s$", fmt(tab$ols), nw$stars)
  )
  labels <- c(
    "$\\theta^{log}_0$", sprintf("$\\theta^{log}_{%d,R}$", seq_len(n_pc_r))
  )
  rows <- c(interleave(labels, ""), "$R^2$", "$N$")
  r2 <- summary(log_var_eq$fit_ols)$r.squared
  cols <- c(
    list(
      c(
        interleave(
          cells,
          sprintf(
            "(%s)",
            paper_format_number(
              nw$statistic,
              PAPER_REPORTING_CONTROL$cells$statistic_digits,
              "na"
            )
          )
        ),
        paper_format_number(
          r2,
          PAPER_REPORTING_CONTROL$cells$statistic_digits,
          "na"
        ),
        sprintf("%d", n_obs)
      ),
      c(interleave(fmt(tab$point), ""), PAPER_NA_TOKEN, sprintf("%d", n_obs))
    ),
    unname(lapply(log_var_eq$sets, function(st) {
      logvar_assert_coef_aligned(st$coef, tab$coef)
      c(
        interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
        PAPER_NA_TOKEN, sprintf("%d", n_obs)
      )
    }))
  )
  list(rows = rows, columns = cols)
}
