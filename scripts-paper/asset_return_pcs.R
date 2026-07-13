# l.pc1 .. l.pc4: lagged principal components of nominal financial asset
# returns, the log-variance equation's conditioning variables PC_R. The
# scores are the bundled `variables` pc columns (PCs of a cross-section of
# nominal asset returns, never of yields), period-end normalized at
# ingestion and relabeled one
# quarter forward so a join by qtr picks up the quarter-t value in the row of
# quarter t+1; the columns get the grammar's lag prefix (l.pc1, ...).
# Run via run_all.R, which defines n_pc_r.

utils::data("variables", package = "hetid")
pc_r_src <- paste0("pc", seq_len(n_pc_r))
stopifnot(all(pc_r_src %in% names(variables)))
lag_asset_return_pc <- dplyr::bind_cols(
  tibble::tibble(
    qtr = tsibble::yearquarter(hetid::to_period_end(variables$date, "quarterly")) + 1L
  ),
  stats::setNames(variables[pc_r_src], paste0("l.", pc_r_src))
)

rm(variables, pc_r_src)
