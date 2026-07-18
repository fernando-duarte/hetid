# Quarterly expected SDF and SDF news series across all ACM maturities.
# Run via run_pipeline.R, which defines the shared maturity grids.

acm <- quarterly_acm_inputs$data
yields <- quarterly_acm_inputs$yields
term_premia <- quarterly_acm_inputs$term_premia

#' Helper function: panel of an SDF series, one column per maturity in mats_qtr
sdf_panel <- function(fn, prefix) {
  mats_qtr |>
    purrr::set_names(paste0(prefix, mats_qtr)) |>
    purrr::map(\(i) {
      fn(
        yields,
        term_premia,
        i = i,
        step = step_qtr,
        dates = quarterly_acm_inputs$dates
      )[[2]]
    }) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      qtr = tsibble::yearquarter(quarterly_acm_inputs$dates),
      .before = 1
    )
}

sdf_news <- sdf_panel(hetid::compute_sdf_innovations, news_prefix)
expected_sdf <- sdf_panel(hetid::compute_expected_sdf, expected_prefix)

# expected SDF lagged lag_qtrs quarters: relabel each value forward so a join
# by qtr picks up the lagged value; columns get the grammar's lag prefix
# (l.expected_sdf_m3, ... for the first lag)
lag_prefix <- paste0("l", if (lag_qtrs == 1L) "" else lag_qtrs, ".")
lag_expected_sdf <- expected_sdf |>
  dplyr::mutate(qtr = qtr + lag_qtrs) |>
  dplyr::rename_with(\(x) paste0(lag_prefix, x), !qtr)

# keep only the panels; the ACM inputs were just function arguments here
rm(acm, yields, term_premia, sdf_panel, lag_prefix)
