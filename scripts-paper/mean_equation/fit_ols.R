# OLS regression of consumption growth on the lagged expected-SDF PCs and the
# SDF news PCs over the sample window.
# Run via run_pipeline.R after build_consumption_growth.R and build_sdf_pcs.R.

# one list drives both the join and the formula, so a frame added here enters
# the regression too
regressors <- list(lag_expected_sdf_pc, sdf_news_pc)

reg_data <- c(list(gr1_pcecc96), regressors) |>
  purrr::reduce(dplyr::full_join, by = "qtr") |>
  filter_window()

ols_mean_eq <- stats::lm(
  stats::reformulate(
    purrr::list_c(purrr::map(regressors, value_cols)),
    response = hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
  ),
  data = reg_data
)

rm(regressors, reg_data)
