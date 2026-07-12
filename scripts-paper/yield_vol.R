# Realized quarterly volatility of bond yields: within each quarter, the
# square root of the sum of squared business-day yield changes (realized
# volatility), one column per ACM maturity (y1_vol .. y120_vol), in the
# yields' percentage-point units, not annualized.
# Run via run_all.R, which defines the shared maturity grids. The daily ACM
# asset (~40 MB) is cache-only; the first run downloads it from GitHub.

acm_daily <- hetid::extract_acm_data(
  data_types = "yields",
  maturities = all_mats,
  frequency = "daily",
  auto_download = TRUE
)

yield_vol <- acm_daily |>
  dplyr::mutate(
    qtr = tsibble::yearquarter(date),
    dplyr::across(dplyr::starts_with("y"), \(y) (y - dplyr::lag(y))^2)
  ) |>
  dplyr::summarise(
    dplyr::across(dplyr::starts_with("y"), \(d2) sqrt(sum(d2, na.rm = TRUE))),
    .by = qtr
  ) |>
  dplyr::rename_with(\(x) paste0(x, "_vol"), !qtr)

rm(acm_daily)
