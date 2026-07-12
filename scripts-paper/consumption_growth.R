# gr1.pcecc96: quarterly real consumption growth (percent, not annualized),
# from FRED PCECC96. The series is already quarterly on FRED (one observation
# per quarter), so the only steps are the yearquarter key and the geometric
# growth rate.
# Run via run_all.R, which defines the pull window and patches the FRED download.

gr1_pcecc96 <- tidyquant::tq_get(
  "PCECC96",
  get = "economic.data",
  from = fred_from,
  to = fred_to
) |>
  dplyr::transmute(
    qtr = tsibble::yearquarter(date),
    !!hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL :=
      100 * (price / dplyr::lag(price) - 1)
  )
