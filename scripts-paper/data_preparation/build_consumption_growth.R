# gr1.pcecc96: quarterly real consumption growth (percent, not annualized),
# from FRED PCECC96. The series is already quarterly on FRED (one observation
# per quarter), so the only steps are the yearquarter key and the geometric
# growth rate.
# Run via run_pipeline.R, which defines the pull window and the input source and
# patches the FRED download.

paper_source_once(paper_path("support", "data", "frozen_inputs.R"))

# fred_source picks the input. The frozen snapshot is the default, so a normal
# run is offline and the vintage is whatever is committed; "live" refreshes it
# and rewrites the file. A missing snapshot stops the run instead of falling
# back to a download, because a silent fall back would move every number in the
# paper without saying so. paper_verify_frozen_inputs() has already checked the
# snapshot exists by the time this runs; the guard below covers direct use.
paper_consumption_input <- function() {
  path <- paper_consumption_snapshot()
  if (identical(fred_source, "live")) {
    live <- tidyquant::tq_get(
      PAPER_ANALYSIS_CONTRACT$input$consumption$fred_series,
      get =
        PAPER_ANALYSIS_CONTRACT$input$consumption$fetch_kind,
      from = fred_from,
      to = fred_to
    )
    utils::write.csv(live, path, row.names = FALSE)
    return(live)
  }
  paper_check_source_switch(fred_source, "fred_source")
  if (!file.exists(path)) {
    stop(sprintf(paste0(
      "the frozen consumption snapshot is missing at %s.\n",
      "  Set fred_source <- \"live\" in config/analysis.R to download the ",
      "current vintage and rewrite it."
    ), path), call. = FALSE)
  }
  snapshot <- tibble::as_tibble(utils::read.csv(
    path,
    stringsAsFactors = FALSE
  ))
  snapshot$date <- as.Date(snapshot$date)
  snapshot
}

gr1_pcecc96 <- paper_consumption_input() |>
  dplyr::transmute(
    qtr = tsibble::yearquarter(date),
    !!hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL :=
      100 * (price / dplyr::lag(price) - 1)
  )
