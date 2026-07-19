# Shared status-aware formatting for estimator console reports.

logvar_hull_text <- function(
  table,
  digits =
    PAPER_REPORTING_CONTROL$precision$console_significant,
  separator = ","
) {
  stopifnot(all(c(
    "set_lower", "set_upper", "status"
  ) %in% names(table)))
  format_string <- sprintf(
    "[%%.%dg%s%%.%dg]",
    as.integer(digits),
    separator,
    as.integer(digits)
  )
  vapply(seq_len(nrow(table)), function(index) {
    if (identical(table$status[[index]], "bounded")) {
      sprintf(
        format_string,
        table$set_lower[[index]],
        table$set_upper[[index]]
      )
    } else {
      table$status[[index]]
    }
  }, character(1))
}

logvar_print_map_summary <- function(
  title,
  map,
  taus,
  census = NULL,
  census_label = NULL
) {
  stopifnot(
    is.character(title),
    length(title) == 1L,
    length(map$sets) == length(taus),
    length(map$counts) == length(taus)
  )
  cat(sprintf(
    "%s: N = %d over %s to %s\n",
    title,
    map$sample$n,
    format(map$sample$span[[1L]]),
    format(map$sample$span[[2L]])
  ))
  keys <- names(map$sets)
  for (index in seq_along(keys)) {
    table <- map$sets[[keys[[index]]]]
    counts <- map$counts[[keys[[index]]]]
    hull <- logvar_hull_text(table)
    cat(sprintf(
      paste0(
        "  tau = %s: %s | attempted %d evaluated %d ",
        "cached %d failed %d\n"
      ),
      paper_format_tau(taus[[index]]),
      paste(hull, collapse = " "),
      counts$n_attempted,
      counts$n_evaluated,
      counts$n_cached,
      counts$n_failed
    ))
  }
  if (!is.null(census)) {
    stopifnot(
      length(census) == length(taus),
      is.character(census_label),
      length(census_label) == 1L
    )
    cat(sprintf(
      "  %s: %s\n",
      census_label,
      paste(
        paste0(
          paper_format_tau(taus),
          "=",
          census
        ),
        collapse = " "
      )
    ))
  }
  invisible(map)
}
