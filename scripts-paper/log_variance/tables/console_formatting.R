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
    if (identical(table$status[[index]], PAPER_ENDPOINT_STATUS[["bounded"]])) {
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

# One console line for an audit reconciliation frame: how many sides agree, or
# how many were downgraded and why
logvar_print_audit_summary <- function(audit, label = "sensitivity gate") {
  if (is.null(audit)) {
    cat(sprintf("  %s: no sides evaluated\n", label))
    return(invisible(NULL))
  }
  demoted <- audit[!is.na(audit$reason), , drop = FALSE]
  if (nrow(demoted) == 0L) {
    cat(sprintf("  %s: %d sides agree, none downgraded\n", label, nrow(audit)))
  } else {
    by_reason <- table(demoted$reason)
    cat(sprintf(
      "  %s: %d of %d sides downgraded (%s)\n",
      label, nrow(demoted), nrow(audit),
      paste(sprintf("%s=%d", names(by_reason), as.integer(by_reason)), collapse = " ")
    ))
  }
  invisible(NULL)
}
