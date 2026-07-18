# Shared status-aware formatting for estimator console reports.

logvar_hull_text <- function(
  table,
  digits = 3L,
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
