# Numeric token parsing for final published TeX tables.

PAPER_TABLE_NUMBER_PATTERN <- paste0(
  "[-+]?(?:",
  "(?:[0-9]+(?:[.][0-9]*)?)|(?:[.][0-9]+)",
  ")(?:[eE][-+]?[0-9]+)?"
)

PAPER_TABLE_TOKEN_PATTERN <- paste0(
  "(?:",
  PAPER_TABLE_NUMBER_PATTERN,
  "[[:space:]]*\\\\times[[:space:]]*10\\^\\{[-+]?[0-9]+\\}",
  "|",
  PAPER_TABLE_NUMBER_PATTERN,
  ")"
)

# Stars follow their number either bare ("0.796***", the paper's convention) or
# as a superscript group ("0.796$^{***}$", the earlier one). Both are read, so
# a table that still carries the old markup keeps its stars compared rather than
# silently reporting none.
PAPER_TABLE_STAR_PATTERN <- paste0(
  "^[[:space:]]*[$]?[[:space:]]*",
  "(?:\\^\\{([*]{1,3})\\}|([*]{1,3}))",
  "[[:space:]]*[$]?"
)

paper_table_normalize_token <- function(token) {
  if (!grepl("\\times", token, fixed = TRUE)) {
    return(token)
  }
  pieces <- regmatches(
    token,
    gregexpr(PAPER_TABLE_NUMBER_PATTERN, token, perl = TRUE)
  )[[1L]]
  stopifnot(length(pieces) == 3L, identical(pieces[[2L]], "10"))
  paste0(pieces[[1L]], "e", pieces[[3L]])
}

paper_table_number_quantum <- function(token) {
  mantissa <- sub("[eE].*$", "", token)
  exponent_text <- sub("^.*[eE]", "", token)
  exponent <- if (identical(exponent_text, token)) {
    0L
  } else {
    as.integer(exponent_text)
  }
  decimal <- regexpr(".", mantissa, fixed = TRUE)[[1L]]
  places <- if (decimal < 0L) 0L else nchar(mantissa) - decimal
  10^(exponent - places)
}

paper_table_token_stars <- function(cell, starts, lengths) {
  vapply(seq_along(starts), function(index) {
    tail <- substring(cell, starts[[index]] + lengths[[index]])
    match <- regexec(PAPER_TABLE_STAR_PATTERN, tail, perl = TRUE)
    pieces <- regmatches(tail, match)[[1L]]
    if (length(pieces) < 2L) {
      return("")
    }
    # one alternative captures, the other is empty
    groups <- pieces[-1L][nzchar(pieces[-1L])]
    if (length(groups)) groups[[1L]] else ""
  }, character(1))
}

paper_table_cell_results <- function(cell) {
  matches <- gregexpr(PAPER_TABLE_TOKEN_PATTERN, cell, perl = TRUE)[[1L]]
  if (matches[[1L]] == -1L) {
    return(data.frame(
      value = double(),
      quantum = double(),
      stars = character()
    ))
  }
  tokens <- regmatches(cell, list(matches))[[1L]]
  normalized <- vapply(tokens, paper_table_normalize_token, character(1))
  data.frame(
    value = as.numeric(normalized),
    quantum = vapply(normalized, paper_table_number_quantum, numeric(1)),
    stars = paper_table_token_stars(cell, matches, attr(matches, "match.length")),
    row.names = NULL
  )
}
