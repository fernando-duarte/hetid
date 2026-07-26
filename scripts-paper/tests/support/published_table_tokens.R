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
  places <- if (decimal < 0L) {
    0L
  } else {
    nchar(mantissa) - decimal
  }
  10^(exponent - places)
}

paper_table_cell_numbers <- function(cell) {
  matches <- gregexpr(PAPER_TABLE_TOKEN_PATTERN, cell, perl = TRUE)
  tokens <- regmatches(cell, matches)[[1L]]
  if (identical(tokens, character())) {
    return(data.frame(
      value = double(),
      quantum = double()
    ))
  }
  tokens <- vapply(tokens, paper_table_normalize_token, character(1))
  data.frame(
    value = as.numeric(tokens),
    quantum = vapply(tokens, paper_table_number_quantum, numeric(1))
  )
}
