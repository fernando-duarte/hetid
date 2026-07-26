# Deliberate renamed duplicates for the SSOT regression.

renamed_extract_results <- function(cell) {
  matches <- gregexpr(
    "[-+]?[0-9]+(?:[.][0-9]+)?",
    cell,
    perl = TRUE
  )[[1L]]
  tokens <- regmatches(cell, list(matches))[[1L]]
  as.numeric(tokens)
}

renamed_display_quantum <- function(token) {
  mantissa <- sub("[eE].*$", "", token)
  exponent <- as.integer(sub("^.*[eE]", "", token))
  decimal <- regexpr(".", mantissa, fixed = TRUE)[[1L]]
  places <- nchar(mantissa) - decimal
  10^(exponent - places)
}

renamed_rounding_comparator <- function(reference, candidate) {
  difference <- abs(reference$value - candidate$value)
  overlap <- (reference$quantum + candidate$quantum) / 2
  slack <- 8 * .Machine$double.eps * overlap
  difference < overlap - slack
}
