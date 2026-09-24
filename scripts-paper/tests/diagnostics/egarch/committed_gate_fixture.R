# Output-independent fixture for the scientific gate payload bound by the
# tracked EGARCH decision. The payload is an xz-compressed version-3 RDS encoded
# as base64 so the fixture remains reviewable text and requires only base R.
# Matches the August instrument update recorded in commit 7e8c7dd; its scientific
# digest must equal the independently pinned decision record before fixture use.

.egarch_base64_decode <- function(parts) {
  encoded <- sub("=+$", "", paste0(parts, collapse = ""))
  alphabet <- strsplit(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    "",
    fixed = TRUE
  )[[1L]]
  values <- match(strsplit(encoded, "", fixed = TRUE)[[1L]], alphabet) - 1L
  stopifnot(!anyNA(values))
  bits <- as.vector(vapply(
    values,
    function(value) rev(as.integer(intToBits(value))[seq_len(6L)]),
    integer(6L)
  ))
  byte_count <- length(bits) %/% 8L
  bit_matrix <- matrix(bits[seq_len(byte_count * 8L)], nrow = 8L)
  as.raw(colSums(bit_matrix * 2^(7L:0L)))
}

egarch_committed_gate_fixture <- function() {
  encoded <- c(
    "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"
  )
  payload <- memDecompress(.egarch_base64_decode(encoded), type = "xz")
  connection <- rawConnection(payload)
  on.exit(close(connection), add = TRUE)
  readRDS(connection)
}
