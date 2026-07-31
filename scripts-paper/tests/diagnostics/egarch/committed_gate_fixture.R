# Output-independent fixture for the scientific gate payload bound by the
# tracked EGARCH decision. The payload is an xz-compressed version-3 RDS encoded
# as base64 so the fixture remains reviewable text and requires only base R.

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
