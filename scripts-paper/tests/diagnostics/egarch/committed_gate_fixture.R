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
