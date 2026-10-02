# Output-independent fixture for the scientific gate payload bound by the
# tracked EGARCH decision. The payload is an xz-compressed version-3 RDS encoded
# as base64 so the fixture remains reviewable text and requires only base R.
# Matches the re-record for the QR log-projection operator (2026-10-02); its
# scientific digest must equal the independently pinned decision record before
# fixture use.

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
