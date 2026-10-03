lv_set_morton_select <- function(mesh, max_points,
                                 control = lv_set_ppml_control()) {
  n <- nrow(mesh)
  k_cols <- ncol(mesh)
  bits <- control$morton_bits
  if (bits * k_cols > control$exact_double_bits) {
    lv_set_stop("The Morton key exceeds the exact range of a double.", call. = FALSE)
  }
  quantized <- matrix(0, n, k_cols)
  for (k in seq_len(k_cols)) {
    range_k <- range(mesh[, k])
    span <- range_k[2L] - range_k[1L]
    if (span > 0) quantized[, k] <- floor((mesh[, k] - range_k[1L]) / span * (2^bits - 1))
  }
  key <- numeric(n)
  for (k in seq_len(k_cols)) {
    for (position in 0:(bits - 1L)) {
      key <- key + ((quantized[, k] %/% (2^position)) %% 2) * 2^(position * k_cols + (k - 1L))
    }
  }
  tie <- do.call(paste, c(lapply(seq_len(k_cols), function(k) sprintf("%.17g", mesh[, k])),
    sep = "|"
  ))
  ranking <- base::order(key, tie, method = "radix")
  m <- if (is.null(max_points)) n else min(max_points, n)
  list(
    grid = mesh[ranking[1L + floor((0:(m - 1L)) * (n / m))], , drop = FALSE],
    selector_id = lv_set_morton_id()
  )
}
