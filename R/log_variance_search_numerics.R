# Exact dyadic identities are deliberately limited to a safe integer lattice.
# x = sign * odd mantissa * 2^exponent, by exact power-of-two arithmetic
lv_log_dyadic <- function(x) {
  if (x == 0) {
    return(c(0, 0))
  }
  magnitude <- abs(x)
  exponent <- floor(log2(magnitude))
  if (2^exponent > magnitude) {
    exponent <- exponent - 1
  } else if (2^(exponent + 1) <= magnitude) {
    exponent <- exponent + 1
  }
  mantissa <- magnitude / 2^exponent * 2^(.Machine$double.digits - 1L)
  exponent <- exponent - (.Machine$double.digits - 1L)
  for (shift in c(32, 16, 8, 4, 2, 1)) {
    if (mantissa %% 2^shift == 0) {
      mantissa <- mantissa / 2^shift
      exponent <- exponent + shift
    }
  }
  c(sign(x) * mantissa, exponent)
}

lv_log_exact <- function(terms) {
  if (any(!is.finite(terms))) {
    return(NA_real_)
  }
  terms <- terms[rowSums(terms == 0) == 0L, , drop = FALSE]
  if (!nrow(terms)) {
    return(0)
  }
  limit <- 2^(.Machine$double.digits - 1L)
  if (nrow(terms) == 1L) {
    product <- lv_log_encode_product(c(abs(terms), prod(sign(terms))), limit)
    if (anyNA(product)) {
      return(NA_real_)
    }
    return(lv_log_exact_sum(matrix(product, 1L), limit))
  }
  # one key per row: its sorted magnitudes, so equal monomials group together
  magnitude <- abs(terms)
  if (ncol(terms) > 1L) {
    magnitude <- matrix(magnitude[order(row(magnitude), magnitude)],
      ncol = ncol(terms), byrow = TRUE
    )
  }
  formatted <- matrix(sprintf("%a", magnitude), nrow(terms))
  keys <- do.call(paste, c(
    lapply(seq_len(ncol(formatted)), function(j) formatted[, j]),
    sep = ":"
  ))
  signs <- Reduce(`*`, lapply(seq_len(ncol(terms)), function(j) sign(terms[, j])))
  encoded <- list()
  for (key in unique(keys)) {
    count <- sum(signs[keys == key])
    if (count == 0) next
    product <- lv_log_encode_product(c(abs(terms[match(key, keys), ]), count), limit)
    if (anyNA(product)) {
      return(NA_real_)
    }
    encoded[[length(encoded) + 1L]] <- product
  }
  if (!length(encoded)) {
    return(0)
  }
  lv_log_exact_sum(do.call(rbind, encoded), limit)
}

lv_log_encode_product <- function(values, limit) {
  mantissa <- 1
  exponent <- 0
  for (value in values) {
    dyadic <- lv_log_dyadic(value)
    mantissa <- mantissa * dyadic[1L]
    exponent <- exponent + dyadic[2L]
    if (!is.finite(mantissa) || abs(mantissa) > limit) {
      return(c(NA_real_, NA_real_))
    }
  }
  c(mantissa, exponent)
}

lv_log_exact_sum <- function(encoded, limit) {
  exponent <- min(encoded[, 2L])
  integers <- encoded[, 1L] * 2^(encoded[, 2L] - exponent)
  if (any(!is.finite(integers)) || sum(abs(integers)) > limit) {
    return(NA_real_)
  }
  total <- sum(integers)
  if (total == 0) {
    return(0)
  }
  unit <- 2^exponent
  value <- total * unit
  if (!is.finite(value) || unit == 0 || !is.finite(unit) || value / unit != total) {
    return(NA_real_)
  }
  value
}

lv_log_point_op <- function(a, b, op) {
  switch(op,
    "+" = lv_log_exact(cbind(c(a, b), 1)),
    "-" = lv_log_exact(cbind(c(a, -b), 1)),
    "*" = lv_log_exact(matrix(c(a, b), 1L)),
    "/" = {
      candidate <- a / b
      remainder <- lv_log_exact(rbind(c(a, 1), c(-candidate, b)))
      if (!is.na(remainder) && remainder == 0) candidate else NA_real_
    }
  )
}

lv_log_op <- function(a, b, op) {
  unknown <- c(-Inf, Inf)
  if (any(!is.finite(c(a, b)))) {
    return(unknown)
  }
  if (op == "/" && b[1L] <= 0 && b[2L] >= 0) {
    return(unknown)
  }
  if (a[1L] == a[2L] && b[1L] == b[2L]) {
    exact <- lv_log_point_op(a[1L], b[1L], op)
    if (is.finite(exact)) {
      return(rep(exact, 2L))
    }
  }
  values <- switch(op,
    "+" = c(a[1L] + b[1L], a[2L] + b[2L]),
    "-" = c(a[1L] - b[2L], a[2L] - b[1L]),
    "*" = as.vector(outer(a, b, "*")),
    "/" = as.vector(outer(a, b, "/"))
  )
  if (any(!is.finite(values))) {
    return(unknown)
  }
  bounds <- range(values)
  for (pass in seq_len(2L)) {
    pad <- 4 * .Machine$double.eps * abs(bounds) + .Machine$double.xmin
    bounds <- bounds + c(-pad[1L], pad[2L])
  }
  if (any(!is.finite(bounds))) unknown else bounds
}

lv_log_dot <- function(a, b) {
  exact <- lv_log_exact(cbind(a, b))
  if (!is.na(exact)) {
    return(rep(exact, 2L))
  }
  value <- c(0, 0)
  for (i in seq_along(a)) {
    value <- lv_log_op(value, lv_log_op(rep(a[i], 2L), rep(b[i], 2L), "*"), "+")
  }
  value
}

lv_log_sum <- function(values) {
  Reduce(function(a, b) lv_log_op(a, b, "+"), values, init = c(0, 0))
}

lv_log_matvec <- function(matrix, vector) {
  lapply(seq_len(nrow(matrix)), function(i) {
    lv_log_sum(lapply(seq_len(ncol(matrix)), function(j) {
      lv_log_op(matrix[[i, j]], vector[[j]], "*")
    }))
  })
}

lv_log_norm <- function(values) {
  lv_log_sum(lapply(values, function(value) rep(max(abs(value)), 2L)))[2L]
}

lv_log_terms <- function(terms) {
  exact <- lv_log_exact(terms)
  if (!is.na(exact)) {
    return(rep(exact, 2L))
  }
  lv_log_sum(lapply(seq_len(nrow(terms)), function(i) {
    Reduce(function(a, b) lv_log_op(a, rep(b, 2L), "*"), terms[i, ], init = c(1, 1))
  }))
}
