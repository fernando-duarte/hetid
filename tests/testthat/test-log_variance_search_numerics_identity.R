# The exact helpers back certificates: pin the arithmetic decomposition and the
# vectorized grouping keys to the string-parsing versions they replaced.
old_log_dyadic <- function(x) {
  if (x == 0) {
    return(c(0, 0))
  }
  parts <- strsplit(sprintf("%a", abs(x)), "p", fixed = TRUE)[[1L]]
  fractional <- strsplit(substring(parts[1L], 3L), ".", fixed = TRUE)[[1L]]
  exponent <- as.numeric(parts[2L]) -
    if (length(fractional) == 2L) 4 * nchar(fractional[2L]) else 0
  mantissa <- 0
  for (digit in strsplit(paste(fractional, collapse = ""), "", fixed = TRUE)[[1L]]) {
    mantissa <- 16 * mantissa + strtoi(digit, base = 16L)
  }
  while (mantissa %% 2 == 0) {
    mantissa <- mantissa / 2
    exponent <- exponent + 1
  }
  c(sign(x) * mantissa, exponent)
}

test_that("the arithmetic dyadic decomposition matches the string parser", {
  set.seed(5)
  edges <- c(
    0, -0, 1, -1, 2^(-1074), -2^(-1074), 2^(-1022), 3 * 2^(-1074), .Machine$double.xmin,
    .Machine$double.xmax, -.Machine$double.xmax, 2^(-60:60), 0.1, 1 / 3, 1 - 2^-53,
    1 + 2^-52, 2^1023, 123456789
  )
  values <- c(edges, rnorm(3000) * 10^runif(3000, -300, 300), runif(500, 0, 1e-310))
  for (x in values) expect_identical(lv_log_dyadic(x), old_log_dyadic(x))
})

test_that("vectorized grouping keys give the exact sums of the row-wise keys", {
  old_exact <- function(terms) {
    if (any(!is.finite(terms))) {
      return(NA_real_)
    }
    terms <- terms[rowSums(terms == 0) == 0L, , drop = FALSE]
    if (!nrow(terms)) {
      return(0)
    }
    keys <- apply(terms, 1L, function(row) paste(sprintf("%a", sort(abs(row))), collapse = ":"))
    signs <- apply(sign(terms), 1L, prod)
    encoded <- list()
    limit <- 2^(.Machine$double.digits - 1L)
    for (key in unique(keys)) {
      count <- sum(signs[keys == key])
      if (count == 0) next
      product <- lv_log_encode_product(c(abs(terms[match(key, keys), ]), count), limit)
      if (anyNA(product)) {
        return(NA_real_)
      }
      encoded[[length(encoded) + 1L]] <- product
    }
    if (!length(encoded)) 0 else lv_log_exact_sum(do.call(rbind, encoded), limit)
  }
  set.seed(9)
  pool <- c(0, 0.5, -0.25, 3, -3, 1e-300, 2^40, 0.1, -0.1)
  for (k in 1:800) {
    width <- sample(1:3, 1L)
    terms <- matrix(sample(pool, sample(1:6, 1L) * width, TRUE), ncol = width)
    if (k %% 9 == 0) terms[1L] <- Inf
    expect_identical(lv_log_exact(terms), old_exact(terms))
  }
})
