# Cross-platform comparison against stored donor oracles. Doubles may differ by
# floating-point noise (BLAS kernels, libm, long double width); every other
# type, every attribute, and the NA/NaN/Inf pattern must match exactly, so a
# status, label, counter, or missingness flip still fails.
#
# A finite double passes when |actual - expected| <= tolerance * max(|expected|, 1).
# "direct" covers closed-form evaluations and fits at a given point (observed
# cross-platform gap <= 1e-10); "solver" covers outputs of the nloptr searches
# (solver_xtol_rel = 1e-8; observed gap <= 2.3e-7 on the CI matrix).
ORACLE_TOLERANCE <- c(direct = 1e-8, solver = 1e-5)

oracle_mismatch <- function(actual, expected, tolerance, path = "value") {
  if (!identical(typeof(actual), typeof(expected)) ||
    !identical(length(actual), length(expected))) {
    return(sprintf("%s: type or length differs", path))
  }
  actual_attributes <- attributes(actual)
  expected_attributes <- attributes(expected)
  attribute_names <- sort(names(expected_attributes))
  if (!identical(sort(names(actual_attributes)), attribute_names)) {
    return(sprintf("%s: attribute names differ", path))
  }
  for (name in attribute_names) {
    gap <- oracle_mismatch(
      actual_attributes[[name]], expected_attributes[[name]], tolerance,
      sprintf("attr(%s, \"%s\")", path, name)
    )
    if (!is.null(gap)) {
      return(gap)
    }
  }
  if (is.double(actual)) {
    actual <- unclass(actual)
    expected <- unclass(expected)
    finite <- is.finite(expected)
    if (!identical(finite, is.finite(actual)) ||
      !identical(actual[!finite], expected[!finite])) {
      return(sprintf("%s: NA/NaN/Inf pattern differs", path))
    }
    excess <- abs(actual[finite] - expected[finite]) /
      pmax(abs(expected[finite]), 1) / tolerance
    if (length(excess) > 0L && max(excess) > 1) {
      i <- which(finite)[which.max(excess)]
      return(sprintf(
        "%s[%d]: actual %.17g, expected %.17g, scaled gap %.3g > %.3g",
        path, i, actual[i], expected[i], max(excess) * tolerance, tolerance
      ))
    }
    return(NULL)
  }
  if (is.list(actual)) {
    labels <- names(expected)
    for (i in seq_along(actual)) {
      label <- if (is.null(labels) || !nzchar(labels[i])) {
        sprintf("%s[[%d]]", path, i)
      } else {
        sprintf("%s$%s", path, labels[i])
      }
      gap <- oracle_mismatch(actual[[i]], expected[[i]], tolerance, label)
      if (!is.null(gap)) {
        return(gap)
      }
    }
    return(NULL)
  }
  if (!identical(actual, expected)) sprintf("%s: values differ", path)
}

expect_oracle_equal <- function(actual, expected, tolerance, info = NULL) {
  gap <- oracle_mismatch(actual, expected, tolerance)
  testthat::expect(is.null(gap), paste(c(gap, info), collapse = "\n"))
  invisible(actual)
}
