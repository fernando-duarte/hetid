test_that("one-sided tails retain the line and do not imply both sides", {
  q <- list(A_i = list(matrix(0, 1, 1)), b_i = list(1), c_i = -1)
  found <- identified_set_search(0, matrix(1), q, 3, matrix(c(1, -1, 0), 1),
    evidence = TRUE
  )
  found <- apply_recession_bounds(found, q, matrix(c(1, -1, 0), 1))
  expect_equal(found$lower, c(-Inf, -1, 0))
  expect_equal(found$upper, c(1, Inf, 0))
  expect_identical(found$tail_lower[[1]]$kind, "line_tail")
  expect_equal(found$tail_lower[[1]]$direction, -1)
  expect_null(found$tail_upper[[1]])
  expect_null(found$tail_lower[[3]])
})

test_that("large bounded lines stay finite with tight search caps", {
  q <- list(A_i = list(matrix(1e-24)), b_i = list(0), c_i = -1)
  found <- identified_set_search(0, matrix(1), q, 3, matrix(1),
    evidence = TRUE, max_growth = 1, search_limit = 2
  )
  expect_equal(found$lower, -1e12)
  expect_equal(found$upper, 1e12)
  expect_null(found$tail_upper[[1]])
})

test_that("growth exhaustion records a limit without manufacturing infinity", {
  q <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1e4)
  obj <- cbind(diag(2), c(1, 1))
  for (limit in c(2, 4096)) {
    found <- identified_set_search(c(0, 0), diag(2), q, 3, obj,
      n_primary = 2, evidence = TRUE, max_growth = 1, search_limit = limit
    )
    reasons <- vapply(found$search$phases, `[[`, character(1), "stop_reason")
    expect_true(if (limit == 2) "search_limit" %in% reasons else "pass_limit" %in% reasons)
    expect_true(all(is.finite(c(found$lower, found$upper))))
    expect_lte(found$upper[3], sqrt(2e4))
    expect_true(all(vapply(found$tail_upper, is.null, logical(1))))
  }
})

test_that("a cylinder retains zero-curvature tail evidence", {
  q <- list(A_i = list(diag(c(1, 0))), b_i = list(c(0, 0)), c_i = -1)
  obj <- diag(2)
  found <- apply_recession_bounds(
    identified_set_search(c(0, 0), diag(2), q, 3, obj, evidence = TRUE), q, obj
  )
  expect_equal(found$lower, c(-1, -Inf))
  expect_equal(found$upper, c(1, Inf))
  proof <- found$tail_upper[[2]]
  expect_identical(proof$kind, "line_tail")
  expect_lte(max(make_system_checker(q)(proof$origin + 1e6 * proof$direction)), 0)
})

test_that("strict curvature certifies orthogonal nonzero objectives via its open cone", {
  q <- list(A_i = list(-diag(2)), b_i = list(c(0, 0)), c_i = 1)
  v <- recession_direction(q)
  obj <- cbind(orthogonal = c(v[2], -v[1]), flat = c(0, 0))
  found <- identified_set_search(c(2, 0), diag(2), q, 3, obj, evidence = TRUE)
  found <- apply_recession_bounds(found, q, obj)
  expect_identical(found$lower, c(orthogonal = -Inf, flat = 0))
  expect_identical(found$upper, c(orthogonal = Inf, flat = 0))
  proof <- found$tail_upper[[1]]
  expect_identical(proof$kind, "strict_curvature")
  expect_equal(sum(obj[, 1] * proof$direction), 0)
  expect_lt(drop(crossprod(proof$direction, q$A_i[[1]] %*% proof$direction)), 0)
})

test_that("objective overflow raises a structured error instead of infinity", {
  q <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -4)
  expect_error(identified_set_search(0, matrix(1), q, 3, matrix(1e308), evidence = TRUE),
    "numeric range",
    class = "hetid_error"
  )
})

test_that("recession search restores an absent as well as an existing RNG seed", {
  q <- list(A_i = list(-diag(2)), b_i = list(c(0, 0)), c_i = 1)
  set.seed(32)
  before <- .Random.seed
  recession_direction(q)
  expect_identical(.Random.seed, before)
  probe <- function() {
    on.exit(assign(".Random.seed", before, globalenv())) # nolint: object_name_linter.
    rm(".Random.seed", envir = globalenv())
    recession_direction(q)
    exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  expect_false(probe())
})
