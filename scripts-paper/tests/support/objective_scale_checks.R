# Scaling a nonzero objective cannot change its optimizer or tail classification.
local({
  shifted_ball <- list(A_i = list(matrix(1)), b_i = list(-4), c_i = 3)
  for (loading in c(1, 1e-13, 1e13, -1, -1e-13, -1e13)) {
    lower <- solve_linear_functional_bound(shifted_ball, loading, "min")
    upper <- solve_linear_functional_bound(shifted_ball, loading, "max")
    expected <- sort(loading * c(1, 3)) / abs(loading)
    actual <- c(lower$bound, upper$bound) / abs(loading)
    check(
      sprintf("shifted interval endpoints preserve objective scale %g", loading),
      lower$valid && upper$valid && lower$bounded && upper$bounded &&
        max(abs(actual - expected)) < 1e-6
    )
  }

  wide_interval <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1e12)
  evidence <- paper_profile_evidence(wide_interval, matrix(1))
  for (loading in c(1, 1e-13, 1e13)) {
    edge <- .classify_profile_search(
      loading, "max", c(10, 100, 1000),
      function(box) list(phi = box, convergence = 0L),
      1, evidence, 1L,
      candidate_is_endpoint = function(theta) TRUE
    )
    check(
      sprintf("growing box edges do not become stable at objective scale %g", loading),
      !edge$valid && is.na(edge$bound)
    )
  }

  ball <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
  loading <- c(1e308, 1e-308)
  unresolved <- solve_linear_functional_bound(ball, loading, "max")
  check(
    "objective normalization that loses a nonzero loading stays unresolved",
    !unresolved$valid && is.na(unresolved$bound)
  )
})
