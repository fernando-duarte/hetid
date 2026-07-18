# Shared box-growth classifier for coordinate and linear-functional bounds.

.profile_invalid_bound <- function() {
  list(
    bound = NA_real_,
    bounded = FALSE,
    valid = FALSE
  )
}

.profile_unbounded_bound <- function(value) {
  list(
    bound = value,
    bounded = FALSE,
    valid = TRUE
  )
}

.classify_profile_search <- function(
  quadratic,
  delta,
  omega,
  boxes,
  solve_box,
  value_at,
  finalize,
  trusted_at,
  unbounded_value,
  target_edge_is_unbounded,
  feasibility_tolerance,
  edge_rtol = PAPER_QUADRATIC_CONTROL$bound_edge_rtol,
  stability_rtol =
    PAPER_QUADRATIC_CONTROL$bound_stability_rtol,
  growth_factor =
    PAPER_QUADRATIC_CONTROL$unbounded_growth_factor
) {
  first <- solve_box(boxes[[1L]])
  if (.solve_finite(first) &&
    all(abs(first$phi) < edge_rtol * boxes[[1L]])) {
    return(finalize(first))
  }

  previous <- NULL
  trusted_at_last <- FALSE
  for (box in boxes[-1L]) {
    result <- solve_box(box)
    if (!.solve_finite(result)) {
      return(.profile_invalid_bound())
    }
    residual <- .feasibility_residual(
      quadratic,
      delta * result$phi,
      omega
    )
    if (!is.finite(residual) ||
      residual > feasibility_tolerance) {
      return(.profile_invalid_bound())
    }
    if (all(abs(result$phi) < edge_rtol * box)) {
      return(finalize(result))
    }

    trusted_at_last <- isTRUE(trusted_at(result, box))
    if (!trusted_at_last) {
      previous <- NULL
      next
    }
    current <- value_at(result)
    if (!is.null(previous)) {
      stable <- abs(current - previous) <=
        stability_rtol * max(1, abs(current))
      if (stable) {
        return(finalize(result))
      }
      growing <- abs(current) >=
        growth_factor * max(abs(previous), delta)
      if (growing) {
        return(.profile_unbounded_bound(unbounded_value))
      }
    }
    previous <- current
  }

  if (isTRUE(target_edge_is_unbounded) &&
    !trusted_at_last) {
    return(.profile_unbounded_bound(unbounded_value))
  }
  .profile_invalid_bound()
}
