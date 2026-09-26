#' Outer Bounds Closure for Quadratic Set Evidence
#'
#' Internal factory for the `outer_bounds` closure that
#' `compute_quadratic_set_evidence()` returns. The closure bounds linear
#' objectives over the feasible set from below and above using only
#' nonnegative constraint combinations whose Hessian is verified positive
#' definite. Its results contain the set whether or not the set is nonempty.
#' They are not attained values and say nothing about optimizer accuracy.
#'
#' Rows are rescaled by powers of two, which leaves the feasible set unchanged
#' exactly; a row whose rescaling would lose a nonzero entry makes every bound
#' unknown. The certificate's weights, `weights / scales` on the original rows,
#' form the common candidate. Per-side candidates come from a deterministic
#' Nelder-Mead search limited by `maxit` iterations that uses no random
#' numbers. Every candidate, including those passed back through `pool`, is
#' verified independently before it bounds anything, and a side takes the
#' tightest verified bound.
#'
#' @param quadratic Validated quadratic system with `A_i`, `b_i`, `c_i`
#' @param certificate Boundedness certificate with `weights` and `scales`, or
#'   `NULL`
#' @param maxit Iteration budget for each per-side candidate search
#' @param nonempty TRUE when a checked point or tail proves the set nonempty.
#'   A verified combination proving the same set empty is then a certificate
#'   conflict and raises a structured error instead of returning bounds.
#' @return Function `outer_bounds(objectives, refine = TRUE, pool = NULL)`.
#'   It returns a data frame with `lower` and `upper`, one row per objective
#'   column. Exactly zero loadings give exact zeros, and unknown sides are `NA`.
#'   Attributes: `pool` (verified simplex weights on the rescaled rows, to pass
#'   back), `sources` (the candidate behind each side), `candidates` (per
#'   candidate error terms), `empty` (TRUE when a verified combination proves
#'   the set empty) and `reason` (why every bound is unknown, or `NULL`).
#' @noRd
quadratic_outer_bounder <- function(quadratic, certificate,
                                    maxit = HETID_CONSTANTS$QUADRATIC_EVIDENCE_MAXIT,
                                    nonempty = FALSE) {
  assert_scalar_integer_in_range(maxit, "maxit", 0, .Machine$integer.max)
  assert_flag(nonempty, "nonempty")
  dimension <- nrow(quadratic$A_i[[1L]])
  sys <- outer_normalize_system(quadratic)
  common <- outer_common_candidate(sys, certificate)
  reason <- outer_unknown_reason(sys, certificate, common)
  outer_assert_consistent(common, nonempty)
  maxit <- as.integer(maxit)
  function(objectives, refine = TRUE, pool = NULL) {
    outer_validate_request(objectives, dimension, refine, pool)
    out <- outer_bounds_evaluate(sys, common, reason, objectives, refine, pool, maxit)
    if (nonempty) outer_assert_consistent(list(empty = attr(out, "empty")), TRUE)
    out
  }
}

outer_common_candidate <- function(sys, certificate) {
  if (is.null(certificate) || is.null(sys)) {
    return(NULL)
  }
  v0 <- outer_certificate_weights(certificate, sys)
  if (is.null(v0)) NULL else outer_verify(sys, v0)
}

outer_unknown_reason <- function(sys, certificate, common) {
  if (is.null(certificate)) {
    return("no positive definite certificate")
  }
  if (is.null(sys)) {
    return("row rescaling would lose a nonzero coefficient")
  }
  if (is.null(common)) {
    return("certificate weights failed outer verification")
  }
  NULL
}

outer_assert_consistent <- function(candidate, nonempty) {
  if (nonempty && isTRUE(candidate$empty)) {
    stop_hetid(paste(
      "Quadratic geometry certificates conflict: a checked point or tail proves",
      "the set nonempty, but a verified combination proves it empty"
    ))
  }
  invisible(TRUE)
}

outer_validate_request <- function(objectives, dimension, refine, pool) {
  assert_bad_argument_ok(
    is.matrix(objectives) && is.numeric(objectives) &&
      nrow(objectives) == dimension && ncol(objectives) > 0L &&
      all(is.finite(objectives)),
    "objectives must be a finite matrix with one row per coordinate",
    arg = "objectives"
  )
  assert_flag(refine, "refine")
  assert_bad_argument_ok(
    is.null(pool) || (is.list(pool) &&
      all(vapply(pool, function(v) is.numeric(v) && is.null(dim(v)), logical(1)))),
    "pool must be NULL or a list of numeric weight vectors",
    arg = "pool"
  )
}

outer_bounds_evaluate <- function(sys, common, reason, objectives, refine, pool, maxit) {
  zero <- colSums(objectives != 0) == 0L
  bounds <- list(
    lower = ifelse(zero, 0, NA_real_), upper = ifelse(zero, 0, NA_real_),
    src_lower = ifelse(zero, 0L, NA_integer_), src_upper = ifelse(zero, 0L, NA_integer_)
  )
  cands <- if (is.null(common)) {
    list()
  } else {
    c(
      list(common),
      Filter(Negate(is.null), lapply(pool, function(v) outer_verify(sys, v)))
    )
  }
  empty <- any(vapply(cands, `[[`, logical(1), "empty"))
  scaled <- outer_scale_objectives(objectives, zero)
  if (length(cands) && !empty && length(scaled$active)) {
    best <- outer_best_bounds(cands, scaled$matrix)
    if (refine && maxit > 0L) best <- outer_refine_sides(sys, best, scaled$matrix, maxit)
    bounds <- outer_rescale_bounds(bounds, best, scaled)
    cands <- best$cands
  }
  unknown <- !zero & (!is.finite(bounds$lower) | !is.finite(bounds$upper))
  bounds$lower[unknown] <- bounds$upper[unknown] <- NA_real_
  bounds$src_lower[unknown] <- bounds$src_upper[unknown] <- NA_integer_
  if (empty) reason <- "a verified combination proves the set empty"
  outer_bounds_result(bounds, cands, empty, reason)
}
