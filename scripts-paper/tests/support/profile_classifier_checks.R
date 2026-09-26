# Evidence decides tail status before any finite-box optimizer result.
profile_boxes <- c(10, 100, 1000)
flat_qs <- list(A_i = list(matrix(0, 2, 2)), b_i = list(c(0, 0)), c_i = -1)
flat_evidence <- paper_profile_evidence(flat_qs, diag(2))
interior_calls <- 0L
flat_upper <- .classify_profile_search(
  c(1, 0), "max", profile_boxes,
  function(box) {
    interior_calls <<- interior_calls + 1L
    list(phi = c(2, 3), convergence = 0L)
  }, 1, flat_evidence, 1L,
  candidate_is_endpoint = function(theta) TRUE
)
check(
  "verified infinite tail overrides a local interior stall",
  interior_calls == 0L && identical(flat_upper$bound, Inf) &&
    flat_upper$valid && !flat_upper$bounded
)

ball_qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
ball_evidence <- paper_profile_evidence(ball_qs, diag(2))
ball_lower <- solve_profile_bound(ball_qs, 1L, "min", evidence = ball_evidence)
ball_upper <- solve_profile_bound(ball_qs, 1L, "max", evidence = ball_evidence)
check(
  "unit ball keeps two independently bounded coordinate endpoints",
  ball_lower$valid && ball_upper$valid &&
    ball_lower$bounded && ball_upper$bounded &&
    abs(ball_lower$bound + 1) < 1e-6 && abs(ball_upper$bound - 1) < 1e-6
)
ball_all <- solve_all_profile_bounds(ball_qs, evidence = ball_evidence)
corrections <- attr(ball_all, "profile_corrections")
check(
  "finite profile bounds expose bounded numerical correction diagnostics",
  nrow(corrections) == 4L &&
    identical(sort(unique(corrections$side)), c("max", "min")) &&
    all(is.finite(corrections$movement)) &&
    all(corrections$movement <= PAPER_QUADRATIC_CONTROL$candidate_correction_rtol)
)
zero_bound <- solve_linear_functional_bound(ball_qs, c(0, 0), "max")
check(
  "zero structural loading has a finite zero bound",
  identical(zero_bound$bound, 0) && zero_bound$bounded && zero_bound$valid
)

halfspace <- list(
  A_i = list(matrix(0, 2, 2)), b_i = list(c(1, 0)), c_i = 0
)
half_evidence <- hetid::compute_quadratic_set_evidence(
  halfspace, matrix(c(1, 0), 2L, 1L),
  points = matrix(c(-1, 0), 1L)
)
half_lower <- solve_profile_bound(halfspace, 1L, "min", evidence = half_evidence)
half_upper <- solve_profile_bound(halfspace, 1L, "max", evidence = half_evidence)
check(
  "halfspace preserves an infinite lower side and finite upper side",
  identical(half_lower$bound, -Inf) && half_lower$valid && !half_lower$bounded &&
    half_upper$valid && half_upper$bounded && abs(half_upper$bound) < 1e-6
)

near_boundary <- profile_checked_candidate(ball_evidence, c(1 + 1e-9, 0))
check(
  "finite candidate contracts into strict membership by a recorded small amount",
  !is.null(near_boundary) && ball_evidence$check_point(near_boundary$theta) &&
    near_boundary$contraction > 0 && near_boundary$contraction < 1e-6
)
check(
  "distant infeasible solver output cannot become a finite endpoint",
  is.null(profile_checked_candidate(ball_evidence, c(100, 0)))
)

unknown_evidence <- hetid::compute_quadratic_set_evidence(
  ball_qs, diag(2),
  n_dir = 0L, maxit = 0L
)
for (value in c(2, 1e6)) {
  unknown <- .classify_profile_search(
    c(1, 0), "max", profile_boxes,
    function(box) list(phi = c(value, box), convergence = 0L),
    1, unknown_evidence, 1L,
    candidate_is_endpoint = function(theta) TRUE
  )
  check(
    sprintf("finite box pattern %g does not certify unresolved geometry", value),
    is.na(unknown$bound) && !unknown$valid && !unknown$bounded
  )
}

thin_qs <- list(
  A_i = list(diag(c(1, 1e-200))), b_i = list(c(0, 0)), c_i = -1
)
thin_anchor <- c(1 - 1e-9, 0)
thin_evidence <- paper_profile_evidence(thin_qs, diag(2),
  points = matrix(thin_anchor, 1L)
)
thin_evidence$feasible_points <- matrix(thin_anchor, 1L)
thin_candidate <- c(1 + 1e-9, 0)
thin_fixed <- profile_checked_candidate(thin_evidence, thin_candidate)
check(
  "thin-ball candidate uses displacement cap even when contraction is large",
  !thin_evidence$check_point(thin_candidate) &&
    thin_evidence$check_point(thin_anchor) && !is.null(thin_fixed) &&
    thin_evidence$check_point(thin_fixed$theta) &&
    thin_fixed$contraction > PAPER_QUADRATIC_CONTROL$candidate_correction_rtol &&
    thin_fixed$movement <= PAPER_QUADRATIC_CONTROL$candidate_correction_rtol &&
    max(abs(thin_fixed$theta - thin_candidate)) < 1e-6
)

coarse <- data.frame(
  tau = c(0, .1, .2), status = c("bounded", "bounded", "unbounded")
)
local({
  local_env <- new.env(parent = environment(tau_star_fixed))
  local_env$eval_width_at_tau <- function(...) {
    list(total = NA_real_, bounded = FALSE, valid = FALSE, status = "unreliable")
  }
  evaluate <- tau_star_fixed
  environment(evaluate) <- local_env
  out <- evaluate(NULL, NULL, coarse, iters = 4L)
  check(
    "unresolved midpoint remains an explicit tau bracket",
    identical(out$tau_star, .1) && identical(out$bracket$lower, .1) &&
      identical(out$bracket$upper, .2) &&
      isTRUE(all.equal(out$bracket$inconclusive, .15)) &&
      identical(out$bracket$status, "unresolved_band") && !out$capped
  )
})
coarse$status[3L] <- "unreliable"
above <- tau_star_fixed(NULL, NULL, coarse, iters = 0L)
check(
  "no found tail gives an unresolved upper bracket, not a cap",
  identical(above$tau_star, .1) && is.na(above$bracket$upper) &&
    identical(above$bracket$status, "unresolved_above") && !above$capped
)
coarse$status[3L] <- "bounded"
cap <- tau_star_fixed(NULL, NULL, coarse, iters = 0L)
check(
  "all certified bounded sweep points permit a stated sweep cap",
  cap$capped && identical(cap$tau_star, .2)
)
