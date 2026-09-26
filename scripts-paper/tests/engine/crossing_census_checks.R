# Crossing-census soundness: outer no-crossing verdicts, attained crossings,
# fail-closed unresolved rows, and one candidate pool per census call. Needs
# `check` and the production profile, containing-box, and census layers.

cc_ball <- list(
  A_i = list(diag(3), diag(3)), b_i = list(numeric(3), numeric(3)),
  c_i = c(-1, -4)
)
cc_box <- function(qs) {
  out <- profile_containing_bounds(paper_profile_evidence(qs, diag(3)), 3L)
  list(lower = out$outer_lower, upper = out$outer_upper)
}
cc_with_solver <- function(solver, code) {
  old <- get("solve_linear_functional_bound", envir = globalenv())
  on.exit(assign("solve_linear_functional_bound", old, envir = globalenv()), add = TRUE)
  assign("solve_linear_functional_bound", solver, envir = globalenv())
  force(code)
}
# every functional range stalls at half its true extent, as a local optimizer can
cc_stalled <- function(quadratic, objective_vec, direction, ...) {
  reach <- 0.5 * sqrt(sum(objective_vec^2))
  list(bound = if (direction == "min") -reach else reach, bounded = TRUE, valid = TRUE)
}

cc_w2 <- rbind(c(1, 0, 0), c(1, 0, 0), c(1, 1, 0), c(1, 1, 0), c(0, 0, 0), c(0, 0, 0))
cc_w1 <- c(0.5, 2, 1.2, 1.5, 0, 0.3)
check("simple outer geometry: crossings, box and outer clears, zero rows", {
  box <- cc_box(cc_ball)
  census <- logvar_crossing_census(cc_ball, box$lower, box$upper, cc_w1, cc_w2)
  identical(census$cross, c(1L, 3L, 5L)) && !length(census$unresolved)
})

check("stalled functional: the sound census never clears a row inside the set", {
  box <- cc_box(cc_ball)
  census <- cc_with_solver(cc_stalled, logvar_crossing_census(
    cc_ball, box$lower, box$upper, c(0.8, 0.8), rbind(c(1, 0, 0), c(0, 1, 0))
  ))
  setequal(c(census$cross, census$unresolved), 1:2) && !length(census$cross)
})

check("outer bounds clear rows before any functional solve", {
  calls <- 0L
  failing <- function(...) {
    calls <<- calls + 1L
    list(bound = NA_real_, bounded = FALSE, valid = FALSE)
  }
  box <- cc_box(cc_ball)
  census <- cc_with_solver(failing, logvar_crossing_census(
    cc_ball, box$lower, box$upper, 1.6, matrix(c(1, 1, 0), 1)
  ))
  !length(census$cross) && !length(census$unresolved) && calls == 0L
})

check("without a certificate no row is cleared, so rows fail closed", {
  outside <- list(A_i = list(-diag(3)), b_i = list(numeric(3)), c_i = 1)
  census <- logvar_crossing_census(
    outside, rep(-5, 3), rep(5, 3), c(0.2, 3),
    rbind(c(1, 0, 0), c(0, 1, 1))
  )
  identical(sort(census$unresolved), 1:2) && !length(census$cross)
})

check("one candidate pool per census call, however many rows", {
  counts <- 0L
  base <- paper_profile_evidence
  counting <- function(...) {
    evidence <- base(...)
    inner <- evidence$outer_bounds
    evidence$outer_bounds <- function(...) {
      counts <<- counts + 1L
      inner(...)
    }
    evidence
  }
  set.seed(3)
  w2 <- matrix(rnorm(600), ncol = 3)
  w1 <- rnorm(200, sd = 1.5)
  box <- cc_box(cc_ball)
  assign("paper_profile_evidence", counting, envir = globalenv())
  on.exit(assign("paper_profile_evidence", base, envir = globalenv()), add = TRUE)
  census <- logvar_crossing_census(cc_ball, box$lower, box$upper, w1, w2)
  assign("paper_profile_evidence", base, envir = globalenv())
  reach <- sqrt(rowSums(w2^2))
  truth_cross <- which(abs(w1) <= reach)
  counts <= 3L && setequal(census$cross, truth_cross) && !length(census$unresolved)
})

check("box screen keeps a near-boundary row that cancellation would clear", {
  # x1 + x2 - x3 over this box reaches 1e16 + 1 - (1e16 - 2) = 3, but the
  # floating dot product rounds 1e16 + 1 to 1e16 and returns 2: main's raw
  # screen clears w1 = 2.5 although the exact box range contains it
  lower <- c(1e16 - 4, 0, 1e16 - 2)
  upper <- c(1e16, 1, 1e16)
  w2 <- matrix(c(1, 1, -1), 1)
  raw_max <- drop(pmax(w2, 0) %*% upper + pmin(w2, 0) %*% lower)
  slack <- PAPER_QUADRATIC_CONTROL$crossing_range_rtol * 2.5
  raw_max == 2 && 2.5 > raw_max + slack &&
    !logvar_census_box_clear(lower, upper, 2.5, w2, slack)
})
check("box screen clears nothing on overflow or NA", {
  slack <- c(1e-8, 1e-8)
  !any(logvar_census_box_clear(
    c(-1e308, 0), c(1e308, 1), c(1e300, 5),
    rbind(c(10, 0), c(NA, 1)), slack
  ))
})
check("accessor rejects crossed containing bounds and NA statuses", {
  crossed <- cb_tab(outer_lower = c(2, -2), lower = c(3, -2), upper = c(1.5, 2))
  missing <- cb_tab(status = c(PAPER_ENDPOINT_STATUS[["bounded"]], NA))
  cb_error(paper_containing_box(crossed)) && cb_error(paper_containing_box(missing))
})
