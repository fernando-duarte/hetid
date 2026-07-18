# Deterministic box-search fixtures for the shared bound classifier.

zero_quadratic <- list(
  A_i = list(matrix(0, nrow = 2L, ncol = 2L)),
  b_i = list(c(0, 0)),
  c_i = 0
)
profile_boxes <- c(10, 100, 1000)
profile_finalize <- function(result) {
  list(
    bound = result$phi[[1L]],
    bounded = TRUE,
    valid = TRUE
  )
}
run_profile_fixture <- function(
  quadratic,
  solve_box,
  value_at = function(result) result$phi[[1L]],
  finalize = profile_finalize,
  trusted_at = function(result, box) TRUE,
  target_edge_is_unbounded = FALSE
) {
  .classify_profile_search(
    quadratic = quadratic,
    delta = 1,
    omega = 1,
    boxes = profile_boxes,
    solve_box = solve_box,
    value_at = value_at,
    finalize = finalize,
    trusted_at = trusted_at,
    unbounded_value = Inf,
    target_edge_is_unbounded = target_edge_is_unbounded,
    feasibility_tolerance = 1e-8
  )
}

interior_calls <- 0L
interior <- run_profile_fixture(
  zero_quadratic,
  function(box) {
    interior_calls <<- interior_calls + 1L
    list(phi = c(2, 3), convergence = 0L)
  }
)
check(
  "profile classifier accepts a first-box interior solution",
  interior_calls == 1L &&
    identical(interior, list(bound = 2, bounded = TRUE, valid = TRUE))
)

coordinate_edge <- run_profile_fixture(
  zero_quadratic,
  function(box) list(phi = c(box, 0), convergence = 0L),
  trusted_at = function(result, box) FALSE,
  target_edge_is_unbounded = TRUE
)
check(
  "profile classifier marks a coordinate riding the final edge as infinite",
  is.infinite(coordinate_edge$bound) &&
    !coordinate_edge$bounded &&
    coordinate_edge$valid
)

stable_functional <- run_profile_fixture(
  zero_quadratic,
  function(box) {
    first <- if (box == profile_boxes[[1L]]) 1 else 2
    if (box == profile_boxes[[3L]]) first <- 2.0001
    list(phi = c(first, box), convergence = 0L)
  }
)
check(
  "profile classifier accepts a stable functional while another coordinate edges",
  stable_functional$bounded &&
    stable_functional$valid &&
    isTRUE(all.equal(stable_functional$bound, 2.0001))
)

growing_functional <- run_profile_fixture(
  zero_quadratic,
  function(box) {
    value <- if (box == profile_boxes[[1L]]) 1 else box * 0.6
    list(phi = c(value, box), convergence = 0L)
  }
)
check(
  "profile classifier marks a growing trusted functional as infinite",
  is.infinite(growing_functional$bound) &&
    !growing_functional$bounded &&
    growing_functional$valid
)

infeasible_quadratic <- zero_quadratic
infeasible_quadratic$b_i[[1L]] <- c(1, 0)
infeasible <- run_profile_fixture(
  infeasible_quadratic,
  function(box) {
    phi <- if (box == profile_boxes[[1L]]) c(box, 0) else c(1, 0)
    list(phi = phi, convergence = 0L)
  }
)
check(
  "profile classifier rejects enlarged-box infeasibility",
  is.na(infeasible$bound) &&
    !infeasible$bounded &&
    !infeasible$valid
)

slack_quadratic <- zero_quadratic
slack_quadratic$c_i <- -1
uncertified <- run_profile_fixture(
  slack_quadratic,
  function(box) {
    phi <- if (box == profile_boxes[[1L]]) c(box, 0) else c(1, 0)
    list(phi = phi, convergence = 0L)
  },
  finalize = function(result) {
    .finalize_bound(
      slack_quadratic,
      1,
      1,
      result,
      1L,
      1e-8
    )
  }
)
check(
  "profile classifier rejects interior but uncertified stability",
  uncertified$bounded &&
    !uncertified$valid &&
    identical(uncertified$bound, 1)
)
