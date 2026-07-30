# Mean-branch per-side status threading, on deterministic solver stubs so the
# two sides can be made to disagree. The interval tables must carry a status per
# side, the draw must mask each side by its own status, and collect must stack
# four endpoint matrices plus the authoritative tau = 0 point status. The
# production solvers are restored on exit.

mps_with_stubs <- function(stubs, run) {
  original <- mget(names(stubs), envir = globalenv())
  on.exit(list2env(original, envir = globalenv()), add = TRUE)
  list2env(stubs, envir = globalenv())
  run()
}
mps_profile <- data.frame(
  component = 1L, lower = -2, upper = 2, width = 4,
  bounded_lower = TRUE, bounded_upper = FALSE,
  valid_lower = TRUE, valid_upper = TRUE
)
# the "min" solve is unbounded but valid, the "max" solve is bounded but
# invalid, so the two functional sides cannot be confused for one another
mps_functional <- function(quadratic, objective_vec, direction, ...) {
  if (identical(direction, "min")) {
    list(bound = -3, bounded = FALSE, valid = TRUE)
  } else {
    list(bound = 4, bounded = TRUE, valid = FALSE)
  }
}
mps_tables <- mps_with_stubs(
  list(
    solve_all_profile_bounds = function(quadratic, ...) mps_profile,
    solve_linear_functional_bound = mps_functional
  ),
  function() {
    coef_interval_tables_from_quadratic(
      NULL, c(b0 = 5), matrix(1, 1L, 1L, dimnames = list("th1", "b0"))
    )
  }
)
check(
  "theta rows map each profile side's flags straight through",
  identical(mps_tables$theta$lower_status, "bounded") &&
    identical(mps_tables$theta$upper_status, "unbounded")
)
check(
  "beta1 rows take the lower side from fmax and the upper side from fmin",
  identical(mps_tables$beta1$set_lower, 5 - 4) &&
    identical(mps_tables$beta1$set_upper, 5 - -3) &&
    identical(mps_tables$beta1$lower_status, "unreliable") &&
    identical(mps_tables$beta1$upper_status, "unbounded")
)
mps_geometry <- list(
  display_slots = 1L, gamma = matrix(1, 1L, 1L),
  tables = list(list(
    beta1 = data.frame(
      coef = "b0", set_lower = -1, set_upper = 4, status = "unbounded",
      lower_status = "bounded", upper_status = "unbounded",
      stringsAsFactors = FALSE
    ),
    theta = data.frame(
      coef = "th1", set_lower = -2, set_upper = 2, status = "bounded",
      lower_status = "bounded", upper_status = "bounded",
      stringsAsFactors = FALSE
    )
  ))
)
mps_draw <- mps_with_stubs(
  list(
    sweep_fixed_gamma = function(...) data.frame(tau = c(0, 0.1)),
    tau_star_fixed = function(...) list(tau_star = 0.1, capped = FALSE)
  ),
  function() {
    set_id_boot_draw_from_est(
      list(point0 = NULL, moments = NULL), mps_geometry,
      list(
        coefs = c("b0", "th1"), tau_star_grid = c(0, 0.1),
        tau_star_iterations = 2L
      )
    )
  }
)
check(
  "a half-infinite draw row keeps its live side and NA-masks only the dead one",
  identical(mps_draw$bounds[[1]]$lower, c(-1, -2)) &&
    identical(mps_draw$bounds[[1]]$upper, c(NA_real_, 2)) &&
    identical(mps_draw$bounds[[1]]$lower_status, c("bounded", "bounded")) &&
    identical(mps_draw$bounds[[1]]$upper_status, c("unbounded", "bounded"))
)
check(
  "a rank-deficient tau = 0 system is unreliable everywhere, never unbounded",
  identical(mps_draw$point_status, rep("unreliable", 2L)) &&
    all(is.na(mps_draw$point))
)
mps_collected <- set_id_boot_collect(
  list(mps_draw, "solver exploded"),
  list(coefs = c("b0", "th1"), taus = 0.1)
)
check(
  "collect stacks four endpoint matrices and injects failed on a failed draw",
  identical(dim(mps_collected$endpoint_draws[[1]]$upper_status), c(2L, 2L)) &&
    identical(
      unname(mps_collected$endpoint_draws[[1]]$upper_status[1, ]),
      c("unbounded", "bounded")
    ) &&
    all(mps_collected$endpoint_draws[[1]]$lower_status[2, ] == "failed")
)
check(
  "collect stacks an authoritative point status with failed on a failed draw",
  identical(dim(mps_collected$point_status), c(2L, 2L)) &&
    identical(colnames(mps_collected$point_status), c("b0", "th1")) &&
    all(mps_collected$point_status[1, ] == "unreliable") &&
    all(mps_collected$point_status[2, ] == "failed")
)
