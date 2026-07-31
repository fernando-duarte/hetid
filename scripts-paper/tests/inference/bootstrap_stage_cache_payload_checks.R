bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
failed <- PAPER_ENDPOINT_STATUS[["failed"]]
cell_matrix <- function(value, type = "double") {
  out <- matrix(value, nrow = 1L, dimnames = list(NULL, "coef"))
  storage.mode(out) <- type
  out
}
mixed_cell <- list(
  lower = cell_matrix(NA_real_),
  upper = cell_matrix(1),
  lower_status = cell_matrix(failed, "character"),
  upper_status = cell_matrix(bounded, "character")
)
# The tau = 0 slot is a direct point evaluation whose two sides are exact copies
# of it; every later slot keeps the two searched sides on their own.
point_cell <- list(
  lower = cell_matrix(0.5),
  upper = cell_matrix(0.5),
  lower_status = cell_matrix(bounded, "character"),
  upper_status = cell_matrix(bounded, "character"),
  point = cell_matrix(0.5),
  point_status = cell_matrix(bounded, "character")
)
logvar_validate <- function(cells) {
  logvar_boot_collection_validate(
    list(ppml = cells),
    0L,
    list(estimator_ids = "ppml", coefs = "coef"),
    c(0, 0.05),
    1L,
    list(fatal_failure_share = 0.75),
    TRUE
  )
}
stopifnot(isTRUE(logvar_validate(list(point_cell, mixed_cell))))
missing_point <- point_cell[c("lower", "upper", "lower_status", "upper_status")]
stopifnot(!isTRUE(logvar_validate(list(missing_point, mixed_cell))))
drifted_point <- point_cell
drifted_point$upper <- cell_matrix(0.5 + 1e-12)
stopifnot(!isTRUE(logvar_validate(list(drifted_point, mixed_cell))))
unbounded_point <- point_cell
unbounded_point$point_status <- cell_matrix("unbounded", "character")
unbounded_point$lower_status <- unbounded_point$point_status
unbounded_point$upper_status <- unbounded_point$point_status
unbounded_point$point <- cell_matrix(NA_real_)
unbounded_point$lower <- unbounded_point$point
unbounded_point$upper <- unbounded_point$point
stopifnot(!isTRUE(logvar_validate(list(unbounded_point, mixed_cell))))

mean_cell <- lapply(
  list(
    lower = rbind(c(0), c(0)),
    upper = rbind(c(1), c(1)),
    lower_status = rbind(c(bounded), c(bounded)),
    upper_status = rbind(c(bounded), c(bounded))
  ),
  function(value) {
    colnames(value) <- "coef"
    value
  }
)
mean_collected <- list(
  point_draws = mean_cell$lower,
  point_status = mean_cell$lower_status,
  n_point_deficient = 0L,
  endpoint_draws = list(mean_cell),
  n_failed = 0L,
  failure_causes = NULL
)
mean_args <- list(
  mean_spec = list(coefs = "coef"),
  display_taus = 0,
  n_draws = 2L,
  failure_control = list(fatal_failure_share = 0.75)
)
stopifnot(isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = mean_collected), mean_args)
)))
# n_failed is now the only recorded copy of what the status mask already says,
# so the cross-check that they agree is the one that has to stay live
bad_failed <- mean_collected
bad_failed$n_failed <- 1L
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = bad_failed), mean_args)
)))
bad_causes <- mean_collected
bad_causes$failure_causes <- table("forged")
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = bad_causes), mean_args)
)))
failed_mean <- mean_collected
failed_mean$point_draws[1L, ] <- NA_real_
failed_mean$point_status[1L, ] <- failed
failed_mean$endpoint_draws[[1L]]$lower[1L, ] <- NA_real_
failed_mean$endpoint_draws[[1L]]$upper[1L, ] <- NA_real_
failed_mean$endpoint_draws[[1L]]$lower_status[1L, ] <- failed
failed_mean$endpoint_draws[[1L]]$upper_status[1L, ] <- failed
failed_mean$n_failed <- 1L
failed_mean$failure_causes <- table("fixture failure")
stopifnot(isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = failed_mean), mean_args)
)))
failed_mean_with_point <- failed_mean
failed_mean_with_point$point_draws[1L, ] <- 0
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = failed_mean_with_point), mean_args)
)))
# A draw certified on one side only keeps that side's value, so the value and
# status must reconcile SIDE BY SIDE rather than under one shared mask.
sided_mean <- mean_collected
sided_mean$endpoint_draws[[1L]]$upper[1L, ] <- NA_real_
sided_mean$endpoint_draws[[1L]]$upper_status[1L, ] <-
  PAPER_ENDPOINT_STATUS[["unbounded"]]
stopifnot(isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = sided_mean), mean_args)
)))
crossed_mean <- sided_mean
crossed_mean$endpoint_draws[[1L]]$lower[1L, ] <- NA_real_
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = crossed_mean), mean_args)
)))
split_failure_mean <- failed_mean
split_failure_mean$endpoint_draws[[1L]]$upper_status[1L, ] <-
  PAPER_ENDPOINT_STATUS[["unbounded"]]
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = split_failure_mean), mean_args)
)))
# a point evaluation cannot diverge
unbounded_point_mean <- mean_collected
unbounded_point_mean$point_status[1L, ] <-
  PAPER_ENDPOINT_STATUS[["unbounded"]]
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = unbounded_point_mean), mean_args)
)))
unreliable_point_mean <- mean_collected
unreliable_point_mean$point_status[1L, ] <-
  PAPER_ENDPOINT_STATUS[["unreliable"]]
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = unreliable_point_mean), mean_args)
)))
