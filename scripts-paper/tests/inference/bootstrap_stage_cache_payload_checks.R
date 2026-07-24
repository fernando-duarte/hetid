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
mixed_result <- logvar_boot_collection_validate(
  list(ppml = list(mixed_cell)),
  0L,
  list(estimator_ids = "ppml", coefs = "coef"),
  0,
  1L,
  list(fatal_failure_share = 0.75),
  TRUE
)
stopifnot(isTRUE(mixed_result))

mean_cell <- list(
  lower = rbind(c(0), c(0)),
  upper = rbind(c(1), c(1)),
  status = rbind(c(bounded), c(bounded))
)
colnames(mean_cell$lower) <- "coef"
colnames(mean_cell$upper) <- "coef"
colnames(mean_cell$status) <- "coef"
mean_collected <- list(
  point_draws = mean_cell$lower,
  n_point_deficient = 0L,
  endpoint_draws = list(mean_cell),
  tau_star_draws = c(1, 0.5),
  n_capped = 1L,
  n_failed = 0L,
  failure_causes = NULL
)
mean_args <- list(
  mean_spec = list(coefs = "coef", tau_star_grid = c(0, 1)),
  display_taus = 0,
  n_draws = 2L,
  failure_control = list(fatal_failure_share = 0.75)
)
stopifnot(isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = mean_collected), mean_args)
)))
bad_capped <- mean_collected
bad_capped$n_capped <- 0L
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = bad_capped), mean_args)
)))
bad_causes <- mean_collected
bad_causes$failure_causes <- table("forged")
stopifnot(!isTRUE(do.call(
  mean_boot_collection_validate,
  c(list(collected = bad_causes), mean_args)
)))
failed_mean <- mean_collected
failed_mean$point_draws[1L, ] <- NA_real_
failed_mean$endpoint_draws[[1L]]$lower[1L, ] <- NA_real_
failed_mean$endpoint_draws[[1L]]$upper[1L, ] <- NA_real_
failed_mean$endpoint_draws[[1L]]$status[1L, ] <- failed
failed_mean$tau_star_draws <- c(NA_real_, 0.5)
failed_mean$n_capped <- 0L
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
