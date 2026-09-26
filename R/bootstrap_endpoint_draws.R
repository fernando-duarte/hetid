#' Run a Serial Endpoint Bootstrap Over Precomputed Indices
#'
#' @param full Full-sample endpoint frame accepted by [bootstrap_set_interval()].
#' @param indices Nonempty list of equally long numeric index vectors, using
#'   whole numbers from one through their common length. Optional list names
#'   provide unique draw identifiers.
#' @param statistic Function of `(index, draw_id)` returning an endpoint frame
#'   with the same coefficient names and order as `full`. `draw_id` is the
#'   position in `indices`. Extra frame columns are retained in `results`.
#'   The callback must raise numerical failures as errors or return them with
#'   `failed` statuses and missing endpoints.
#' @param seed Optional integer seed. The caller's RNG kind is used.
#' @details Callback errors become failed draws with missing endpoints and a
#'   retained message and condition classes. A returned malformed frame, including
#'   `NULL`, is a contract error and stops the run. Returned per-side failures are
#'   preserved independently. The original full-sample evidence, indices and raw
#'   successful callback results are retained.
#'
#'   All input data must already share the same date-aligned estimation sample;
#'   this function does not align, join, or preprocess data. It runs serially and
#'   owns no checkpointing, cache, parallel backend or progress policy.
#'   RNG kind and `.Random.seed`, including its absence, are restored on exit,
#'   with or without `seed`. R's hidden Box-Muller normal cache is not restored.
#'   Reproducibility requires the same input and RNG kind; arbitrary stochastic
#'   callbacks need not agree with a parallel execution.
#' @return A list containing the four paired matrices consumed directly by
#'   [bootstrap_set_interval()], plus `full`, `indices`, `results`, `errors`,
#'   `callback_failed`, and `n_callback_failed`. Callback failure counts do not
#'   include valid returned frames whose endpoint statuses are `failed`.
#' @export
#' @examples
#' y <- seq_len(20)
#' statistic <- function(index, draw_id) {
#'   center <- mean(y[index])
#'   data.frame(
#'     coef = "mean", lower = center, upper = center,
#'     lower_status = "bounded", upper_status = "bounded"
#'   )
#' }
#' full <- statistic(seq_along(y), 0)
#' set.seed(17)
#' draws <- bootstrap_endpoint_draws(full, circular_mbb_indices(20, 3, 20), statistic)
#' bootstrap_set_interval(full, draws, "containment", 0.1, 10, 0.8)$summary
bootstrap_endpoint_draws <- function(full, indices, statistic, seed = NULL) {
  validate_bootstrap_full(full)
  validate_bootstrap_indices(indices)
  assert_bad_argument_ok(is.function(statistic), "statistic must be a function", arg = "statistic")
  if (!is.null(seed)) assert_scalar_integer_in_range(seed, "seed", 0, .Machine$integer.max)
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved), add = TRUE)
  if (!is.null(seed)) set.seed(seed)
  n <- length(indices)
  axes <- list(names(indices), full$coef)
  missing_endpoints <- matrix(NA_real_, n, nrow(full), dimnames = axes)
  failed <- matrix("failed", n, nrow(full), dimnames = axes)
  out <- list(
    lower = missing_endpoints, upper = missing_endpoints,
    lower_status = failed, upper_status = failed
  )
  results <- errors <- vector("list", n)
  names(results) <- names(errors) <- names(indices)
  callback_failed <- rep(FALSE, n)
  for (id in seq_len(n)) {
    result <- tryCatch(list(value = statistic(indices[[id]], id)),
      error = function(error) {
        list(error = list(
          message = conditionMessage(error),
          classes = class(error)
        ))
      }
    )
    if (!is.null(result$error)) {
      errors[id] <- list(result$error)
      callback_failed[id] <- TRUE
      next
    }
    value <- result$value
    tryCatch(
      {
        validate_bootstrap_full(value)
        assert_bad_argument_ok(
          identical(value$coef, full$coef),
          "callback coefficient names must match full exactly and in order"
        )
      },
      hetid_error = function(error) {
        stop_bad_argument(paste0("statistic result for draw ", id, ": ", conditionMessage(error)),
          arg = "statistic"
        )
      }
    )
    results[id] <- list(value)
    for (field in names(out)) out[[field]][id, ] <- value[[field]]
  }
  c(out, list(
    full = full, indices = indices, results = results, errors = errors,
    callback_failed = callback_failed, n_callback_failed = sum(callback_failed)
  ))
}
