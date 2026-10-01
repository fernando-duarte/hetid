#' Run a Serial Endpoint Bootstrap Over Precomputed Indices
#'
#' Evaluates an endpoint statistic for each supplied resample and collects paired
#' endpoints, per-side statuses, and callback failure evidence for interval calibration.
#'
#' @param full Nonempty full-sample endpoint data frame accepted by
#'   \code{\link{bootstrap_set_interval}}, with unique character coefficient names.
#' @param indices Nonempty list of equally long numeric index vectors, using
#'   finite whole numbers from one through their common positive length.
#'   Missing indices are not allowed. Optional list names must be unique, nonempty,
#'   and nonmissing.
#' @param statistic Function of \code{(index, draw_id)} returning an endpoint frame
#'   with the same coefficient names and order as \code{full}. \code{draw_id} is the
#'   position in \code{indices}. Extra frame columns are retained in \code{results}.
#'   The callback must raise numerical failures as errors or return them with
#'   \code{failed} statuses and missing endpoints.
#' @param seed Optional scalar integer from zero through \code{.Machine$integer.max},
#'   or \code{NULL} (the default) to use the caller's current random-number state.
#'   The caller's RNG kind is used. This seeds callbacks, not index generation.
#' @details Callback errors become failed draws with missing endpoints and a
#'   retained message and condition classes. A returned malformed frame, including
#'   \code{NULL}, is a contract error and stops the run with a \code{hetid_error}
#'   condition. Callback warnings are not intercepted. Returned per-side failures are
#'   preserved independently. The original full-sample evidence, indices and raw
#'   successful callback results are retained.
#'
#'   All input data must already share the same date-aligned estimation sample;
#'   this function does not align, join, or preprocess data. It runs serially and
#'   owns no checkpointing, cache, parallel backend or progress policy.
#'   RNG kind and \code{.Random.seed}, including its absence, are restored on exit,
#'   with or without \code{seed}. R's hidden Box-Muller normal cache is not restored.
#'   Reproducibility requires the same input and RNG kind; arbitrary stochastic
#'   callbacks need not agree with a parallel execution.
#' @return A list with numeric \code{lower} and \code{upper} matrices and character
#'   \code{lower_status} and \code{upper_status} matrices, each with
#'   \code{length(indices)} rows and \code{nrow(full)} columns. Row names are
#'   \code{names(indices)}; column names are \code{full$coef}. These matrices are
#'   consumed directly by \code{\link{bootstrap_set_interval}}.
#'   The list also contains the unchanged \code{full} and \code{indices};
#'   \code{results} and \code{errors}, lists named like \code{indices}, with one entry
#'   per draw; the unnamed logical vector \code{callback_failed}; and its sum,
#'   \code{n_callback_failed}. Callback errors leave \code{NULL} results and store
#'   \code{message} and \code{classes} in the corresponding error entry, with
#'   \code{NA_real_} endpoints and \code{failed} statuses. Valid returned frames are
#'   retained in \code{results} with \code{NULL} error entries, even when their
#'   per-side statuses are \code{failed}; these do not count as callback failures.
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
#' draws <- (function() {
#'   had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
#'   if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
#'   on.exit(if (had_seed) {
#'     assign(".Random.seed", old_seed, envir = globalenv())
#'   } else {
#'     rm(".Random.seed", envir = globalenv())
#'   })
#'   set.seed(17)
#'   indices <- circular_mbb_indices(length(y), 3, 20)
#'   bootstrap_endpoint_draws(full, indices, statistic, seed = 17)
#' })()
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
