#' Evaluate Code With Scoped Random-Number State
#'
#' Optionally selects a random-number generator and seed, evaluates an expression,
#' and restores the caller's generator and seed on exit.
#'
#' @param code Expression evaluated in the caller's environment after RNG setup.
#' @param seed Optional scalar integer from zero through \code{.Machine$integer.max}.
#'   \code{NULL} (the default) does not call \code{set.seed()}.
#' @param kind Optional character vector of length three, specifying the generator,
#'   normal generator and discrete sampler in \code{\link[base]{RNGkind}} order.
#'   \code{NULL} (the default) retains the caller's RNG kind.
#' @details The generator is selected before setting the seed. To reproduce macro
#'   searches independently of the caller's generator, explicitly supply
#'   \code{c("Mersenne-Twister", "Inversion", "Rejection")} and the required seed.
#'   With both options \code{NULL}, code uses the caller's current random-number stream.
#'
#'   RNG kind and \code{.Random.seed}, including its absence, are restored on normal
#'   return and errors. R's hidden Box-Muller normal cache is not restored.
#'   Warnings from explicit RNG selection and evaluated code are not intercepted.
#'   Restoring the previously selected caller kind does not repeat legacy-generator
#'   warnings, so warning escalation cannot interrupt state cleanup.
#'   Invalid setup arguments raise a \code{hetid_error_bad_argument} condition;
#'   errors from \code{code} propagate unchanged.
#' @return The value of \code{code}, preserving its visibility.
#' @export
#' @examples
#' with_rng_scope(runif(3),
#'   seed = 17,
#'   kind = c("Mersenne-Twister", "Inversion", "Rejection")
#' )
with_rng_scope <- function(code, seed = NULL, kind = NULL) {
  if (!is.null(seed)) assert_scalar_integer_in_range(seed, "seed", 0, .Machine$integer.max)
  if (!is.null(kind)) {
    assert_bad_argument_ok(
      is.character(kind) && is.null(dim(kind)) && length(kind) == 3L &&
        !anyNA(kind) && all(nzchar(kind)),
      "kind must be NULL or three nonmissing, nonempty RNG names",
      arg = "kind"
    )
  }
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved), add = TRUE)
  if (!is.null(kind)) {
    tryCatch(do.call(RNGkind, as.list(unname(kind))),
      error = function(error) stop_bad_argument(conditionMessage(error), arg = "kind")
    )
  }
  if (!is.null(seed)) set.seed(seed)
  force(code)
}
