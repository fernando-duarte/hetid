#' Constraint Factory for Quadratic Form Evaluation
#'
#' Factories that validate quadratic coefficients and return constraint evaluators.
#'
#' @name constraint_factory
#' @keywords internal
NULL

#' Create Constraint Checker from Quadratic Components
#'
#' Factory function that captures quadratic coefficients and returns a
#' closure for efficiently evaluating the quadratic constraint
#' at many theta values (e.g., grid search or optimisation).
#' Inputs are validated once at factory time; the returned closure
#' performs no checks.
#'
#' @param A_i Square symmetric numeric matrix from quadratic computation.
#' @param b_i Numeric coefficient vector from quadratic computation,
#'   with length equal to \code{nrow(A_i)}.
#' @param c_i Numeric scalar constant from quadratic computation.
#'
#' @return A function taking a numeric \code{theta} vector of length
#'   \code{nrow(A_i)} and returning the numeric scalar
#'   \eqn{\theta' A_i \theta + b_i' \theta + c_i}. Non-positive
#'   values satisfy the constraint, including equality at its boundary.
#'
#' @details Invalid coefficient types or a nonsymmetric matrix signal
#'   a \code{hetid_error_bad_argument}; a coefficient-vector length mismatch
#'   signals a \code{hetid_error_dimension_mismatch}. Missing and non-finite
#'   coefficients are not explicitly rejected or removed, and missing values
#'   propagate to the result. Supply finite coefficients and candidates for
#'   membership checks. The closure does not validate \code{theta}; invalid
#'   inputs can produce ordinary R arithmetic errors or non-finite results.
#'
#' @export
#'
#' @examples
#' A <- matrix(c(1, 0, 0, 1), 2, 2)
#' b <- c(-2, -2)
#' c_val <- 0.5
#' check <- make_constraint_checker(A, b, c_val)
#' check(c(0.5, 0.5))
#' check(c(0.5, 0.5)) <= 0
make_constraint_checker <- function(A_i, b_i, c_i) { # nolint: object_name_linter.
  assert_bad_argument_ok(
    is.matrix(A_i) && is.numeric(A_i) && nrow(A_i) == ncol(A_i),
    "A_i must be a square numeric matrix",
    arg = "A_i"
  )
  assert_bad_argument_ok(
    isSymmetric(A_i),
    "A_i must be a symmetric matrix",
    arg = "A_i"
  )
  assert_bad_argument_ok(
    is.numeric(b_i) && is.null(dim(b_i)),
    "b_i must be a numeric vector",
    arg = "b_i"
  )
  assert_dimension_ok(
    length(b_i) == nrow(A_i),
    paste0(
      "b_i must have length nrow(A_i) = ", nrow(A_i),
      "; got length ", length(b_i)
    )
  )
  assert_bad_argument_ok(
    is.numeric(c_i) && length(c_i) == 1 && is.null(dim(c_i)),
    "c_i must be a numeric scalar",
    arg = "c_i"
  )
  function(theta) {
    as.numeric(crossprod(theta, A_i %*% theta)) +
      sum(b_i * theta) + c_i
  }
}

#' Build a Checker Over Every Constraint of a Quadratic System
#'
#' Companion to \code{\link{make_constraint_checker}} for
#' multi-constraint systems: takes the \code{quadratic} element
#' produced by \code{\link{build_general_quadratic_system}} (or the
#' \code{\link{build_quadratic_system}}) and returns a closure
#' evaluating every constraint at a candidate theta. Following the
#' package's \code{hin <= 0} convention, theta lies inside the
#' identified set exactly when every returned value is non-positive
#' (\code{all(values <= 0)}). A system where no theta satisfies all
#' constraints is an empty estimated set; this checker is the
#' intended tool for probing that case on a grid. Failure to find a feasible
#' grid point does not establish emptiness.
#'
#' @param quadratic List containing \code{A_i} and \code{b_i} lists and a
#'   numeric \code{c_i} vector, all of equal length, as in either builder's
#'   \code{quadratic} element. Each position supplies the coefficients for
#'   one \code{\link{make_constraint_checker}}; entries are paired by position,
#'   so their order must agree even when names are supplied.
#' @return A function taking a numeric \code{theta} vector conformable with
#'   every coefficient matrix and returning one numeric value per constraint,
#'   in input order. Names are copied from \code{quadratic$A_i} when present;
#'   unnamed inputs return an unnamed vector. Empty parallel inputs return
#'   \code{numeric(0)}, imposing no constraints.
#'
#' @details Invalid parallel structure signals a
#'   \code{hetid_error_bad_argument}. Invalid individual coefficients re-signal the
#'   \code{\link{make_constraint_checker}} condition unchanged, except that its message is
#'   prefixed with the constraint's name or position.
#'   The returned closure does not validate \code{theta}; the missing-value
#'   and finite-input caveats of \code{\link{make_constraint_checker}} apply.
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   w1 <- rnorm(50)
#'   w2 <- matrix(rnorm(100), nrow = 50)
#'   z <- matrix(rnorm(150), nrow = 50)
#'   moments <- compute_identification_moments(w1, w2, z)
#'   qs <- build_general_quadratic_system(
#'     separate_instruments_lambda(moments), 0.2, moments
#'   )
#'   check_all <- make_system_checker(qs$quadratic)
#'   values <- check_all(c(0, 0))
#'   print(values)
#'   all(values <= 0)
#' })
make_system_checker <- function(quadratic) {
  assert_bad_argument_ok(
    is_parallel_quadratic(quadratic),
    paste0(
      "quadratic must carry parallel A_i, b_i, c_i elements, e.g. ",
      "the quadratic element of build_general_quadratic_system()"
    ),
    arg = "quadratic"
  )
  nms <- names(quadratic[["A_i"]])
  checkers <- lapply(seq_along(quadratic[["A_i"]]), function(k) {
    slot_label <- if (is.null(nms) || !nzchar(nms[k])) k else nms[k]
    tryCatch(
      make_constraint_checker(
        quadratic[["A_i"]][[k]], quadratic[["b_i"]][[k]],
        quadratic[["c_i"]][[k]]
      ),
      hetid_error = function(e) {
        e$message <- paste0("constraint ", slot_label, ": ", conditionMessage(e))
        stop(e)
      }
    )
  })
  names(checkers) <- nms
  function(theta) {
    vapply(checkers, function(f) as.numeric(f(theta)), numeric(1))
  }
}

#' Quadratic List Carries Parallel A/b/c Elements
#'
#' Checks that \code{A_i} and \code{b_i} are lists and \code{c_i} is numeric,
#' with matching lengths. Individual coefficients are validated separately
#' by \code{make_constraint_checker()}.
#'
#' @param quadratic Candidate quadratic list.
#' @return Logical scalar indicating whether the elements have parallel lengths.
#' @noRd
is_parallel_quadratic <- function(quadratic) {
  if (!is.list(quadratic)) {
    return(FALSE)
  }
  all(c(
    is.list(quadratic[["A_i"]]),
    is.list(quadratic[["b_i"]]),
    is.numeric(quadratic[["c_i"]]),
    length(quadratic[["A_i"]]) == length(quadratic[["b_i"]]),
    length(quadratic[["A_i"]]) == length(quadratic[["c_i"]])
  ))
}
