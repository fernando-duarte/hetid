#' Compute Basic Components for Identified Set
#'
#' Computes the basic components L_i, V_i, and Q_i for the identified set
#' calculation for each maturity i.
#'
#' @param gamma Finite numeric matrix (J x I) where each column gamma_i contains the
#'   coefficients for system column i. I must equal the moments'
#'   \code{n_components} attribute and J its instrument count. Every constrained
#'   column must contain a nonzero value; unconstrained columns may be zero.
#'   Missing or non-finite values are rejected with a \code{hetid_error}.
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}.
#'
#' @return An object of class \code{hetid_components}: a list (with
#' M = \code{length(maturities)} the active constraint maturities and
#' n_components the theta axis) containing
#' \describe{
#'   \item{L_i}{Named numeric vector of length M (keys maturity_N); element k
#'     is L_i for maturity \code{maturities[k]}.}
#'   \item{V_i}{Named numeric vector of length M (keys maturity_N); element k
#'     is V_i for maturity \code{maturities[k]}.}
#'   \item{Q_i}{Named list of length M (keys maturity_N); element k is the
#'     length-n_components vector Q_i for maturity \code{maturities[k]}, named
#'     \code{maturity_1}, ..., \code{maturity_I}.}
#' }
#' carrying the moments' \code{maturities} and \code{n_components}
#' attributes forward. Arithmetic overflow can produce non-finite entries.
#' \code{print()} shows the theta-axis dimension and active constraint indices,
#' then returns \code{x} invisibly.
#'
#' @details
#' Uses the centered \eqn{1/T} moments in \code{moments}. For each maturity i, computes:
#' \deqn{L_i(\boldsymbol{\Gamma}) = \boldsymbol{\gamma}_i^{\top} \hat{\mathbf{R}}_i^{(0)}}
#' \deqn{V_i(\boldsymbol{\Gamma}) = \boldsymbol{\gamma}_i^{\top}
#'   \left(\hat{\mathbf{P}}_i^{(0)} (\hat{\mathbf{P}}_i^{(0)})^{\top}\right)
#'   \boldsymbol{\gamma}_i}
#' \deqn{\mathbf{Q}_i(\boldsymbol{\Gamma}) = \boldsymbol{\gamma}_i^{\top}
#'   \hat{\mathbf{R}}_i^{(1)} \in \mathbb{R}^I}
#'
#' where \eqn{\boldsymbol{\gamma}_i} is the i-th column of the matrix
#' \eqn{\boldsymbol{\Gamma}}, \eqn{\hat{\mathbf{R}}_i^{(0)}} is a vector,
#' \eqn{\hat{\mathbf{R}}_i^{(1)}} is a matrix, and \eqn{\hat{\mathbf{P}}_i^{(0)}}
#' is a vector.
#'
#' @template section-maturity-convention
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv)
#'   on.exit({
#'     if (is.null(old_seed)) {
#'       rm(".Random.seed", envir = .GlobalEnv)
#'     } else {
#'       assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'     }
#'   })
#'   set.seed(42)
#'   w1 <- rnorm(30)
#'   w2 <- matrix(rnorm(60), ncol = 2)
#'   pcs <- matrix(rnorm(30), ncol = 1)
#'   gamma <- matrix(c(0, 1), nrow = 1)
#'   moments <- compute_identification_moments(w1, w2, pcs, maturities = 2)
#'   components <- compute_identified_set_components(gamma, moments)
#'   print(components$Q_i)
#'   print(components)
#' })
compute_identified_set_components <- function(gamma, moments) {
  validate_hetid_moments(moments)
  assert_bad_argument_ok(
    is.matrix(gamma),
    "gamma must be a matrix",
    arg = "gamma"
  )
  assert_numeric_finite_values(gamma, "gamma")

  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  j_rows <- nrow(moments$r_i_0)

  assert_dimension_ok(
    ncol(gamma) == n_components,
    paste0(
      "gamma must have n_components (", n_components,
      ") columns to match the moments' system"
    )
  )
  assert_dimension_ok(
    nrow(gamma) == j_rows,
    paste0(
      "gamma must have the same number of rows (J = ", j_rows,
      ") as the moments' instruments"
    )
  )

  assert_gamma_columns_nonzero(gamma, maturities)

  n_maturities <- length(maturities)

  L_i <- numeric(n_maturities) # nolint: object_name_linter.
  V_i <- numeric(n_maturities) # nolint: object_name_linter.
  Q_i <- vector("list", n_maturities) # nolint: object_name_linter.

  names(L_i) <- maturity_names(maturities) # nolint: object_name_linter.
  names(V_i) <- maturity_names(maturities) # nolint: object_name_linter.
  names(Q_i) <- maturity_names(maturities) # nolint: object_name_linter.

  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    gamma_i <- gamma[, i, drop = FALSE]

    parts <- constraint_components(gamma_i, idx, moments)
    L_i[idx] <- parts$L # nolint: object_name_linter.
    V_i[idx] <- parts$V # nolint: object_name_linter.
    Q_i[[idx]] <- parts$Q # nolint: object_name_linter.
  }

  components <- new_hetid_components(
    L_i = L_i, V_i = V_i, Q_i = Q_i,
    maturities = maturities, n_components = n_components
  )
  validate_hetid_components(components)
  components
}

#' Construct a hetid_components Object
#'
#' Constructs a component list with maturity and theta-axis attributes.
#' Checks outer types and lengths and coerces valid indices to integers.
#' Does not check names, finiteness, or individual \code{Q_i} entries; call
#' \code{\link{validate_hetid_components}} for the full shape check.
#' The public \code{\link{compute_identified_set_components}} always runs it.
#' For valid containers, outer names must be \code{maturity_N} in maturity order,
#' and each \code{Q_i} entry must be a numeric vector of length \code{n_components}.
#'
#' @param L_i Numeric vector of length \code{length(maturities)} with L_i values.
#' @param V_i Numeric vector of length \code{length(maturities)} with V_i values.
#' @param Q_i List of length \code{length(maturities)} with Q_i vectors.
#' @param maturities Nonempty numeric vector of distinct, finite integer w2 column
#'   indices in \code{1:n_components}; input order is preserved.
#' @param n_components Finite positive integer theta-axis dimension, no larger than
#'   \code{.Machine$integer.max}.
#'
#' @return A \code{hetid_components} list with unchanged \code{L_i}, \code{V_i}, and
#'   \code{Q_i} entries and integer \code{maturities} and \code{n_components} attributes.
#' @keywords internal
new_hetid_components <- function(L_i, V_i, Q_i, # nolint: object_name_linter.
                                 maturities, n_components) {
  assert_scalar_integer_in_range(
    n_components, "n_components", 1, .Machine$integer.max
  )
  n_components <- as.integer(n_components)
  validate_maturities(
    maturities,
    max_value = n_components, max_label = "n_components"
  )
  maturities <- as.integer(maturities)
  n <- length(maturities)
  assert_bad_argument_ok(
    is.numeric(L_i) && length(L_i) == n,
    "L_i must be a numeric vector of length(maturities)",
    arg = "L_i"
  )
  assert_bad_argument_ok(
    is.numeric(V_i) && length(V_i) == n,
    "V_i must be a numeric vector of length(maturities)",
    arg = "V_i"
  )
  assert_bad_argument_ok(
    is.list(Q_i) && length(Q_i) == n,
    "Q_i must be a list of length(maturities) elements",
    arg = "Q_i"
  )
  structure(
    list(L_i = L_i, V_i = V_i, Q_i = Q_i),
    maturities = maturities,
    n_components = n_components,
    class = "hetid_components"
  )
}

#' @rdname compute_identified_set_components
#' @param x A \code{hetid_components} object, for \code{print()}.
#' @param ... Unused arguments, accepted for method consistency.
#' @export
print.hetid_components <- function(x, ...) {
  cat("<hetid_components>\n")
  cat("  components (theta axis): ", attr(x, "n_components"), "\n", sep = "")
  cat(
    "  maturities (constraint axis): ",
    paste(attr(x, "maturities"), collapse = ", "), "\n",
    sep = ""
  )
  invisible(x)
}
