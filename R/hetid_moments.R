#' Compute Identification Moments
#'
#' Computes all seven statistical moments needed for the identified set
#' and returns them in a single validated \code{hetid_moments} container
#' that carries the maturity identity through the pipeline. This is the
#' entry point of the identification chain; pass the result to
#' \code{\link{build_quadratic_system}} or
#' \code{\link{compute_identified_set_components}}.
#'
#' @param w1 Numeric vector of \eqn{\omega_1} residuals from
#'   \code{\link{compute_w1_residuals}()}, with at least two observations.
#' @param w2 Numeric matrix or data frame of \eqn{\omega_2} residuals
#'   (T x I) from \code{\link{compute_w2_residuals}()}, with at least one
#'   column and the same number of observations as \code{w1}.
#' @param pcs Numeric matrix or data frame of exogenous time-series
#'   instruments (T x J), with at least one column and the same number
#'   of observations as \code{w1}. In the VFCI application these are
#'   principal components of nominal financial asset returns. Column names
#'   label the instrument axis (falling back to \code{pc1}, ..., \code{pcJ}).
#' @param maturities Nonempty numeric vector of distinct integer-valued
#'   \code{w2} column indices in \code{1:ncol(w2)} whose moment conditions
#'   are computed (the constraint axis). \code{NULL}, the default, selects
#'   all columns of \code{w2}. The supplied order is preserved.
#'
#' @return An object of class \code{hetid_moments}: a list with elements
#'   \code{s_i_0}, \code{sigma_i_sq}, \code{r_i_0}, \code{r_i_1},
#'   \code{p_i_0}, \code{s_i_1}, \code{s_i_2} and attributes
#'   \code{maturities}, \code{n_components}, \code{n_obs},
#'   \code{n_instruments}.
#'   With M the number of selected columns, \code{I = ncol(w2)}, and
#'   \code{J = ncol(pcs)}, the elements are:
#'   \describe{
#'     \item{\code{s_i_0}, \code{sigma_i_sq}}{Numeric vectors of length M.}
#'     \item{\code{r_i_0}, \code{p_i_0}}{J x M numeric matrices.}
#'     \item{\code{r_i_1}}{A list of M numeric J x I matrices.}
#'     \item{\code{s_i_1}}{A list of M numeric vectors of length I.}
#'     \item{\code{s_i_2}}{A list of M numeric I x I covariance matrices.}
#'   }
#'   Constraint-axis names are \code{maturity_N} in the order of
#'   \code{maturities}; inner theta-axis names cover all \code{w2} columns.
#'   The attributes record selected indices, full theta dimension, observation
#'   count, and instrument count, respectively. Degenerate inputs can return
#'   zero moments with a warning; no positive-variance guarantee is made.
#'
#' @details
#' \code{w1}, \code{w2}, and \code{pcs} must contain finite numeric values;
#' missing values are rejected
#' rather than removed. Align observations by date before calling: this function
#' does not match dates or reorder rows. Data frames are coerced to matrices.
#'
#' All seven moments are centered sample covariances or variances with
#' \eqn{1/T} normalization. See \code{\link{compute_scalar_statistics}},
#' \code{\link{compute_vector_statistics}}, and
#' \code{\link{compute_matrix_statistics}} for their formulas.
#' The variance-positivity diagnostic emits a
#' \code{hetid_warning_degenerate_variance} when identification variances are
#' numerically degenerate. Invalid inputs raise structured \code{hetid_error}
#' conditions.
#'
#' Direct callers are responsible for supplying unique instrument column
#' names; the validated front door for constructing instrument matrices
#' is \code{\link{build_instrument_matrix}()}.
#'
#' @template section-maturity-convention
#'
#' @seealso \code{\link{build_instrument_matrix}} for constructing
#'   instrument matrices with optional transformations,
#'   \code{\link{build_general_quadratic_system}} for building quadratic
#'   constraints from arbitrary linear combinations of instruments.
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
#'     get(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     NULL
#'   }
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   n_obs <- 100
#'   w1 <- rnorm(n_obs)
#'   w2 <- matrix(rnorm(n_obs * 2), nrow = n_obs, ncol = 2)
#'   pcs <- matrix(rnorm(n_obs * 2), nrow = n_obs, ncol = 2)
#'
#'   moments <- compute_identification_moments(w1, w2, pcs)
#'   print(moments)
#'
#'   subset_moments <- compute_identification_moments(
#'     w1, w2, pcs,
#'     maturities = 2
#'   )
#'   names(subset_moments$s_i_0)
#' })
compute_identification_moments <- function(w1, w2, pcs, maturities = NULL) {
  validated <- validate_statistics_inputs(w1, w2, maturities)
  w2 <- validated$w2
  maturities <- validated$maturities
  pcs <- validate_pcs_input(pcs, validated$t_obs)

  scalar_stats <- compute_scalar_statistics_impl(w1, w2, maturities)
  warn_if_variance_degenerate(
    w1, w2, maturities,
    sigma_i_sq = scalar_stats$sigma_i_sq,
    s_i_0 = scalar_stats$s_i_0
  )
  vector_stats <- compute_vector_statistics_impl(w1, w2, pcs, maturities)
  matrix_stats <- compute_matrix_statistics_impl(w1, w2, maturities)

  moments <- new_hetid_moments(
    list(
      s_i_0 = scalar_stats$s_i_0,
      sigma_i_sq = scalar_stats$sigma_i_sq,
      r_i_0 = vector_stats$r_i_0,
      r_i_1 = vector_stats$r_i_1,
      p_i_0 = vector_stats$p_i_0,
      s_i_1 = matrix_stats$s_i_1,
      s_i_2 = matrix_stats$s_i_2
    ),
    maturities = maturities,
    n_components = ncol(w2),
    n_obs = validated$t_obs
  )
  validate_hetid_moments(moments)
  moments
}

#' Construct a hetid_moments Object
#'
#' Low-level cheap constructor for the \code{hetid_moments} class: type,
#' scalar, and maturity-vector checks with lossless coercions, trusting
#' the shapes of the statistics themselves. The per-maturity
#' structural-alignment sweep lives in \code{validate_hetid_moments()},
#' which the public boundary \code{compute_identification_moments()}
#' always runs; hot paths rebuilding containers from known-good parts
#' may call this constructor directly and skip it.
#'
#' @param stats List with the seven statistics (\code{s_i_0},
#'   \code{sigma_i_sq}, \code{r_i_0}, \code{r_i_1}, \code{p_i_0},
#'   \code{s_i_1}, \code{s_i_2}). Extra elements are discarded.
#' @param maturities Nonempty numeric vector of distinct integer-valued
#'   \code{w2} column indices in \code{1:n_components}, coerced to integer.
#' @param n_components Positive integer-valued numeric scalar giving the
#'   theta-axis dimension (\code{ncol(w2)}), no larger than
#'   \code{.Machine$integer.max}.
#' @param n_obs Positive integer-valued numeric observation count, no larger
#'   than \code{.Machine$integer.max}.
#'
#' @return A \code{hetid_moments} list containing the seven named statistics
#'   in the order above, with integer attributes \code{maturities},
#'   \code{n_components}, and \code{n_obs}, plus \code{n_instruments} from
#'   \code{nrow(stats$r_i_0)}. Shapes are not validated by this constructor.
#' @keywords internal
new_hetid_moments <- function(stats, maturities, n_components, n_obs) {
  required <- c(
    "s_i_0", "sigma_i_sq", "r_i_0", "r_i_1", "p_i_0", "s_i_1", "s_i_2"
  )
  assert_bad_argument_ok(
    is.list(stats) && all(required %in% names(stats)),
    paste0(
      "stats must be a list containing: ",
      paste(required, collapse = ", ")
    ),
    arg = "stats"
  )
  assert_scalar_integer_in_range(
    n_obs, "n_obs", 1, .Machine$integer.max
  )
  assert_scalar_integer_in_range(
    n_components, "n_components", 1, .Machine$integer.max
  )
  n_components <- as.integer(n_components)
  validate_maturities(
    maturities,
    max_value = n_components, max_label = "n_components"
  )
  maturities <- as.integer(maturities)

  structure(
    stats[required],
    maturities = maturities,
    n_components = n_components,
    n_obs = as.integer(n_obs),
    n_instruments = nrow(stats$r_i_0),
    class = "hetid_moments"
  )
}
