#' Compute Identification Moments
#'
#' Computes all seven statistical moments needed for the identified set
#' and returns them in a single validated \code{hetid_moments} container
#' that carries the maturity identity through the pipeline. This is the
#' entry point of the identification chain; pass the result to
#' \code{\link{build_quadratic_system}} or
#' \code{\link{compute_identified_set_components}}.
#'
#' @param w1 Numeric vector of W1 residuals from compute_w1_residuals()
#' @param w2 Matrix of W2 residuals (T x I) from compute_w2_residuals()
#' @param pcs Matrix of principal components (T x J)
#' @param maturities Integer vector of w2 column indices whose moment
#'   conditions are computed (the constraint axis). Default is all
#'   columns of w2.
#'
#' @return An object of class \code{hetid_moments}: a list with elements
#'   \code{s_i_0}, \code{sigma_i_sq}, \code{r_i_0}, \code{r_i_1},
#'   \code{p_i_0}, \code{s_i_1}, \code{s_i_2} and attributes
#'   \code{maturities}, \code{n_components}, \code{n_obs}.
#'
#' @template section-maturity-convention
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 100
#' w1 <- rnorm(n_obs)
#' w2 <- matrix(rnorm(n_obs * 4), nrow = n_obs, ncol = 4)
#' pcs <- matrix(rnorm(n_obs * 3), nrow = n_obs, ncol = 3)
#'
#' moments <- compute_identification_moments(w1, w2, pcs)
#' moments
#'
#' subset_moments <- compute_identification_moments(
#'   w1, w2, pcs,
#'   maturities = c(2, 4)
#' )
#' names(subset_moments$s_i_0)
compute_identification_moments <- function(w1, w2, pcs, maturities = NULL) {
  validated <- validate_statistics_inputs(w1, w2, maturities)
  w2 <- validated$w2
  maturities <- validated$maturities
  warn_if_variance_degenerate(w1, w2, maturities, validated$t_obs)

  scalar_stats <- compute_scalar_statistics(w1, w2, maturities = maturities)
  vector_stats <- compute_vector_statistics(w1, w2, pcs, maturities = maturities)
  matrix_stats <- compute_matrix_statistics(w1, w2, maturities = maturities)

  new_hetid_moments(
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
}

#' Construct a Validated hetid_moments Object
#'
#' Low-level constructor and single structural-alignment gate for the
#' \code{hetid_moments} class. Validates every outer (constraint-axis)
#' shape and name against \code{maturity_names(maturities)} and every
#' inner (theta-axis) dimension against \code{n_components}.
#'
#' @param stats List with the seven statistics (\code{s_i_0},
#'   \code{sigma_i_sq}, \code{r_i_0}, \code{r_i_1}, \code{p_i_0},
#'   \code{s_i_1}, \code{s_i_2})
#' @param maturities Integer vector of w2 column indices
#' @param n_components Theta-axis dimension (\code{ncol(w2)})
#' @param n_obs Number of observations the moments were computed from
#'
#' @return A classed \code{hetid_moments} list
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
  assert_bad_argument_ok(
    length(n_obs) == 1 && is.finite(n_obs) && n_obs > 0,
    "n_obs must be a positive scalar",
    arg = "n_obs"
  )
  n_components <- as.integer(n_components)
  validate_maturities(
    maturities,
    max_value = n_components, max_label = "n_components"
  )
  maturities <- as.integer(maturities)
  assert_bad_argument_ok(
    !anyDuplicated(maturities),
    paste0(
      "maturities must not contain duplicates; got: ",
      paste(maturities, collapse = ", ")
    ),
    arg = "maturities"
  )

  validate_moments_shapes(stats, maturities, n_components)

  structure(
    stats[required],
    maturities = maturities,
    n_components = n_components,
    n_obs = as.integer(n_obs),
    class = "hetid_moments"
  )
}
