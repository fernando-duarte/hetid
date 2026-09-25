#' Search for a Recession Direction of the Identified Set
#'
#' Internal search for a direction along which the identified set runs to
#' infinity. Because every constraint has the form
#' \eqn{g_i(\theta) = \theta' A_i \theta + b_i'\theta + c_i}, moving from
#' any feasible point along \eqn{v} sends \eqn{g_i} to \eqn{-\infty}
#' whenever \eqn{v' A_i v < 0}. A direction that does this for every
#' constraint at once certifies unboundedness. This strict-curvature
#' condition is sufficient, not necessary: a cylinder can be unbounded
#' along a direction with zero curvature.
#'
#' The search is a finite sample of the unit sphere, so it decides
#' unboundedness one way only: a returned direction is a witness and can
#' be checked, while \code{NULL} means no witness was found rather than a
#' proof that none exists. That asymmetry is why the caller reports
#' \code{Inf} on the strength of a witness or a feasible infinite line
#' tail, never on the strength of a search window.
#'
#' Directions use a fixed seed with the caller's RNG kind. The caller's
#' \code{.Random.seed}, including its absence, is restored afterwards.
#' Reproducibility assumes the same \code{RNGkind()}. The default Inversion
#' normal generator has no cached deviate; Box-Muller's cached second deviate
#' is outside \code{.Random.seed} and is cleared by \code{set.seed()}.
#'
#' @param quadratic Quadratic form list with \code{A_i}, as returned by
#'   \code{build_quadratic_system()}
#' @param n_dir Number of unit directions to sample
#' @return Numeric unit vector \eqn{v} with \eqn{v' A_i v < 0} for every
#'   constraint, or \code{NULL} when the sample contains none
#' @noRd
recession_direction <- function(quadratic,
                                n_dir = IDENTIFIED_SET_CONTROL$N_DIR) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) saved <- get(".Random.seed", envir = globalenv())
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", saved, envir = globalenv()) # nolint: object_name_linter.
      } else {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )
  set.seed(IDENTIFIED_SET_CONTROL$DIR_SEED)
  n_components <- nrow(quadratic$A_i[[1]])
  dirs <- matrix(stats::rnorm(n_dir * n_components), nrow = n_dir)
  dirs <- dirs / sqrt(rowSums(dirs^2))
  negative <- rep(TRUE, n_dir)
  for (a_mat in quadratic$A_i) { # nolint: object_name_linter.
    negative <- negative & rowSums((dirs %*% a_mat) * dirs) < 0
    if (!any(negative)) {
      return(NULL)
    }
  }
  dirs[which(negative)[1], ]
}
