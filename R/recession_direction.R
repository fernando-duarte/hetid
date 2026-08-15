#' Search for a Recession Direction of the Identified Set
#'
#' Internal search for a direction along which the identified set runs to
#' infinity. Because every constraint has the form
#' \eqn{g_i(\theta) = \theta' A_i \theta + b_i'\theta + c_i}, moving from
#' any feasible point along \eqn{v} sends \eqn{g_i} to \eqn{-\infty}
#' whenever \eqn{v' A_i v < 0}. A direction that does this for every
#' constraint at once is a recession direction, and the set is unbounded
#' exactly when one exists. Conversely, if every direction leaves some
#' constraint growing, the set is contained in a ball.
#'
#' The search is a finite sample of the unit sphere, so it decides
#' unboundedness one way only: a returned direction is a witness and can
#' be checked, while \code{NULL} means no witness was found rather than a
#' proof that none exists. That asymmetry is why the caller reports
#' \code{Inf} on the strength of a witness and never on the strength of a
#' search window.
#'
#' The directions are drawn from a fixed seed and the caller's random
#' stream is restored afterwards, so the search is reproducible and does
#' not consume the user's randomness.
#'
#' @param quadratic Quadratic form list with \code{A_i}, as returned by
#'   \code{build_quadratic_system()}
#' @param n_dir Number of unit directions to sample
#' @return Numeric unit vector \eqn{v} with \eqn{v' A_i v < 0} for every
#'   constraint, or \code{NULL} when the sample contains none
#' @noRd
recession_direction <- function(quadratic,
                                n_dir = IDENTIFIED_SET_CONTROL$N_DIR) {
  if (exists(".Random.seed", envir = globalenv())) {
    saved <- get(".Random.seed", envir = globalenv())
    on.exit(
      assign(".Random.seed", saved, envir = globalenv()), # nolint: object_name_linter.
      add = TRUE
    )
  }
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
