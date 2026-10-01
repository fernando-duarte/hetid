#' Evidence for Boundedness of a Quadratic Feasible Set
#'
#' Search for sufficient evidence about linear objectives over the intersection
#' of quadratic inequalities. This function does not optimize finite endpoints.
#'
#' @param quadratic List containing nonempty parallel lists \code{A_i}, \code{b_i} and a
#'   numeric vector \code{c_i}, representing \verb{x' A_i x + b_i' x + c_i <= 0}.
#'   Matrices must be finite, real, exactly symmetric and share one positive
#'   dimension. Each \code{b_i} vector has that dimension; \code{c_i} has one
#'   entry per constraint. All coefficients must be finite and real.
#' @param objectives Finite numeric matrix, with one objective loading per column
#'   and one row per coordinate. At least one column is required. Only exactly
#'   zero loadings denote constants.
#' @param points Optional finite numeric matrix of candidate points, one per row.
#'   It has one column per coordinate; \code{NULL} supplies no user candidates.
#'   Nonzero points are also tried as direction candidates.
#' @param directions Optional finite numeric matrix of candidate directions,
#'   one per row and one column per coordinate. \code{NULL} supplies no user
#'   candidates. These are checked independently before accepting any evidence.
#' @param n_dir Nonnegative integer number of additional sampled directions;
#'   zero disables sampling. Defaults to \code{IDENTIFIED_SET_CONTROL$N_DIR}.
#' @param maxit Nonnegative integer iteration budget for multivariate candidate
#'   searches; zero disables deterministic optimization. Scalar weight searches
#'   use a fixed tolerance. Defaults to \code{HETID_CONSTANTS$QUADRATIC_EVIDENCE_MAXIT}.
#'   Search exhaustion never proves boundedness.
#'
#' @details
#' A positive definite nonnegative combination of constraint Hessians proves
#' that the feasible set is bounded if it is nonempty. Nonemptiness requires a
#' checked point or a feasible infinite tail. A strict negative-curvature
#' direction for every constraint makes all nonconstant linear objectives
#' two-sided unbounded, including objectives orthogonal to that direction.
#' Infinite line tails give side-specific evidence. Simple coordinate half-spaces
#' and structurally zero null-coordinate blocks also provide directional bounds.
#'
#' Every certificate is sufficient, and searches are incomplete. An unresolved
#' side is not evidence of emptiness, finiteness or infinity. Numerical sign
#' checks use conservative rounding margins; near-zero cancellation and poorly
#' conditioned systems can remain unresolved. Finite optimizer output alone
#' supplies no boundedness evidence.
#'
#' Missing or nonfinite numeric entries are rejected rather than omitted.
#' Invalid inputs raise structured \code{hetid_error} conditions. Conflicting
#' geometry certificates also raise a structured error.
#'
#' Direction sampling uses the existing fixed-seed search and restores
#' \code{.Random.seed}, including its absence. Reproducibility assumes the same
#' \code{RNGkind()}. The Box-Muller cached deviate, which is outside
#' \code{.Random.seed}, cannot be restored by that search. Use \code{n_dir = 0}
#' to avoid RNG use entirely.
#'
#' @return A list with \code{summary}, a data frame with columns \code{objective},
#'   \code{lower_state}, \code{upper_state}, and \code{constant}, one row per
#'   objective. States are \code{bounded}, \code{unbounded}, or \code{unresolved}.
#'   Objective names use column names, replacing missing or empty names by
#'   \code{objective_j} for column \code{j} and making duplicates unique.
#'   \code{nonempty} is a logical flag for verified nonemptiness; \code{FALSE}
#'   does not prove emptiness. \code{boundedness} is a positive definite
#'   combination certificate or \code{NULL}; \code{directional} is a list of
#'   directional bound certificates; \code{strict_direction} is a verified
#'   strict negative-curvature direction or \code{NULL}. \code{tails} is an
#'   objective-named list of tail evidence. \code{feasible_points} is a matrix
#'   of verified points, one per row, possibly with zero rows.
#'   The \code{check_point(point)} closure accepts a finite real numeric vector
#'   with one entry per coordinate and returns a logical scalar. It uses the
#'   same strict membership check as finite nonemptiness witnesses. It returns
#'   false for unresolved membership or invalid points and uses no optimizer
#'   feasibility tolerance. The
#'   \code{outer_bounds} closure,
#'   \code{outer_bounds(objectives, refine = TRUE, pool = NULL)}, accepts an
#'   objective matrix with the same coordinate dimension and returns a data
#'   frame with \code{lower} and \code{upper}, one row per objective column.
#'   These numerical bounds contain the set and use checked positive definite
#'   combinations of the constraints. Set \code{refine = FALSE} to use only
#'   the common certificate and supplied candidate pool without further searches.
#'   Bounds are \code{NA} when no combination is verified, and exact zero loadings
#'   give exact zeros. These bounds need not be attained. Pass the returned
#'   \code{pool} attribute back to reuse candidate weights across calls. Every weight
#'   vector is verified again. Multivariate searches use \code{maxit}; scalar weight
#'   searches use a fixed tolerance. The search consumes no random numbers.
#'   Other attributes are \code{sources} (candidate indices for each bound),
#'   \code{candidates} (numerical certificate diagnostics), \code{empty} (whether
#'   a verified combination proves emptiness), and \code{reason} (an explanation
#'   when all nonconstant bounds are unknown, or \code{NULL}).
#' @export
#' @examples
#' ball <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
#' evidence <- compute_quadratic_set_evidence(ball, diag(2), n_dir = 0)
#' evidence$summary
#' evidence$check_point(c(0, 0))
#' evidence$outer_bounds(diag(2), refine = FALSE)
#'
#' halfspace <- list(A_i = list(matrix(0, 1, 1)), b_i = list(1), c_i = 0)
#' compute_quadratic_set_evidence(
#'   halfspace, matrix(1),
#'   points = matrix(-1), directions = matrix(-1),
#'   n_dir = 0, maxit = 0
#' )$summary
compute_quadratic_set_evidence <- function(quadratic, objectives,
                                           points = NULL, directions = NULL,
                                           n_dir = IDENTIFIED_SET_CONTROL$N_DIR,
                                           maxit = HETID_CONSTANTS$QUADRATIC_EVIDENCE_MAXIT) {
  input <- validate_quadratic_evidence(quadratic, objectives, points, directions)
  assert_scalar_integer_in_range(n_dir, "n_dir", 0, .Machine$integer.max)
  assert_scalar_integer_in_range(maxit, "maxit", 0, .Machine$integer.max)
  count <- ncol(objectives)
  objective_names <- quadratic_objective_names(objectives)
  constant <- colSums(objectives != 0) == 0L
  candidate_search <- quadratic_boundedness_search(quadratic, maxit)
  certificate <- candidate_search$certificate
  points <- quadratic_collect_points(quadratic, input$points, certificate, maxit)
  tail_result <- quadratic_collect_tails(
    quadratic, objectives, input, points, candidate_search, n_dir, maxit
  )
  nonempty <- nrow(points) > 0L || tail_result$nonempty
  lower <- tail_result$lower
  upper <- tail_result$upper
  directional <- quadratic_directional_bounds(quadratic, objectives)
  finite_lower <- rep(!is.null(certificate), count) | constant | directional$lower
  finite_upper <- rep(!is.null(certificate), count) | constant | directional$upper
  if (any(lower & finite_lower) || any(upper & finite_upper)) {
    stop_hetid("Quadratic geometry certificates conflict")
  }
  lower_state <- ifelse(lower, "unbounded",
    ifelse(finite_lower & nonempty, "bounded", "unresolved")
  )
  upper_state <- ifelse(upper, "unbounded",
    ifelse(finite_upper & nonempty, "bounded", "unresolved")
  )
  list(
    summary = data.frame(
      objective = objective_names, lower_state, upper_state,
      constant = constant, stringsAsFactors = FALSE
    ),
    nonempty = nonempty, boundedness = certificate,
    directional = directional$certificates,
    strict_direction = tail_result$strict,
    tails = stats::setNames(tail_result$tails, objective_names),
    feasible_points = points,
    check_point = quadratic_point_verifier(quadratic),
    outer_bounds = quadratic_outer_bounder(quadratic, certificate, maxit, nonempty)
  )
}
