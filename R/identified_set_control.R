#' Identified-Set Search Controls
#'
#' @description
#' Numerical controls for the slack-\eqn{\tau} identified-set box search.
#' The search uses an analytic free-coordinate hull on a grid, so these
#' controls describe a grid and a growth schedule rather than an optimizer.
#'
#' @format List containing identified-set search controls:
#' \describe{
#'   \item{N_GRID}{Points per gridded coordinate (41L); must be odd so the
#'     grid contains the center. The box is an
#'     inner approximation whose gridded coordinates carry the resolution
#'     error, so raising this tightens the box and costs
#'     \code{N_GRID^(I-1)} hull solves per coordinate}
#'   \item{MAX_GROWTH}{Maximum extent-doubling passes per growth phase
#'     before the search stops growing and keeps the finite bounds it has
#'     found (12L); unboundedness requires a strict-curvature witness or
#'     a feasible infinite line tail, never an exhausted budget}
#'   \item{N_DIR}{Unit directions sampled when searching for a recession
#'     direction (20000L)}
#'   \item{DIR_SEED}{Seed for that direction sample (20260815L). Samples
#'     depend on \code{RNGkind()}; \code{.Random.seed} is restored, but
#'     the cached Box-Muller normal deviate is not}
#'   \item{N_POINTS}{Interpolation steps taken from the center toward each
#'     box witness when sampling the set for a profile (5L)}
#'   \item{FEAS_TOL}{Largest constraint value accepted when filtering
#'     finite profile candidates (1e-10). Boundary rounding can otherwise
#'     reject a point. This absolute tolerance depends on constraint units.
#'     Functional-bound witnesses use it times the sum of absolute quadratic,
#'     linear and constant terms. Line-cell and infinite-tail classification
#'     do not use it}
#'   \item{SEARCH_LIMIT}{Largest half-width, in slab-frame units, the
#'     growth loop will expand to (4096)}
#'   \item{NULL_LOADING_RTOL}{Default for \code{null_loading_rtol} in
#'     \code{compute_identified_set_box()} (\code{sqrt(.Machine$double.eps)}).
#'     A structural loading column with no entry above this fraction of the
#'     largest loading in its own row of \code{beta2r} is treated as
#'     exactly zero. Such a column is rounding noise on a known zero, and
#'     left in it would turn a point-identified coefficient into an
#'     interval, or into an unbounded one when the set is unbounded. The
#'     rule assumes the columns of \code{x} are comparably scaled, as
#'     principal components and own lags are; the box records the columns
#'     it snapped as \code{null_loading}}
#' }
#'
#' @return A named list of identified-set search controls (the elements
#'   described in \strong{Format}). Access individual controls with
#'   \code{$}.
#' @examples
#' IDENTIFIED_SET_CONTROL$N_GRID
#' IDENTIFIED_SET_CONTROL$FEAS_TOL
#' IDENTIFIED_SET_CONTROL$NULL_LOADING_RTOL
#' @export
IDENTIFIED_SET_CONTROL <- list(
  N_GRID = 41L,
  MAX_GROWTH = 12L,
  N_DIR = 20000L,
  DIR_SEED = 20260815L,
  N_POINTS = 5L,
  FEAS_TOL = 1e-10,
  SEARCH_LIMIT = 4096,
  NULL_LOADING_RTOL = sqrt(.Machine$double.eps)
)
