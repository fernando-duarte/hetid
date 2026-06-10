#' @section Maturity Indexing Convention:
#' A moments container is bound to one system: the \code{(w1, w2, pcs)}
#' it was computed from. Two axes are recorded as attributes of
#' \code{hetid_moments} objects:
#' \describe{
#'   \item{\code{n_components}}{the theta axis: \code{ncol(w2)}, the
#'     dimension of theta, the length of \code{tau}, and the column
#'     count of \code{gamma}. Immutable for a given system.}
#'   \item{\code{maturities}}{the constraint axis: which w2 columns (by
#'     column index, values in \code{1..n_components}) have their moment
#'     conditions computed and enforced. May be any subset, e.g.
#'     \code{c(2, 4, 5)} of a 6-column system.}
#' }
#'
#' "Maturity" therefore means w2 column index within the system, not
#' necessarily a true bond maturity; calling analyses map column indices
#' to bond labels externally when the system is built from a selection
#' of bonds. Column i of \code{gamma} and element i of \code{tau} pair
#' with w2 column i by position; bond labels never enter the alignment.
#' Per-maturity outputs are named \code{maturity_N} where N is
#' the w2 column index, with element k corresponding to
#' \code{maturities[k]}. Inner theta-axis dimensions (each \code{r_i_1}
#' matrix, \code{s_i_1} vector, and \code{s_i_2} matrix) are always full
#' \code{n_components}-sized. Downstream functions accept only validated
#' containers and resolve positions internally, so cross-object maturity
#' misalignment cannot occur.
