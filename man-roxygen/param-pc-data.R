#' @param pcs Matrix of principal components (n x J) where n is the
#'   number of observations (one row per yield row, aligned to the
#'   yields by calendar date) and J is the number of components,
#'   extracted from a broader set of returns of financial assets.
#'   Required: join the components to the yields by \code{date} before
#'   calling. The matrix may be any instrument set, not only principal
#'   components; see \code{\link{build_instrument_matrix}()} for a
#'   validated constructor that supports arbitrary transformations.
