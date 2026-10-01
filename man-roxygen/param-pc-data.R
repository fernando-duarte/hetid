#' @param pcs Numeric matrix or data frame of instruments (n x J), with one row
#'   per yield row, already aligned to yields by calendar date. In the VFCI
#'   application these are principal components of nominal financial asset
#'   returns. Other instrument sets are accepted; see
#'   \code{\link{build_instrument_matrix}} for a validated constructor supporting
#'   arbitrary transformations. Join instruments to yields by \code{date}
#'   before calling.
