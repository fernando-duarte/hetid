#' @param n_pcs Integer specifying the number of principal components to use.
#'   Must be between 1 and 6. Default is 4 following Adrian, Crump, and Moench (2013).
#'   The choice of n_pcs represents a trade-off between capturing variation in
#'   financial asset returns and maintaining computational stability. More components
#'   capture more variation but may lead to overfitting in finite samples.
