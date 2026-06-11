#' @section General instrument scheme:
#' For each component i the weight matrix \code{lambda[[i]]} (J x K_i)
#' defines K_i constructed instruments, each a linear combination of
#' the J columns of the instrument matrix the moments were computed
#' from. The identified set intersects one quadratic constraint per
#' (component, combination) pair: \code{sum(K_i)} constraints in
#' total. The legacy single-combination case is \code{K_i = 1} for
#' every component (a J x I matrix); using every instrument
#' separately is \code{lambda[[i]] = diag(J)}. Each constraint
#' depends on its weight column only through its direction, so any
#' nonzero rescaling of a column leaves the constraint set unchanged.
#' A per-component instrument SUBSET is expressed directly as zero
#' rows in that component's weight matrix;
#' \code{align_instrument_sets()} builds the union axis and
#' \code{lambda_from_support()} zero-pads compact weights onto it.
#' Honesty note: intersecting more sample-moment constraints
#' weakly shrinks the ESTIMATED set mechanically, so widths are
#' comparable only across schemes with the same constraint count.
