#' @param step Positive integer number of months per news period, no greater than
#'   half of \code{HETID_CONSTANTS$MAX_MATURITY}. Functions with a default use
#'   \code{HETID_CONSTANTS$DEFAULT_STEP}. For time-series inputs, adjacent observations
#'   must be \code{step} months apart. Functions that read maturity \code{i + step}
#'   require \code{i <= HETID_CONSTANTS$MAX_MATURITY - step}; additional
#'   horizon restrictions are described for each function.
