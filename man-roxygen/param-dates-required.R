#' @param dates Required non-missing \code{Date} vector of period-end calendar
#'   dates, one per row of \code{yields}/\code{term_premia}. Supplied dates are
#'   used as labels without normalization. \code{NULL}, non-\code{Date}, or
#'   missing dates raise \code{hetid_error_bad_argument}; a wrong-length vector
#'   raises \code{hetid_error_dimension_mismatch}.
