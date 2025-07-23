#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Extract yields and term premia
#' yields <- data[, grep("^y", names(data))]
#' term_premia <- data[, grep("^tp", names(data))]
#' }
