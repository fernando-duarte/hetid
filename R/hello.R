#' Say Hello
#'
#' This function prints a greeting message
#'
#' @param name Character string with the name to greet
#' @return NULL (invisibly)
#' @export
#' @examples
#' hello("World")
hello <- function(name = "World") {
  message(paste0("Hello, ", name, "!"))
  invisible(NULL)
}
