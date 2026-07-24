bootstrap_validation_expression_has_paper_path <- function(
  expression,
  expected_path
) {
  stopifnot(
    is.character(expected_path),
    length(expected_path) > 0L,
    all(!is.na(expected_path)),
    all(nzchar(expected_path))
  )

  found <- FALSE
  inspect_call <- function(node) {
    if (found || !is.call(node)) return(invisible(NULL))

    if (identical(node[[1L]], as.name("paper_path"))) {
      arguments <- as.list(node)[-1L]
      values <- vapply(
        arguments,
        function(argument) {
          if (is.character(argument) && length(argument) == 1L) {
            argument
          } else {
            NA_character_
          }
        },
        character(1)
      )
      if (!anyNA(values) && identical(unname(values), expected_path)) {
        found <<- TRUE
        return(invisible(NULL))
      }
    }

    for (child in as.list(node)[-1L]) {
      inspect_call(child)
      if (found) break
    }
    invisible(NULL)
  }

  inspect_call(expression)
  found
}

bootstrap_validation_expression_is_assignment_call <- function(
  expression,
  target,
  function_name
) {
  stopifnot(
    is.character(target),
    length(target) == 1L,
    !is.na(target),
    nzchar(target),
    is.character(function_name),
    length(function_name) == 1L,
    !is.na(function_name),
    nzchar(function_name)
  )
  is.call(expression) &&
    identical(expression[[1L]], as.name("<-")) &&
    is.symbol(expression[[2L]]) &&
    identical(as.character(expression[[2L]]), target) &&
    is.call(expression[[3L]]) &&
    identical(expression[[3L]][[1L]], as.name(function_name))
}
