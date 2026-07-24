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

bootstrap_validation_rebind_gate_decision <- function(
  decision,
  fresh_gate,
  gate_hash
) {
  stopifnot(
    is.list(decision),
    is.list(fresh_gate),
    is.character(gate_hash),
    length(gate_hash) == 1L,
    !is.na(gate_hash),
    nzchar(gate_hash)
  )
  lag_name <- sprintf("lag%d", as.integer(fresh_gate$gate_lag))
  stopifnot(
    length(fresh_gate$sample_id) == 1L,
    length(fresh_gate$gate_lag) == 1L,
    length(fresh_gate$gate_alpha) == 1L,
    lag_name %in% names(fresh_gate$q_stats),
    lag_name %in% names(fresh_gate$p_values),
    length(fresh_gate$verdict) == 1L
  )

  decision$gate_science_sha256 <- gate_hash
  decision$sample_id <- fresh_gate$sample_id
  decision$gate_lag <- as.integer(fresh_gate$gate_lag)
  decision$gate_alpha <- fresh_gate$gate_alpha
  decision$gate_q <- unname(fresh_gate$q_stats[[lag_name]])
  decision$gate_p <- unname(fresh_gate$p_values[[lag_name]])
  decision$gate_verdict <- fresh_gate$verdict
  decision
}
