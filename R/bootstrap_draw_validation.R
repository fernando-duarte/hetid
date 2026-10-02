validate_bootstrap_indices <- function(indices) {
  assert_bad_argument_ok(is.list(indices) && length(indices) > 0L,
    "indices must be a nonempty list",
    arg = "indices"
  )
  if (!is.null(names(indices))) assert_instrument_names(names(indices), "indices")
  n <- length(indices[[1]])
  assert_bad_argument_ok(n >= 1L, "indices[[1]] must be a nonempty index vector", arg = "indices")
  valid <- vapply(indices, function(index) {
    bootstrap_is_numeric(index) && is.null(dim(index)) && length(index) == n &&
      all(is.finite(index)) && all(index == floor(index)) && all(index >= 1 & index <= n)
  }, logical(1))
  assert_bad_argument_ok(
    all(valid), "indices must have one common length and valid sample indices"
  )
  invisible(TRUE)
}

bootstrap_rng_capture <- function() {
  present <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  list(
    kind = RNGkind(), present = present,
    seed = if (present) get(".Random.seed", envir = globalenv()) else NULL
  )
}

bootstrap_rng_restore <- function(saved) {
  if (!identical(RNGkind(), saved$kind)) do.call(RNGkind, as.list(saved$kind))
  if (saved$present) {
    assign(".Random.seed", saved$seed, envir = globalenv()) # nolint: object_name_linter.
  } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(NULL)
}
