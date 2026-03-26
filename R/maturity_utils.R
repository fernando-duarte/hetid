#' Maturity Utilities
#'
#' Helpers for resolving and validating maturity indices.
#'
#' @name maturity_utils
#' @keywords internal
NULL

#' Build Maturity Names
#'
#' Generates standard maturity label vector from indices.
#'
#' @param maturities Integer vector of maturity indices
#' @return Character vector, e.g. c("maturity_1", "maturity_2")
#' @keywords internal
maturity_names <- function(maturities) {
  paste0(HETID_CONSTANTS$MATURITY_PREFIX, maturities)
}

#' Assert valid statistics list
#'
#' @keywords internal
#' @noRd
assert_valid_stats_list <- function(stats_list) {
  assert_bad_argument_ok(
    is.list(stats_list) && length(stats_list) > 0,
    "stats_list must be a non-empty list",
    arg = "stats_list"
  )
}

#' Collect maturity name metadata
#'
#' @keywords internal
#' @noRd
collect_maturity_name_info <- function(stats_list) {
  all_names <- lapply(stats_list, names)
  has_names <- !vapply(all_names, is.null, logical(1))

  assert_bad_argument_ok(
    !any(has_names) || all(has_names),
    paste0(
      "Cannot mix named and unnamed statistical inputs. ",
      "Either name all inputs with 'maturity_N' names ",
      "or leave all unnamed."
    )
  )

  if (!all(has_names)) {
    return(list(all_names = all_names, parsed = NULL))
  }

  anchor <- all_names[[1]]
  assert_bad_argument_ok(
    all(grepl("^maturity_\\d+$", anchor)),
    paste0(
      "Named inputs must follow 'maturity_N' pattern. ",
      "Got: ", paste(anchor, collapse = ", ")
    )
  )

  for (nm in names(all_names)[-1]) {
    assert_bad_argument_ok(
      identical(all_names[[nm]], anchor),
      paste0(
        "Names of ", nm,
        " do not match maturities. ",
        "All named inputs must have identical names."
      )
    )
  }

  parsed <- as.integer(sub("^maturity_", "", anchor))
  assert_bad_argument_ok(
    !anyDuplicated(parsed),
    paste0(
      "Duplicate maturity values in names: ",
      paste(parsed[duplicated(parsed)], collapse = ", ")
    )
  )

  list(all_names = all_names, parsed = parsed)
}

#' Infer maturities from unnamed or named inputs
#'
#' @keywords internal
#' @noRd
infer_maturities <- function(parsed, stats_list, n_components) {
  if (!is.null(parsed)) {
    return(parsed)
  }

  n_stats <- length(stats_list[[1]])
  assert_dimension_ok(
    n_stats >= n_components,
    paste0(
      "Cannot infer maturities: inputs have length ",
      n_stats, " but n_components = ", n_components,
      ". Pass maturities explicitly or use named ",
      "inputs with 'maturity_N' names."
    )
  )

  seq_len(n_stats)
}

#' Validate explicit maturities against input names
#'
#' @keywords internal
#' @noRd
validate_maturity_names <- function(maturities, all_names, parsed) {
  if (is.null(parsed)) {
    return(invisible(TRUE))
  }

  expected <- maturity_names(maturities)
  assert_bad_argument_ok(
    identical(all_names[[1]], expected),
    paste0(
      "Input names do not match maturities. ",
      "Names: ",
      paste(all_names[[1]], collapse = ", "),
      ". Expected: ",
      paste(expected, collapse = ", ")
    ),
    arg = "maturities"
  )

  invisible(TRUE)
}

#' Resolve and validate maturity indices
#'
#' @param maturities Explicit maturities or NULL to infer
#' @param stats_list Named list of statistical inputs to check
#' @param n_components Total number of components (ncol(gamma))
#'
#' @return Integer vector of maturity indices
#' @keywords internal
resolve_maturities <- function(maturities, stats_list,
                               n_components) {
  assert_valid_stats_list(stats_list)
  name_info <- collect_maturity_name_info(stats_list)

  if (is.null(maturities)) {
    maturities <- infer_maturities(
      name_info$parsed,
      stats_list,
      n_components
    )
  } else {
    validate_maturity_names(
      maturities,
      name_info$all_names,
      name_info$parsed
    )
  }

  as.integer(maturities)
}
