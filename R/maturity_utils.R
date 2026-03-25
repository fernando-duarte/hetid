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

#' Resolve and Validate Maturity Indices
#'
#' Infers or validates maturity indices from named statistical
#' inputs. Used by identification-layer functions.
#'
#' @param maturities Explicit maturities or NULL to infer
#' @param stats_list Named list of statistical inputs to check
#' @param n_components Total number of components (ncol(gamma))
#'
#' @return Integer vector of maturity indices
#' @keywords internal
resolve_maturities <- function(maturities, stats_list,
                               n_components) {
  if (!is.list(stats_list) || length(stats_list) == 0) {
    stop("stats_list must be a non-empty list")
  }

  all_names <- lapply(stats_list, names)
  has_names <- !vapply(all_names, is.null, logical(1))

  if (any(has_names) && !all(has_names)) {
    stop(
      "Cannot mix named and unnamed statistical inputs. ",
      "Either name all inputs with 'maturity_N' names ",
      "or leave all unnamed."
    )
  }

  parsed <- NULL
  if (all(has_names)) {
    anchor <- all_names[[1]]
    if (!all(grepl("^maturity_\\d+$", anchor))) {
      stop(
        "Named inputs must follow 'maturity_N' pattern. ",
        "Got: ", paste(anchor, collapse = ", ")
      )
    }
    for (nm in names(all_names)[-1]) {
      if (!identical(all_names[[nm]], anchor)) {
        stop(
          "Names of ", nm,
          " do not match maturities. ",
          "All named inputs must have identical names."
        )
      }
    }
    parsed <- as.integer(sub("^maturity_", "", anchor))
    if (anyDuplicated(parsed)) {
      stop(
        "Duplicate maturity values in names: ",
        paste(
          parsed[duplicated(parsed)],
          collapse = ", "
        )
      )
    }
  }

  if (is.null(maturities)) {
    if (!is.null(parsed)) {
      maturities <- parsed
    } else {
      n_stats <- length(stats_list[[1]])
      if (n_stats < n_components) {
        stop(
          "Cannot infer maturities: inputs have length ",
          n_stats, " but n_components = ", n_components,
          ". Pass maturities explicitly or use named ",
          "inputs with 'maturity_N' names."
        )
      }
      maturities <- seq_len(n_stats)
    }
  } else if (!is.null(parsed)) {
    expected <- paste0("maturity_", maturities)
    if (!identical(all_names[[1]], expected)) {
      stop(
        "Input names do not match maturities. ",
        "Names: ",
        paste(all_names[[1]], collapse = ", "),
        ". Expected: ",
        paste(expected, collapse = ", ")
      )
    }
  }

  as.integer(maturities)
}
