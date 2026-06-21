#' Build an Instrument Matrix from Primitives and Transformations
#'
#' Assembles the instrument matrix Z fed to
#' \code{\link{compute_identification_moments}} from a primitive
#' matrix and optional user transformations: each transform is a
#' function of the full primitive matrix returning a vector or matrix
#' of derived instruments (for example squares, interactions, or any
#' nonlinear feature). Outputs are validated (numeric, finite, aligned
#' rows, unique column names) and bound columnwise.
#'
#' Transforms must return fully finite values: apply lag- or
#' difference-type constructions before calling this function and trim
#' incomplete rows yourself, so that the rows of the result stay
#' aligned with the residual series.
#'
#' @param z Numeric matrix or data frame of primitive instruments
#'   (T x J). Unnamed columns are labeled \code{z1..zJ}.
#' @param transforms NULL, a function, or a named list of functions;
#'   each receives \code{z} (as a named numeric matrix) and returns a
#'   length-T vector or T-row matrix. Unnamed list entries are labeled
#'   \code{h1, h2, ...} by position. Output columns are always labeled
#'   from the transform name; a multi-column output gets suffixes
#'   \code{_1, _2, ...}.
#' @param include_original Logical; keep the primitive columns in the
#'   result (default TRUE). FALSE requires at least one transform.
#'
#' @return Numeric matrix (T x total instruments) with unique column
#'   names, ready for \code{compute_identification_moments()}.
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' z <- matrix(rnorm(60), nrow = 20, dimnames = list(NULL, c("a", "b", "c")))
#' build_instrument_matrix(
#'   z,
#'   transforms = list(
#'     sq_a = function(z) z[, "a"]^2,
#'     ab = function(z) z[, "a"] * z[, "b"]
#'   )
#' )[1:3, ]
build_instrument_matrix <- function(z, transforms = NULL,
                                    include_original = TRUE) {
  assert_tabular(z, "z")
  z <- as.matrix(z)
  assert_numeric_finite_values(z, "z")
  assert_bad_argument_ok(
    ncol(z) >= 1, "z must have at least one column",
    arg = "z"
  )
  if (is.null(colnames(z))) {
    colnames(z) <- paste0("z", seq_len(ncol(z)))
  }
  assert_instrument_names(colnames(z), "z")

  if (is.function(transforms)) {
    transforms <- list(transforms)
  }
  assert_bad_argument_ok(
    is.null(transforms) ||
      (is.list(transforms) &&
        length(transforms) > 0 &&
        all(vapply(transforms, is.function, logical(1)))),
    "transforms must be NULL, a function, or a list of functions",
    arg = "transforms"
  )
  if (!is.null(transforms)) {
    # Backfill missing names by INDEX: list(named = f, g) carries
    # names c("named", ""), and indexing a list by "" silently
    # returns the first unnamed element -- never iterate by name
    if (is.null(names(transforms))) {
      names(transforms) <- paste0("h", seq_along(transforms))
    } else {
      unnamed <- !nzchar(names(transforms))
      names(transforms)[unnamed] <-
        paste0("h", seq_along(transforms))[unnamed]
    }
    assert_bad_argument_ok(
      anyDuplicated(names(transforms)) == 0,
      "transform names must be unique",
      arg = "transforms"
    )
  }

  assert_bad_argument_ok(
    include_original || length(transforms) > 0,
    "include_original = FALSE requires at least one transform",
    arg = "include_original"
  )

  transform_names <- names(transforms)
  transform_blocks <- lapply(seq_along(transforms), function(t_idx) {
    as_transform_block(
      transforms[[t_idx]](z), transform_names[t_idx], nrow(z)
    )
  })
  blocks <- if (include_original) {
    c(list(z), transform_blocks)
  } else {
    transform_blocks
  }

  instruments <- do.call(cbind, blocks)
  assert_instrument_names(colnames(instruments), "instruments")
  instruments
}

#' Validate and Label One Transform Output
#'
#' @param out Raw return value of a transform
#' @param nm Transform name (used for labels and error messages)
#' @param t_obs Required number of rows
#' @return Numeric matrix with column names
#' @noRd
as_transform_block <- function(out, nm, t_obs) {
  if (is.numeric(out) && is.null(dim(out))) {
    out <- matrix(out, ncol = 1)
  }
  assert_tabular(out, paste0("transforms$", nm))
  out <- as.matrix(out)
  assert_bad_argument_ok(
    ncol(out) >= 1,
    paste0("transforms$", nm, " must return at least one column"),
    arg = "transforms"
  )
  assert_numeric_finite_values(out, paste0("transforms$", nm))
  assert_dimension_ok(
    nrow(out) == t_obs,
    paste0(
      "transforms$", nm, " must return ", t_obs,
      " rows to align with z"
    )
  )
  # Label unconditionally from the transform name: arithmetic on z
  # (e.g. z^2) keeps z's dimnames, which would alias the originals
  colnames(out) <- if (ncol(out) == 1) {
    nm
  } else {
    paste0(nm, "_", seq_len(ncol(out)))
  }
  out
}

#' Assert Instrument Column Names Are Usable as Keys
#'
#' Names label the instrument axis of the moments and key the
#' zero-row subset recipes built on top of them, so they must be
#' unique, non-empty, and non-NA.
#'
#' @param nms Character vector of column names (or NULL)
#' @param label Object label for the error message
#' @return Invisible TRUE
#' @noRd
assert_instrument_names <- function(nms, label) {
  assert_bad_argument_ok(
    !is.null(nms) && !anyNA(nms) && all(nzchar(nms)) &&
      anyDuplicated(nms) == 0,
    paste0(
      label, " must have unique, non-empty, non-NA column names"
    ),
    arg = label
  )
  invisible(TRUE)
}
