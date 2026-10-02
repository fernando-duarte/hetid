#' Assert Slack Values Are Finite and in [0, 1)
#'
#' Single source of truth for the tau range rule, shared by
#' \code{validate_quadratic_inputs()}, \code{as_tau_list()} (both its list and numeric
#' branches), \code{compute_identified_set_box()}, and \code{linear_bounds_frame()}.
#'
#' @param tau Numeric vector of dimensionless slacks in \code{[0, 1)}.
#'   Missing and non-finite values are rejected; an empty vector is valid.
#' @return Invisible \code{TRUE}; invalid values signal \code{hetid_error_bad_argument}.
#' @noRd
assert_tau_values_ok <- function(tau) {
  assert_bad_argument_ok(
    all(is.finite(tau)),
    "All elements of tau must be finite (no NA, NaN, or Inf)",
    arg = "tau"
  )
  assert_bad_argument_ok(
    all(tau >= 0 & tau < 1),
    "All elements of tau must be in [0, 1)",
    arg = "tau"
  )
  invisible(TRUE)
}

#' Assert NULL (or Empty) Entries at Unconstrained Columns
#'
#' Shared guard for the per-component list validators (\code{as_tau_list},
#' \code{as_lambda_list}, \code{assert_support_list}): every column not
#' in \code{maturities} must hold an empty entry. \code{predicate_desc}
#' and \code{suffix} reproduce each caller's exact message;
#' \code{is_empty} swaps the emptiness test (tau also accepts
#' any zero-length entry, the others require NULL).
#'
#' @param x Candidate per-component list of length \code{n_components}.
#' @param n_components Integer system width (theta-axis dimension).
#' @param maturities Integer indices of constrained system columns.
#' @param arg Character argument name stored in the error condition.
#' @param predicate_desc Character wording for the empty state, default \code{"NULL"}.
#' @param suffix Character message suffix, empty by default.
#' @param is_empty Function returning one logical value per entry, default \code{is.null}.
#' @return Invisible \code{TRUE}; nonempty entries signal \code{hetid_error_bad_argument}.
#' @noRd
assert_null_at_unconstrained <- function(x, n_components, maturities, arg,
                                         predicate_desc = "NULL",
                                         suffix = "",
                                         is_empty = is.null) {
  unconstrained <- setdiff(seq_len(n_components), maturities)
  bad_extra <- unconstrained[
    !vapply(x[unconstrained], is_empty, logical(1))
  ]
  assert_bad_argument_ok(
    length(bad_extra) == 0,
    paste0(
      arg, " must be ", predicate_desc,
      " at unconstrained system column(s) ",
      paste(bad_extra, collapse = ", "), suffix
    ),
    arg = arg
  )
  invisible(TRUE)
}

#' Coerce Slacks to the Per-Component List Form
#'
#' A scalar replicates across every constraint; a numeric vector of length
#' \code{n_components} replicates \code{tau[i]} across system column i's K_i
#' combinations. A list must carry K_i numeric values per constrained column
#' and NULL or any zero-length entry at unconstrained columns. Numeric list
#' entries retain their dimensions and names. Slacks must be finite and in
#' \code{[0, 1)}. Flat numeric inputs of other lengths or with dimensions are
#' rejected; total combination count does not determine their interpretation.
#'
#' @param tau Numeric scalar, numeric vector of length \code{n_components}, or list.
#' @param lambda_list Validated list of weight matrices from \code{as_lambda_list()}.
#' @param moments Validated \code{hetid_moments} object carrying both axes.
#' @return List indexed by system column, of length \code{n_components}.
#'   Numeric inputs produce vectors and empty numerics at unconstrained
#'   columns; a valid list is returned unchanged, including names and empty entries.
#' @noRd
as_tau_list <- function(tau, lambda_list, moments) {
  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  k_per <- integer(n_components)
  for (i in maturities) {
    k_per[i] <- ncol(lambda_list[[i]])
  }

  if (is.numeric(tau) && is.null(dim(tau))) {
    return(promote_numeric_tau(tau, k_per, n_components))
  }

  assert_bad_argument_ok(
    is.list(tau) && length(tau) == n_components,
    paste0(
      "tau must be a scalar, a length-", n_components,
      " numeric vector, or a list of length ", n_components
    ),
    arg = "tau"
  )
  assert_null_at_unconstrained(
    tau, n_components, maturities, "tau",
    predicate_desc = "NULL or zero-length",
    is_empty = function(v) is.null(v) || length(v) == 0
  )
  for (i in maturities) {
    assert_bad_argument_ok(
      is.numeric(tau[[i]]),
      paste0("tau[[", i, "]] must be numeric"),
      arg = "tau"
    )
    assert_tau_values_ok(tau[[i]])
    assert_dimension_ok(
      length(tau[[i]]) == k_per[i],
      paste0(
        "tau[[", i, "]] must have one slack per combination (K = ",
        k_per[i], ")"
      )
    )
  }
  tau
}

#' Promote a Numeric tau to the Per-Component List Form
#'
#' Worker for the dimensionless-numeric branch of
#' \code{as_tau_list()}: a scalar replicates across every constraint,
#' a vector of length \code{n_components} replicates \code{tau[i]} across column i's
#' combinations.
#'
#' @param tau Numeric scalar or vector of length \code{n_components}, without dimensions.
#' @param k_per Integer combination counts per system column, zero if unconstrained.
#' @param n_components Integer theta-axis dimension.
#' @return Unnamed list of length \code{n_components}, with K_i slacks per column
#'   and zero-length numeric vectors at unconstrained columns.
#' @noRd
promote_numeric_tau <- function(tau, k_per, n_components) {
  assert_tau_values_ok(tau)
  if (length(tau) == 1) {
    tau <- rep(tau, n_components)
  }
  assert_dimension_ok(
    length(tau) == n_components,
    paste0(
      "numeric tau must be a scalar or have length n_components (",
      n_components, "); per-combination slacks must be given as ",
      "a list of length-K_i vectors"
    )
  )
  lapply(seq_len(n_components), function(i) {
    rep(tau[i], k_per[i])
  })
}

#' Assert sigma_i_sq Is Finite and Strictly Positive
#'
#' Shared by \code{validate_quadratic_inputs()} and
#' \code{build_general_quadratic_system()}.
#'
#' @param sigma_i_sq Numeric vector from the moments container, in constraint-axis order.
#' @param maturities Integer system-column indices in the same order, for error messages.
#' @return Invisible \code{TRUE}; invalid variances signal \code{hetid_error_bad_argument}
#'   naming their system-column indices.
#' @noRd
assert_sigma_positive <- function(sigma_i_sq, maturities) {
  bad_sigma <- which(
    !is.finite(sigma_i_sq) | sigma_i_sq <= 0
  )
  assert_bad_argument_ok(
    length(bad_sigma) == 0,
    paste0(
      "sigma_i_sq is non-positive, non-finite, or NA ",
      "for maturity/maturities ",
      paste(maturities[bad_sigma], collapse = ", "),
      ". Cannot compute identified set -- ",
      "insufficient heteroskedasticity."
    ),
    arg = "sigma_i_sq"
  )
  invisible(TRUE)
}
