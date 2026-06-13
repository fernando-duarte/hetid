#' Assert Slack Values Are Finite and in [0, 1)
#'
#' Single source of truth for the tau range rule, shared by
#' \code{validate_quadratic_inputs()} (legacy path) and
#' \code{as_tau_list()} (general path).
#'
#' @param tau Numeric vector of slacks
#' @return Invisible TRUE
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
#' zero-length numerics, the others require NULL).
#'
#' @param x Candidate per-component list
#' @param n_components System width
#' @param maturities Constrained system columns
#' @param arg Condition argument name
#' @param predicate_desc Wording for the required-empty state
#' @param suffix Trailing clause appended after the column list
#' @param is_empty Per-element emptiness predicate
#' @return Invisible TRUE if valid, stops otherwise
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
#' Promotion rules: a scalar replicates across every constraint; a
#' length-I numeric replicates \code{tau[i]} across component i's K_i
#' combinations (legacy semantics); a list must carry a length-K_i
#' numeric per constrained column and NULL or zero-length entries at
#' unconstrained columns. A flat numeric of any other length
#' (including sum(K_i)) or with dimensions is rejected as ambiguous.
#'
#' @param tau Scalar, length-I numeric, or list
#' @param lambda_list Output of \code{as_lambda_list()}
#' @param moments A \code{hetid_moments} object
#' @return List of length \code{n_components}
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
#' a length-I vector replicates \code{tau[i]} across component i's
#' combinations.
#'
#' @param tau Scalar or length-I numeric (the caller's branch
#'   condition guarantees a dimensionless non-list numeric)
#' @param k_per Per-column combination counts
#' @param n_components Theta-axis dimension
#' @return List of length \code{n_components}
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
#' @param sigma_i_sq Numeric vector from the moments container
#' @param maturities Maturity vector for the error message
#' @return Invisible TRUE
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
