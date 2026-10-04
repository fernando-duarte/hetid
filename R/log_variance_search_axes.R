lv_set_axis <- function(value, labels, arg, finite = TRUE) {
  assert_bad_argument_ok(is.numeric(value) && is.null(dim(value)) &&
    length(value) == length(labels), paste(arg, "has the wrong numeric vector shape"), arg)
  assert_bad_argument_ok(
    is.null(names(value)) || identical(names(value), labels),
    paste("names of", arg, "must match the declared axis in order"), arg
  )
  if (finite) assert_numeric_finite_values(value, arg)
  invisible(TRUE)
}

lv_set_matrix_axes <- function(value, rows, columns, arg) {
  assert_bad_argument_ok(
    is.matrix(value) && is.numeric(value) &&
      identical(dim(value), c(length(rows), length(columns))),
    paste(arg, "has the wrong matrix shape"), arg
  )
  assert_bad_argument_ok(
    is.null(rownames(value)) || identical(rownames(value), rows),
    paste(arg, "row names must match the coefficient axis"), arg
  )
  assert_bad_argument_ok(
    is.null(colnames(value)) || identical(colnames(value), columns),
    paste(arg, "column names must match the theta axis"), arg
  )
  invisible(TRUE)
}

lv_set_checked_fit <- function(fit, labels) {
  assert_bad_argument_ok(is.list(fit), "fit_at_b must return a fit list", "estimator")
  if (identical(fit$fit_status, "ok") && isTRUE(fit$converged)) {
    lv_set_axis(fit$coef, labels, "fit coefficients", finite = FALSE)
  }
  fit
}

lv_set_checked_jacobian <- function(estimator, b, fit) {
  jac <- estimator$jacobian_at_b(b, fit)
  if (is.null(jac)) {
    return(matrix(NaN, length(estimator$coef_labels), length(b)))
  }
  lv_set_matrix_axes(jac, estimator$coef_labels, estimator$theta_labels, "Jacobian")
  jac
}

lv_set_check_extra_starts <- function(starts, labels) {
  if (is.null(starts)) {
    return(invisible(TRUE))
  }
  if (is.numeric(starts)) {
    lv_set_axis(starts, labels, "extra_starts")
  } else {
    assert_bad_argument_ok(
      is.list(starts), "extra_starts must contain theta vectors",
      "extra_starts"
    )
    for (candidate in starts) lv_set_check_extra_starts(candidate, labels)
  }
  invisible(TRUE)
}

lv_set_check_scan <- function(found, estimator) {
  assert_bad_argument_ok(is.list(found), "scan_grid must return a list", "estimator")
  lv_set_validate_scan_count(found$n_failed)
  if (is.null(found$min)) {
    return(found)
  }
  for (side in c("min", "max")) {
    lv_set_axis(found[[side]], estimator$coef_labels, "scan values", finite = FALSE)
    lv_set_matrix_axes(
      found[[paste0("arg_", side)]], estimator$coef_labels,
      estimator$theta_labels, "scan attaining points"
    )
    pool <- found[[paste0("arg_", side, "_pool")]]
    if (is.null(pool)) next
    assert_bad_argument_ok(is.list(pool), "scan pools must be lists", "estimator")
    for (starts in pool) {
      assert_bad_argument_ok(
        is.null(starts) || is.list(starts),
        "each coefficient pool must be a list of theta vectors", "estimator"
      )
      for (point in starts) lv_set_axis(point, estimator$theta_labels, "scan pool start")
    }
  }
  found
}

lv_set_validate_scan_count <- function(count) {
  assert_scalar_finite(count, "scan_grid n_failed", arg = "estimator")
  assert_bad_argument_ok(
    !is.complex(count) && is.null(dim(count)),
    "scan_grid n_failed must be a real scalar without dimensions", "estimator"
  )
  assert_bad_argument_ok(
    count >= 0 && count == floor(count),
    "scan_grid n_failed must be a nonnegative integer", "estimator"
  )
  invisible(TRUE)
}

lv_set_labels <- function(labels, arg) {
  assert_bad_argument_ok(
    is.character(labels) && is.null(dim(labels)) && length(labels) > 0L &&
      !anyNA(labels) && all(nzchar(labels)) && !anyDuplicated(labels),
    paste(arg, "must contain unique nonempty labels"), arg
  )
  invisible(TRUE)
}

lv_set_side_flags <- function(flags, labels, arg) {
  assert_bad_argument_ok(
    is.logical(flags) && is.null(dim(flags)) &&
      length(flags) == length(labels) && !anyNA(flags),
    paste(arg, "must be a nonmissing logical vector on the coefficient axis"), "estimator"
  )
  assert_bad_argument_ok(
    is.null(names(flags)) || identical(names(flags), labels),
    paste(arg, "names must match the coefficient axis in order"), "estimator"
  )
  invisible(TRUE)
}

lv_set_check_selector <- function(selected, mesh) {
  lv_set_assert(is.list(selected))
  lv_set_assert(
    is.matrix(selected$grid), is.numeric(selected$grid),
    nrow(selected$grid) >= 1L, ncol(selected$grid) == ncol(mesh)
  )
  id <- selected$selector_id
  lv_set_assert(is.character(id) && is.null(dim(id)) && length(id) == 1L &&
    !is.na(id) && nzchar(id))
  keys <- lv_set_b_keys(selected$grid)
  lv_set_assert(!anyDuplicated(keys), all(keys %in% lv_set_b_keys(mesh)))
  selected
}

# lv_set_b_key() for every row of a matrix at once
lv_set_b_keys <- function(mesh) {
  formatted <- matrix(sprintf("%.17g", mesh), nrow(mesh))
  do.call(paste, c(lapply(seq_len(ncol(formatted)), function(j) formatted[, j]), sep = "|"))
}
