# Prediction rows are explicitly aligned to the retained coefficient axis.
log_variance_prediction_design <- function(object, newdata, include_intercept, dates) {
  if (is.null(newdata)) {
    design <- object$x_design
    if (is.null(dates)) dates <- object$dates
  } else {
    assert_tabular(newdata, "newdata")
    newdata <- as.matrix(newdata)
    assert_bad_argument_ok(is.numeric(newdata), "newdata must be numeric", arg = "newdata")
    assert_numeric_finite_values(newdata, "newdata")
    coef_labels <- colnames(object$x_design)[-1L]
    assert_dimension_ok(ncol(newdata) == length(coef_labels), "newdata regressor count differs")
    if (!is.null(colnames(newdata))) {
      assert_bad_argument_ok(identical(colnames(newdata), coef_labels),
        "newdata columns must match training regressors in order",
        arg = "newdata"
      )
    }
    design <- cbind(rep(1, nrow(newdata)), newdata)
    colnames(design) <- colnames(object$x_design)
  }
  if (!include_intercept) design[, 1L] <- 0
  validate_log_variance_dates(dates, nrow(design))
  list(design = design, dates = dates)
}

# Keep missing geometry/fitting outcomes distinct from numeric projection failures.
empty_log_variance_envelope <- function(object, input, type, include_intercept) {
  n <- nrow(input$design)
  missing_values <- rep(NA_real_, n)
  bounds <- data.frame(
    row = seq_len(n), lower = missing_values, upper = missing_values,
    eta_lower = missing_values, eta_upper = missing_values,
    lower_status = rep(object$reason, n), upper_status = rep(object$reason, n)
  )
  if (!is.null(input$dates)) bounds <- cbind(data.frame(date = input$dates), bounds)
  witnesses <- matrix(NA_real_, n, ncol(object$candidates),
    dimnames = list(rownames(input$design), colnames(object$candidates))
  )
  list(
    bounds = bounds, arg_lower = witnesses, arg_upper = witnesses,
    candidate_lower = rep(NA_integer_, n), candidate_upper = rep(NA_integer_, n),
    type = type, include_intercept = include_intercept,
    design = input$design, dates = input$dates, sample = object
  )
}

transform_sampled_log_variance <- function(eta, type) {
  failure <- rep(NA_character_, length(eta))
  if (type == "log_variance") {
    return(list(value = eta, failure = failure))
  }
  value <- exp(eta * if (type == "variance") 1 else 0.5)
  failure[is.finite(eta) & !is.finite(value)] <- "transform_overflow"
  failure[is.finite(eta) & value == 0] <- "transform_underflow"
  value[!is.na(failure)] <- NA_real_
  list(value = value, failure = failure)
}

#' Print Retained Log-Variance Samples
#' @param x A \code{hetid_log_variance_sample}.
#' @param ... Unused, for method consistency.
#' @return The object, invisibly.
#' @export
print.hetid_log_variance_sample <- function(x, ...) {
  validate_hetid_log_variance_sample(x)
  cat("<hetid_log_variance_sample>\n")
  cat("  estimator:", x$estimator, "\n")
  cat("  candidates:", nrow(x$candidates), " failed:", attr(x$bounds, "n_failed"), "\n")
  cat("  sampling:", x$reason, "\n")
  print(x$bounds)
  invisible(x)
}
