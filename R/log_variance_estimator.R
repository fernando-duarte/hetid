#' Log-Variance Estimator Registry and Design Builder
#'
#' The registry is the sole owner of the valid-estimator set: callers take a
#' plain \code{estimator} string and hand it here, so adding an estimator is
#' one branch in \code{log_variance_estimator_specs()} plus its worker files,
#' with no caller changes. \code{se_types} lives on the spec because a future
#' estimator carries its own SE variants.
#'
#' @name log_variance_estimator_registry
#' @keywords internal
NULL

# label of the intercept column the design builder prepends; also the name the
# recovered coefficient carries through the whole log-variance chain
LOG_VARIANCE_INTERCEPT_LABEL <- "(Intercept)"

#' Supported Log-Variance Estimator Specs
#'
#' Built inside a function, not stored as a package-level constant: the
#' \code{fit_response} slot points at a worker defined in a later-sourced
#' file, which would not exist yet at build time.
#'
#' @noRd
log_variance_estimator_specs <- function() {
  list(
    ppml = list(
      id = "ppml",
      label = "PPML (quasi-Poisson, log link)",
      fit_response = ppml_fit_response,
      vcov = ppml_vcov_variants,
      se_types = LOG_VARIANCE_CONTROL$SE_TYPES
    )
  )
}

#' Look Up a Log-Variance Estimator Spec
#'
#' @param estimator Single string naming the estimator (\code{"ppml"})
#'
#' @return A list with elements \code{id}, \code{label},
#'   \code{fit_response}, \code{vcov}, and \code{se_types}
#' @keywords internal
log_variance_estimator <- function(estimator) {
  specs <- log_variance_estimator_specs()
  valid <- names(specs)
  assert_bad_argument_ok(
    is.character(estimator) && length(estimator) == 1L &&
      !is.na(estimator) && estimator %in% valid,
    paste0("estimator must be one of: ", paste(valid, collapse = ", ")),
    arg = "estimator"
  )
  specs[[estimator]]
}

#' Build the Log-Variance Design Matrix
#'
#' Prepends the intercept column to the volatility regressors. Column labels
#' are validated on the \emph{final} design, so an \code{x} column already
#' named \code{"(Intercept)"} is a collision and errors: duplicate or blank
#' labels would later corrupt coefficient identity and the SE frame.
#'
#' @param x Numeric matrix (or object coercible by
#'   \code{\link[base]{as.matrix}}) of volatility regressors, without an
#'   intercept column. Unnamed columns fall back to \code{pc1..pcK}.
#'
#' @return Numeric matrix with \code{ncol(x) + 1} columns, the first the
#'   intercept, with unique non-blank column labels
#' @keywords internal
log_variance_design <- function(x) {
  x <- as.matrix(x)
  if (is.null(colnames(x))) {
    colnames(x) <- get_pc_column_names(ncol(x))
  }
  design <- cbind(rep(1, nrow(x)), x)
  colnames(design) <- c(LOG_VARIANCE_INTERCEPT_LABEL, colnames(x))
  design_labels <- colnames(design)
  assert_bad_argument_ok(
    is.numeric(design) && !anyNA(design_labels) &&
      all(nzchar(design_labels)) && !anyDuplicated(design_labels),
    paste0(
      "design column labels must be non-missing, non-blank, and unique ",
      "(the intercept column is named ", LOG_VARIANCE_INTERCEPT_LABEL, ")"
    ),
    arg = "x"
  )
  design
}
