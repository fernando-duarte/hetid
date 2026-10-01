#' Run PC Regression
#'
#' Fits an ordinary least-squares regression with an intercept for
#' \eqn{\omega_1} and \eqn{\omega_2} residual computation.
#'
#' @details Inputs must already be aligned; the caller applies any required
#' leads or lags before fitting.
#' Rows with \code{NA} or \code{NaN} in \code{y} or a selected regressor
#' are omitted jointly; missing values in unused columns do not affect the fit.
#' Response and selected regressors must be numeric. Infinite values on
#' complete rows signal a \code{hetid_error_bad_argument} condition.
#' At least \code{n_pcs + 2} complete observations are required.
#' Regressor labels come from the selected columns' names, sanitized with
#' \code{make.names(..., unique = TRUE)}. If any selected name is missing or
#' empty, or column names are absent, all selected columns use the names from
#' \code{\link{get_pc_column_names}} instead.
#' Formula-reserved names \code{...} and \code{..N} use collision-free internal
#' names in the fitted model. Its \code{hetid_regressor_names} attribute maps
#' internal names to sanitized public labels; prediction data use internal names.
#' The returned coefficient vector always uses the public labels.
#'
#' Too few complete observations signal a
#' \code{hetid_error_insufficient_data} condition. A selected regressor named
#' \code{y} after sanitization signals a \code{hetid_error_bad_argument}
#' condition. An aliased coefficient signals a \code{hetid_error} condition.
#' Other invalid inputs can raise errors from subsetting or \code{stats::lm()}.
#' Computing R-squared can warn when the fit is essentially perfect.
#'
#' @param y Numeric response vector with one element per row of \code{pcs}.
#' @param pcs Numeric regressor matrix with rows aligned to \code{y}.
#'   In the bundled workflow, regressors include principal components of
#'   nominal financial asset returns and may include additional controls.
#' @param n_pcs Positive integer number of leading regressor columns to use,
#'   no greater than \code{ncol(pcs)}. This counts all selected regressors,
#'   including controls, rather than only principal components.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{residuals, fitted}{Named numeric vectors for complete observations,
#'       in their original order, without padding omitted rows.}
#'     \item{coefficients}{Named numeric vector containing the intercept and
#'       selected regressor coefficients.}
#'     \item{r_squared}{Numeric scalar giving the regression's R-squared,
#'       which can be \code{NaN} for a constant response.}
#'     \item{model}{The fitted \code{lm} object.}
#'     \item{complete_idx}{Logical vector of length \code{length(y)} indicating
#'       the observations used in the fit.}
#'     \item{df_residual}{Integer residual degrees of freedom.}
#'   }
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @keywords internal
run_pc_regression <- function(y, pcs, n_pcs) {
  pcs <- pcs[, seq_len(n_pcs), drop = FALSE]
  assert_bad_argument_ok(is.numeric(y), "y must contain only numeric values", arg = "y")
  numeric_pcs <- if (is.data.frame(pcs)) {
    all(vapply(pcs, is.numeric, logical(1L)))
  } else {
    is.numeric(pcs)
  }
  assert_bad_argument_ok(
    numeric_pcs, "pcs must contain only numeric values",
    arg = "pcs"
  )

  complete_idx <- complete.cases(y, pcs)
  n_complete <- sum(complete_idx)
  min_obs_for_regression <- min_obs_for_pc_regression(n_pcs)
  if (n_complete < min_obs_for_regression) {
    stop_insufficient_data(paste0(
      "Insufficient complete observations for PC regression: got ",
      n_complete, ", need at least ", min_obs_for_regression,
      " (n_reg + 2)"
    ))
  }
  y_clean <- y[complete_idx]
  pcs_clean <- pcs[complete_idx, , drop = FALSE]
  assert_numeric_finite_values(y_clean, "y")
  assert_numeric_finite_values(as.matrix(pcs_clean), "pcs")

  nms <- colnames(pcs)
  pc_names <- if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    get_pc_column_names(n_pcs)
  } else {
    # sanitize non-syntactic names so formula and data.frame agree
    make.names(nms, unique = TRUE)
  }
  # data.frame() renames a regressor named y, and model.matrix() drops the response
  # from the predictors; reject the collision to avoid losing a regressor
  if ("y" %in% pc_names) {
    stop_bad_argument(
      "pcs must not contain a column named \"y\": it collides with the response",
      arg = "pcs"
    )
  }
  fit_names <- pc_names
  reserved <- pc_names == "..." | grepl("^\\.\\.[0-9]+$", pc_names)
  if (any(reserved)) {
    aliases <- paste0(".hetid_x", which(reserved))
    fit_names[reserved] <- utils::tail(make.unique(c(pc_names, aliases)), sum(reserved))
  }
  colnames(pcs_clean) <- fit_names
  formula_str <- paste("y ~", paste(fit_names, collapse = " + "))
  reg_data <- data.frame(y = y_clean, pcs_clean)
  model <- lm(
    as.formula(formula_str),
    data = reg_data
  )
  if (any(reserved)) {
    attr(model, "hetid_regressor_names") <- stats::setNames(pc_names, fit_names)
  }

  # reject collinear regressors before passing NA coefficients downstream
  coefs <- coef(model)
  names(coefs) <- c("(Intercept)", pc_names)
  if (anyNA(coefs)) {
    aliased <- names(coefs)[is.na(coefs)]
    stop_hetid(paste0(
      "Rank-deficient regression design: aliased coefficient(s) ",
      paste(aliased, collapse = ", "),
      ". The conditioning columns are collinear."
    ))
  }

  list(
    residuals = residuals(model),
    fitted = fitted(model),
    coefficients = coefs,
    r_squared = summary(model)[["r.squared"]],
    model = model,
    complete_idx = complete_idx,
    df_residual = model[["df.residual"]]
  )
}
