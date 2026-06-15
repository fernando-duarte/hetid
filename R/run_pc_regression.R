#' Run PC Regression
#'
#' Shared regression core for W1 and W2 residual computation.
#' Expects pre-aligned, pre-lagged inputs. Regressor labels come from
#' the matrix's own column names (sanitized for formula use); unnamed
#' or partially named input falls back to the bundled pc1..pcN names.
#'
#' @param y Numeric response vector
#' @param pcs Matrix of regressors (principal components in the
#'   bundled workflow)
#' @param n_pcs Number of leading columns to use
#'
#' @return List with residuals, fitted, coefficients, r_squared, model,
#'   complete_idx, and df_residual
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @keywords internal
run_pc_regression <- function(y, pcs, n_pcs) {
  # Subset to first n_pcs columns to avoid name
  # recycling when user supplies extra columns
  pcs <- pcs[, seq_len(n_pcs), drop = FALSE]

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

  nms <- colnames(pcs)
  pc_names <- if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    get_pc_column_names(n_pcs)
  } else {
    # Sanitize so the formula string and data.frame name mangling
    # agree for non-syntactic user names ("10y rate" -> "X10y.rate")
    make.names(nms, unique = TRUE)
  }
  colnames(pcs_clean) <- pc_names
  formula_str <- paste(
    "y ~", paste(pc_names, collapse = " + ")
  )
  reg_data <- data.frame(y = y_clean, pcs_clean)
  model <- lm(
    as.formula(formula_str),
    data = reg_data
  )

  # Aliasing guard: collinear regressors (e.g. own-lags nearly collinear with
  # PCs) make lm() drop terms to NA. Fail here with a clear error instead of
  # silently propagating an under-ranked design downstream.
  coefs <- coef(model)
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
    r_squared = summary(model)$r.squared,
    model = model,
    complete_idx = complete_idx,
    df_residual = model$df.residual
  )
}
