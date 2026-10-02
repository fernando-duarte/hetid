# Canonical identities and capabilities for log-variance estimators.

PAPER_LOGVAR_RESPONSE_SCALES <- c(
  variance = "variance",
  log = "log"
)

.paper_logvar_spec <- function(
  key,
  display_name,
  result_object,
  builder,
  response_scale,
  display,
  artifacts,
  capabilities,
  dependencies = character(0),
  budget_policy
) {
  stopifnot(
    response_scale %in% PAPER_LOGVAR_RESPONSE_SCALES,
    identical(
      names(display),
      c("title_quantity", "y_label")
    )
  )
  list(
    key = key,
    display_name = display_name,
    result_object = result_object,
    builder = builder,
    response_scale = response_scale,
    display = display,
    artifacts = artifacts,
    capabilities = capabilities,
    dependencies = dependencies,
    budget_policy = budget_policy
  )
}

# The display strings the conditional-volatility estimators share
PAPER_LOGVAR_VOLATILITY_DISPLAY <- list(
  title_quantity =
    "fitted conditional residual volatility",
  y_label = paste(
    "Conditional SD of consumption-growth residual",
    "(percentage points)"
  )
)

PAPER_LOGVAR_PRIMARY_ESTIMATORS <- list(
  ppml = .paper_logvar_spec(
    key = "ppml",
    display_name = "PPML",
    result_object = "log_var_eq_ppml",
    builder = "logvar_ppml_estimator",
    response_scale = PAPER_LOGVAR_RESPONSE_SCALES[["variance"]],
    display = PAPER_LOGVAR_VOLATILITY_DISPLAY,
    artifacts = c(
      table = "structural_var_estimators_table",
      bounds = "ppml_bounds_figure",
      fitted_volatility =
        "ppml_fitted_volatility_figure"
    ),
    capabilities = c(
      "bounds_by_tau", "table", "set_bootstrap",
      "fitted_volatility", "engine_envelope"
    ),
    budget_policy = "primary"
  ),
  harvey = .paper_logvar_spec(
    key = "harvey",
    display_name = "Harvey",
    result_object = "log_var_eq_harvey",
    builder = "logvar_harvey_estimator",
    response_scale = PAPER_LOGVAR_RESPONSE_SCALES[["variance"]],
    display = PAPER_LOGVAR_VOLATILITY_DISPLAY,
    artifacts = c(
      table = "structural_var_estimators_table",
      bounds = "harvey_bounds_figure",
      fitted_volatility =
        "harvey_fitted_volatility_figure"
    ),
    capabilities = c(
      "bounds_by_tau", "table", "set_bootstrap",
      "fitted_volatility", "engine_envelope"
    ),
    dependencies = "ppml",
    budget_policy = "primary"
  )
)

# extension estimators (no set bootstrap) live in their own file
paper_source_once(paper_path(
  "config",
  "logvar_estimator_extensions.R"
))

PAPER_LOGVAR_ESTIMATORS <- c(
  PAPER_LOGVAR_PRIMARY_ESTIMATORS,
  PAPER_LOGVAR_ESTIMATOR_EXTENSIONS
)

paper_logvar_estimator_ids <- function(
  capability = NULL,
  primary = NULL
) {
  specs <- PAPER_LOGVAR_ESTIMATORS
  keep <- rep(TRUE, length(specs))
  if (!is.null(capability)) {
    keep <- keep & vapply(
      specs,
      function(spec) capability %in% spec$capabilities,
      logical(1)
    )
  }
  if (!is.null(primary)) {
    primary_ids <- names(PAPER_LOGVAR_PRIMARY_ESTIMATORS)
    keep <- keep &
      (names(specs) %in% primary_ids) == isTRUE(primary)
  }
  names(specs)[keep]
}

paper_logvar_estimator_spec <- function(key) {
  spec <- PAPER_LOGVAR_ESTIMATORS[[key]]
  if (is.null(spec)) {
    stop(
      sprintf("Unknown log-variance estimator: %s", key),
      call. = FALSE
    )
  }
  spec
}

paper_source_once(paper_path(
  "config",
  "logvar_estimator_access.R"
))
