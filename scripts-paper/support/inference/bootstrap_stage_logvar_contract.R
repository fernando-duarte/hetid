bootstrap_stage_logvar_contract <- function(spec) {
  stopifnot(
    identical(spec$complete_case_policy, BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY),
    bootstrap_stage_graph_ok(spec$estimator_ids, spec$estimator_dependencies)
  )
  invisible(TRUE)
}

bootstrap_stage_logvar_rows <- function(dat, est, spec, key_col) {
  bootstrap_stage_logvar_contract(spec)
  stopifnot(key_col %in% names(dat), all(spec$pc_cols %in% names(dat)))
  pc_data <- dat[, spec$pc_cols, drop = FALSE]
  complete <- stats::complete.cases(pc_data)
  rows <- list(
    w1 = est$w1[complete],
    w2 = est$w2[complete, , drop = FALSE],
    key = dat[[key_col]][complete],
    pc_data = pc_data[complete, , drop = FALSE]
  )
  stopifnot(identical(names(rows), spec$complete_case_policy$subset_roles))
  rows
}

logvar_set_boot_compat_spec <- function(spec) {
  fill <- function(record, name, value) {
    if (!name %in% names(record)) record[[name]] <- value
    record
  }
  spec <- fill(spec, "key_col", PAPER_ANALYSIS_CONTRACT$model$key_col)
  spec <- fill(spec, "complete_case_policy", BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY)
  spec <- fill(spec, "estimator_dependencies", stats::setNames(
    lapply(spec$estimator_ids, function(id) paper_logvar_estimator_spec(id)$dependencies),
    spec$estimator_ids
  ))
  spec <- fill(
    spec,
    "pc_preprocessing",
    PAPER_ANALYSIS_CONTRACT$model$preprocessing$return_pc
  )
  spec <- fill(spec, "search_control", LOGVAR_SEARCH_CONTROL)
  spec <- fill(spec, "ppml_control", LOGVAR_PPML_CONTROL)
  fill(spec, "harvey_control", LOGVAR_HARVEY_CONTROL)
}

bootstrap_stage_cache_side_values_ok <- function(value, status, side) {
  stopifnot(side %in% c("lower", "upper"))
  bounded <- status == PAPER_ENDPOINT_STATUS[["bounded"]]
  unbounded <- status == PAPER_ENDPOINT_STATUS[["unbounded"]]
  unreliable <- status == PAPER_ENDPOINT_STATUS[["unreliable"]]
  failed <- status == PAPER_ENDPOINT_STATUS[["failed"]]
  missing <- is.na(value) & !is.nan(value)
  directed <- if (identical(side, "lower")) {
    is.infinite(value) & value < 0
  } else {
    is.infinite(value) & value > 0
  }
  allowed <- (bounded & is.finite(value)) |
    (unbounded & (missing | directed)) |
    (unreliable & (missing | is.finite(value) | directed)) |
    (failed & missing)
  !any(is.nan(value)) && all(allowed)
}

bootstrap_stage_cache_interval_order_ok <- function(
  lower, upper, lower_status, upper_status
) {
  bounded <- lower_status == PAPER_ENDPOINT_STATUS[["bounded"]] &
    upper_status == PAPER_ENDPOINT_STATUS[["bounded"]]
  all(lower[bounded] <= upper[bounded])
}

bootstrap_stage_cache_failure_masks <- function(
  lower, upper, lower_status, upper_status
) {
  as_rows <- function(value) {
    if (is.null(dim(value))) matrix(value, nrow = 1L) else value
  }
  lower <- as_rows(lower)
  upper <- as_rows(upper)
  lower_status <- as_rows(lower_status)
  upper_status <- as_rows(upper_status)
  failed <- PAPER_ENDPOINT_STATUS[["failed"]]
  any_failed <- apply(
    lower_status == failed | upper_status == failed,
    1L,
    any
  )
  fully_failed <- apply(
    lower_status == failed & upper_status == failed &
      is.na(lower) & is.na(upper),
    1L,
    all
  )
  list(any = any_failed, full = fully_failed)
}
