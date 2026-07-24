bootstrap_stage_builders <- function(stage_spec) {
  logvar <- stage_spec$log_variance
  logvar_set_boot_builders(
    scale_value = logvar$response_scale,
    logols_coef = logvar$logols_coef,
    estimator_ids = logvar$estimator_ids,
    ppml_control = logvar$ppml_control,
    harvey_control = logvar$harvey_control,
    normal_log_square_gap = logvar$normal_log_square_gap
  )
}

bootstrap_stage_eval_specs <- function(stage_spec) {
  shared_fields <- unique(c(
    names(stage_spec$frame),
    names(stage_spec$system),
    names(stage_spec$tau),
    "taus",
    "tau_grid"
  ))
  stopifnot(
    !any(names(stage_spec$mean) %in% shared_fields),
    !any(names(stage_spec$log_variance) %in% shared_fields)
  )
  list(
    mean = stage_spec$mean,
    log_variance = c(
      stage_spec$log_variance,
      list(builders = bootstrap_stage_builders(stage_spec))
    )
  )
}

set_id_boot_geometry <- function(
  est,
  gamma,
  union_taus,
  display_taus,
  quadratic_builder = tau_quadratic_system,
  table_builder = coef_interval_tables_from_quadratic
) {
  display_slots <- seq_along(display_taus) + 1L
  stopifnot(
    is.list(est), is.list(est$tau0_quadratic), is.function(quadratic_builder),
    is.function(table_builder), length(display_taus) > 0L,
    identical(union_taus, c(0, display_taus)), !anyNA(union_taus),
    all(is.finite(union_taus)), all(display_taus > 0), !anyDuplicated(display_taus),
    identical(union_taus[display_slots], display_taus)
  )
  quadratics <- vector("list", length(union_taus))
  tables <- vector("list", length(union_taus))
  quadratics[[1L]] <- est$tau0_quadratic
  for (index in seq_along(display_taus)) {
    slot <- display_slots[[index]]
    quadratics[[slot]] <- quadratic_builder(gamma, display_taus[[index]], est$moments)
    tables[[slot]] <- table_builder(quadratics[[slot]], est$beta1r, est$beta2r)
  }
  list(
    gamma = gamma, taus = union_taus, tau0_slot = 1L,
    display_slots = display_slots, quadratics = quadratics, tables = tables
  )
}

bootstrap_stage_draw_dependencies <- function() {
  list(
    estimate = estimate_set_id_system,
    geometry = set_id_boot_geometry,
    mean = set_id_boot_draw_from_est,
    volatility = logvar_set_boot_draw_from_est
  )
}

bootstrap_stage_capture <- function(run) {
  tryCatch(run(), error = function(error) conditionMessage(error))
}

bootstrap_stage_context_assert <- function(dat, context, key_col) {
  stopifnot(
    identical(names(context), c("index", "draw_id", "row_order", "quarter_keys")),
    is.numeric(context$index), bootstrap_stage_count_ok(context$draw_id),
    identical(context$row_order, rownames(dat)),
    identical(context$quarter_keys, dat[[key_col]])
  )
  invisible(TRUE)
}

bootstrap_stage_shared_state <- function(dat, stage_spec, context = NULL, dependencies) {
  if (!is.null(context)) {
    bootstrap_stage_context_assert(dat, context, stage_spec$frame$key_col)
  }
  estimate <- dependencies$estimate(dat, stage_spec$system)
  geometry <- dependencies$geometry(
    estimate, stage_spec$system$gamma, stage_spec$tau$union, stage_spec$tau$display
  )
  geometry$key_col <- stage_spec$frame$key_col
  if (!is.null(context)) geometry$draw_context <- context
  stopifnot(
    identical(geometry$gamma, stage_spec$system$gamma),
    identical(geometry$taus, stage_spec$tau$union),
    identical(geometry$display_slots, seq_along(stage_spec$tau$display) + 1L),
    identical(geometry$key_col, stage_spec$frame$key_col)
  )
  list(estimate = estimate, geometry = geometry)
}

bootstrap_stage_primary_draw <- function(
  dat,
  stage_spec,
  draw_context,
  dependencies = bootstrap_stage_draw_dependencies()
) {
  eval_specs <- bootstrap_stage_eval_specs(stage_spec)
  shared <- bootstrap_stage_capture(function() {
    bootstrap_stage_shared_state(dat, stage_spec, draw_context, dependencies)
  })
  if (is.character(shared)) {
    return(list(mean = shared, volatility = shared))
  }
  mean <- bootstrap_stage_capture(function() {
    dependencies$mean(shared$estimate, shared$geometry, eval_specs$mean)
  })
  volatility <- bootstrap_stage_capture(function() {
    dependencies$volatility(
      dat, shared$estimate, shared$geometry, eval_specs$log_variance
    )
  })
  list(mean = mean, volatility = volatility)
}

bootstrap_stage_volatility_evaluate <- function(
  dat,
  stage_spec,
  draw_context = NULL,
  dependencies = bootstrap_stage_draw_dependencies()
) {
  eval_specs <- bootstrap_stage_eval_specs(stage_spec)
  shared <- bootstrap_stage_shared_state(
    dat, stage_spec, draw_context, dependencies
  )
  dependencies$volatility(
    dat, shared$estimate, shared$geometry, eval_specs$log_variance
  )
}

bootstrap_stage_volatility_anchor <- function(
  dat,
  stage_spec,
  dependencies = bootstrap_stage_draw_dependencies()
) {
  bootstrap_stage_volatility_evaluate(
    dat, stage_spec,
    dependencies = dependencies
  )
}

bootstrap_stage_volatility_draw <- function(
  dat,
  stage_spec,
  draw_context = NULL,
  dependencies = bootstrap_stage_draw_dependencies()
) {
  bootstrap_stage_capture(function() {
    bootstrap_stage_volatility_evaluate(
      dat, stage_spec, draw_context, dependencies
    )
  })
}

bootstrap_stage_indexed_input <- function(index, draw_id, stage_spec) {
  frame <- stage_spec$frame
  dat <- frame$data[index, , drop = FALSE]
  context <- list(
    index = index, draw_id = draw_id, row_order = rownames(dat),
    quarter_keys = dat[[frame$key_col]]
  )
  list(data = dat, context = context)
}

bootstrap_stage_primary_indexed_draw <- function(index, draw_id, stage_spec) {
  input <- bootstrap_stage_indexed_input(index, draw_id, stage_spec)
  bootstrap_stage_primary_draw(input$data, stage_spec, input$context)
}

bootstrap_stage_volatility_indexed_draw <- function(index, draw_id, stage_spec) {
  input <- bootstrap_stage_indexed_input(index, draw_id, stage_spec)
  bootstrap_stage_volatility_draw(input$data, stage_spec, input$context)
}

bootstrap_stage_project_raw <- function(draws, branch) {
  stopifnot(branch %in% c("mean", "volatility"))
  lapply(draws, function(draw) {
    if (is.character(draw)) {
      return(draw)
    }
    stopifnot(identical(names(draw), c("mean", "volatility")))
    draw[[branch]]
  })
}
