# Helper function: declare the structural inference controls from the paper's owners
# rows are forecast origins t, one quarter before the paper's response quarters,
# so the window is the paper window shifted back one quarter. n_grid and
# n_points are the public box and PPML sampling knobs; the other defaults are
# the paper's bootstrap, inference and reporting controls
structural_inference_settings <- function(
  n_draws = boot_reps, seed = boot_seed, block_length = NULL,
  n_grid = hetid::IDENTIFIED_SET_CONTROL$N_GRID, n_points = 20L,
  z_col = paper_instrument_choice()$column
) {
  # the ported calculation estimates beta2R, which is specification B only
  published <- vapply(c("mean", "volatility"), paper_published_spec, "")
  if (!all(published == "B")) {
    stop(sprintf(
      "structural inference requires published spec B (estimated beta2R); got %s",
      paste(names(published), published, sep = " = ", collapse = ", ")
    ), call. = FALSE)
  }
  integer_ok <- function(x, minimum) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x >= minimum && x == floor(x) && x <= .Machine$integer.max
  }
  origin <- tsibble::yearquarter(c(date_begin, date_end)) - 1L
  if (is.null(block_length)) {
    block_length <- paper_mbb_block_len(as.integer(origin[[2L]] - origin[[1L]]) + 1L)
  }
  stopifnot(
    integer_ok(n_draws, 2), integer_ok(seed, 0), integer_ok(block_length, 1),
    integer_ok(n_grid, 3), n_grid %% 2L == 1L, integer_ok(n_points, 1),
    is.character(z_col), length(z_col) == 1L,
    z_col %in% vapply(PAPER_INSTRUMENT_CHOICES, `[[`, "", "column")
  )
  model <- PAPER_ANALYSIS_CONTRACT$model
  inference <- PAPER_ANALYSIS_CONTRACT$inference
  mean_ols <- PAPER_REPORTING_CONTROL$mean_ols
  stopifnot(identical(mean_ols$hac_lags, PAPER_REPORTING_CONTROL$ppml$hac_lags))
  # origin-keyed name = paper response-quarter column. growth and the
  # instrument are read one quarter ahead of the origin, hence the lead prefix;
  # the lagged SDF and return PCs are origin values under their unlagged names.
  # f<z> (e.g. fy60_vol_log) keeps the macro port's name rather than f.<z>
  y <- hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
  inputs <- c(
    stats::setNames(y, paste0("f", y)),
    stats::setNames(model$lag_expected_pc_cols, model$expected_pc_cols),
    stats::setNames(model$news_pc_cols, model$news_pc_cols),
    stats::setNames(z_col, paste0("f", z_col)),
    stats::setNames(model$return_pc_cols, model$return_pc_source_cols)
  )
  list(
    n_draws = as.integer(n_draws), seed = as.integer(seed),
    block_length = as.integer(block_length), n_grid = as.integer(n_grid),
    n_points = as.integer(n_points), taus = PAPER_ANALYSIS_CONTRACT$tau$display,
    alpha = inference$nominal_alpha,
    min_reps = as.integer(ceiling(inference$minimum_valid_draw_share * n_draws)),
    stability = inference$stability_share,
    maximum_failed_share = PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share,
    hac_lags = mean_ols$hac_lags,
    date_begin = format(origin[[1L]]), date_end = format(origin[[2L]]),
    y = paste0("f", y), x = model$expected_pc_cols, y2 = model$news_pc_cols,
    z = paste0("f", z_col), x_var = model$return_pc_source_cols, inputs = inputs,
    interval_target = "pointwise",
    interval_control = list(
      tolerance = inference$target_p_lambda_tolerance,
      max_evals = inference$target_p_max_evals
    )
  )
}
