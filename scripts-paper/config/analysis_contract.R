# Canonical scientific axes, slack policy, generated names, and input metadata.

paper_named_doubles <- function(fields,
                                values = rep(NA_real_, length(fields))) {
  stopifnot(length(fields) == length(values), !anyDuplicated(fields))
  as.list(stats::setNames(as.numeric(values), fields))
}

paper_record_doubles <- function(record, fields) {
  stopifnot(all(fields %in% names(record)))
  as.numeric(unlist(record[fields], use.names = FALSE))
}

paper_tau_key <- function(tau) paper_numeric_key(tau)

PAPER_INFERENCE_SEARCH_CONTROL <- list(
  im_root = list(
    bracket_lower = 0,
    bracket_upper = 10,
    tolerance = 1e-10
  ),
  stoye_root = list(
    bracket_lower = 1e-6,
    bracket_upper = 8,
    tolerance = 1e-8
  ),
  bvn = list(
    degenerate_correlation = 0.999,
    integration_relative_tolerance = 1e-9
  ),
  robust_endpoint_correlation = list(
    minimum_pairs = 8L,
    mad_trim_multiple = 6
  ),
  tau_star = list(
    fine_grid_points = 20L,
    bisection_iterations = 40L,
    bootstrap_bisection_iterations = 15L
  ),
  bootstrap = list(
    fatal_failure_share = 0.25,
    sensitivity_min_reps = 50L,
    sensitivity_reps_share = 0.25,
    progress_report_every = 25L
  ),
  logvar_endpoint = list(stability_share = 0.85)
)

paper_bootstrap_failure_limit <- function(
  reps, control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  floor(reps * control$bootstrap$fatal_failure_share)
}

paper_bootstrap_sensitivity_reps <- function(
  reps, control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  max(
    control$bootstrap$sensitivity_min_reps,
    floor(reps * control$bootstrap$sensitivity_reps_share)
  )
}

PAPER_ANALYSIS_CONTRACT <- local({
  n_mean_pc <- 3L
  n_return_pc <- 4L
  mean_index <- seq_len(n_mean_pc)
  return_index <- seq_len(n_return_pc)
  list(
    version = "1.0.0",
    model = list(
      n_mean_pc = n_mean_pc,
      n_return_pc = n_return_pc,
      expected_pc_cols = paste0("expected_sdf_pc", mean_index),
      lag_expected_pc_cols = paste0("lag_expected_sdf_pc", mean_index),
      news_pc_cols = paste0("sdf_news_pc", mean_index),
      return_pc_source_cols = paste0("pc", return_index),
      return_pc_cols = paste0("l.pc", return_index),
      artifact_fields = list(
        mean = paste0("b", mean_index),
        return = paste0("thetaR", return_index),
        variance = paste0("beta", return_index),
        scale = paste0("scale", return_index)
      ),
      index = list(
        mean = mean_index,
        theta_intercept = 1L,
        theta_return = 1L + return_index,
        scale = return_index
      ),
      joint_gmm_dimensions = list(
        n_moments_unprofiled = 2L * (n_return_pc + 1L),
        n_parameters_unprofiled = n_mean_pc + n_return_pc + 2L,
        n_moments_profiled = 2L * n_return_pc,
        n_parameters_profiled = n_mean_pc + n_return_pc
      )
    ),
    tau = list(
      baseline = 0.05,
      cap = 0.99,
      display = c(0.05, 0.10, 0.20, 0.40),
      projection = c(0.05, 0.10, 0.20),
      sweep_step = 0.005,
      bootstrap_step = 0.05,
      figure_grid_n = 25L
    ),
    variance_share = list(
      grid_points_per_axis = 101L,
      coherence_ratio = 0.98,
      coherence_slack = 1e-9,
      render_degenerate_rtol = 1e-9
    ),
    inference = list(nominal_alpha = 0.10),
    figure = list(region_dimension = 3L),
    input = list(
      acm = list(
        data_types = c("yields", "term_premia"),
        frequency = "quarterly",
        auto_download = FALSE,
        source = "auto"
      ),
      consumption = list(fred_series = "PCECC96"),
      instrument = list(
        column = "y60_vol",
        label = paste(
          "the de-meaned realized quarterly volatility of the five-year yield"
        ),
        producer = file.path(
          "data_preparation",
          "build_yield_volatility.R"
        )
      )
    )
  )
})

stopifnot(
  PAPER_INFERENCE_SEARCH_CONTROL$im_root$bracket_lower <
    PAPER_INFERENCE_SEARCH_CONTROL$im_root$bracket_upper,
  PAPER_INFERENCE_SEARCH_CONTROL$stoye_root$bracket_lower <
    PAPER_INFERENCE_SEARCH_CONTROL$stoye_root$bracket_upper,
  PAPER_INFERENCE_SEARCH_CONTROL$im_root$tolerance > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$stoye_root$tolerance > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bvn$degenerate_correlation > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bvn$degenerate_correlation < 1,
  PAPER_INFERENCE_SEARCH_CONTROL$bvn$integration_relative_tolerance > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$robust_endpoint_correlation$minimum_pairs >= 2L,
  PAPER_INFERENCE_SEARCH_CONTROL$robust_endpoint_correlation$mad_trim_multiple > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$tau_star$fine_grid_points >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bisection_iterations >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bootstrap_bisection_iterations >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share < 1,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$sensitivity_min_reps >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$sensitivity_reps_share > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$sensitivity_reps_share <= 1,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$progress_report_every >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share < 1,
  identical(
    PAPER_ANALYSIS_CONTRACT$tau$display[[1L]],
    PAPER_ANALYSIS_CONTRACT$tau$baseline
  ),
  all(
    PAPER_ANALYSIS_CONTRACT$tau$projection %in%
      PAPER_ANALYSIS_CONTRACT$tau$display
  ),
  PAPER_ANALYSIS_CONTRACT$tau$cap < 1,
  PAPER_ANALYSIS_CONTRACT$variance_share$grid_points_per_axis >= 2L,
  PAPER_ANALYSIS_CONTRACT$variance_share$coherence_ratio > 0,
  PAPER_ANALYSIS_CONTRACT$variance_share$coherence_ratio <= 1,
  PAPER_ANALYSIS_CONTRACT$variance_share$coherence_slack >= 0,
  PAPER_ANALYSIS_CONTRACT$variance_share$render_degenerate_rtol > 0,
  PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha > 0,
  PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha < 1,
  !anyDuplicated(unlist(
    PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  ))
)

paper_instrument_description <- function(
  spec = PAPER_ANALYSIS_CONTRACT$input$instrument
) {
  latex_column <- gsub("_", "\\_", spec$column, fixed = TRUE)
  sprintf("%s (\\texttt{%s})", spec$label, latex_column)
}
