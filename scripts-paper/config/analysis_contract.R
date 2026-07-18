# Canonical scientific axes, slack policy, generated names, and input metadata.

paper_source_once(paper_path("config", "figure_rendering.R"))
paper_source_once(paper_path("config", "inference_search_control.R"))

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
      key_col = "qtr",
      intercept_col = "(Intercept)",
      expected_pc_cols = paste0("expected_sdf_pc", mean_index),
      lag_expected_pc_cols = paste0("lag_expected_sdf_pc", mean_index),
      news_pc_cols = paste0("sdf_news_pc", mean_index),
      return_pc_source_cols = paste0("pc", return_index),
      return_pc_cols = paste0("l.pc", return_index),
      preprocessing = list(
        sdf_pc = list(center = TRUE, scale = TRUE),
        return_pc = list(center = TRUE, scale = FALSE)
      ),
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
    inference = list(
      version = "1.1.0",
      nominal_alpha = 0.10,
      minimum_valid_draw_share = 0.50
    ),
    figure = list(region_dimension = 3L),
    input = list(
      acm = list(
        data_types = c("yields", "term_premia"),
        frequency = "quarterly",
        auto_download = FALSE,
        source = "auto"
      ),
      yield_volatility = list(
        data_types = "yields",
        frequency = "daily",
        auto_download = TRUE,
        source = "auto"
      ),
      consumption = list(
        fred_series = "PCECC96",
        fetch_kind = "economic.data"
      ),
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
  PAPER_ANALYSIS_CONTRACT$inference$minimum_valid_draw_share > 0,
  PAPER_ANALYSIS_CONTRACT$inference$minimum_valid_draw_share <= 1,
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

paper_normalize_model_matrix <- function(x, policy) {
  stopifnot(
    is.list(policy),
    identical(names(policy), c("center", "scale")),
    is.logical(policy$center),
    length(policy$center) == 1L,
    is.logical(policy$scale),
    length(policy$scale) == 1L
  )
  scale(
    as.matrix(x),
    center = policy$center,
    scale = policy$scale
  )
}

paper_model_pca <- function(x, policy) {
  stopifnot(
    is.list(policy),
    identical(names(policy), c("center", "scale"))
  )
  stats::prcomp(
    as.matrix(x),
    center = policy$center,
    scale. = policy$scale
  )
}
