# Independent boundary checks for the canonical paper analysis contract.

contract <- PAPER_ANALYSIS_CONTRACT
model <- contract$model
tau <- contract$tau
variance_share <- contract$variance_share
figure_render <- PAPER_FIGURE_RENDER_CONTROL

check(
  "analysis contract baseline is the first display slack",
  identical(tau$baseline, tau$display[[1L]])
)
check(
  "figure render controls own reproducible grids and devices",
  is.integer(figure_render$region_3d$seed) &&
    figure_render$region_3d$wall_grid_points >= 2L &&
    figure_render$projections$grid_points >= 2L &&
    all(vapply(
      figure_render$devices,
      function(device) all(device > 0),
      logical(1)
    ))
)
check(
  "projection slacks are named display slacks",
  all(tau$projection %in% tau$display)
)
check(
  "mean-PC fields derive from the mean-PC axis",
  identical(
    model$artifact_fields$mean,
    paste0("b", seq_len(model$n_mean_pc))
  )
)
check(
  "return-PC fields derive from the return-PC axis",
  identical(
    model$artifact_fields$return,
    paste0("thetaR", seq_len(model$n_return_pc))
  )
)
check(
  "scale fields derive from the return-PC axis",
  identical(
    model$artifact_fields$scale,
    paste0("scale", seq_len(model$n_return_pc))
  )
)
check(
  "variance-slope fields derive from the return-PC axis",
  identical(
    model$artifact_fields$variance,
    paste0("beta", seq_len(model$n_return_pc))
  )
)
check(
  "joint-GMM unprofiled moment dimension is derived",
  identical(
    model$joint_gmm_dimensions$n_moments_unprofiled,
    2L * (model$n_return_pc + 1L)
  )
)
check(
  "joint-GMM profiled moment dimension is derived",
  identical(
    model$joint_gmm_dimensions$n_moments_profiled,
    2L * model$n_return_pc
  )
)
check(
  "generated artifact fields are unique",
  !anyDuplicated(unlist(model$artifact_fields))
)
check(
  "variance-share controls have one named analysis-contract owner",
  identical(
    unname(unlist(variance_share)),
    c(101, 0.98, 1e-9, 1e-9)
  )
)
share_files <- c(
  optimization = paper_path(
    "mean_equation", "variance_shares", "share_optimization.R"
  ),
  computation = paper_path(
    "mean_equation", "variance_shares", "compute_variance_shares.R"
  ),
  rendering = paper_path(
    "mean_equation", "variance_shares", "render_variance_share_table.R"
  )
)
share_code <- vapply(
  share_files,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
)
share_fields <- c(
  optimization = "grid_points_per_axis",
  computation = "coherence_ratio",
  rendering = "render_degenerate_rtol"
)
check(
  "variance-share consumers reference their named contract fields",
  all(mapply(
    grepl,
    share_fields,
    share_code,
    MoreArgs = list(fixed = TRUE),
    USE.NAMES = FALSE
  )) &&
    grepl("coherence_slack", share_code[["computation"]], fixed = TRUE)
)
check(
  "variance-share consumers do not restate their control literals",
  !grepl("length.out = 101L", share_code[["optimization"]], fixed = TRUE) &&
    !grepl(">= 0.98", share_code[["computation"]], fixed = TRUE) &&
    !grepl("<= 1e-9", share_code[["rendering"]], fixed = TRUE)
)
rm(share_files, share_code, share_fields)
check(
  "instrument prose derives its canonical column",
  grepl(
    gsub("_", "\\_", contract$input$instrument$column, fixed = TRUE),
    paper_instrument_description(),
    fixed = TRUE
  )
)
check(
  "model key and intercept schema are explicit",
  identical(model$key_col, "qtr") &&
    identical(model$intercept_col, "(Intercept)")
)
check(
  "all PC axes have their declared cardinality",
  length(model$expected_pc_cols) == model$n_mean_pc &&
    length(model$lag_expected_pc_cols) == model$n_mean_pc &&
    length(model$news_pc_cols) == model$n_mean_pc &&
    length(model$return_pc_source_cols) == model$n_return_pc &&
    length(model$return_pc_cols) == model$n_return_pc
)
check(
  "PCA preprocessing policies are explicit",
  identical(
    model$preprocessing$sdf_pc,
    list(center = TRUE, scale = TRUE)
  ) &&
    identical(
      model$preprocessing$return_pc,
      list(center = TRUE, scale = FALSE)
    )
)
primary <- paper_logvar_estimator_ids(primary = TRUE)
check(
  "primary estimator membership is registry-owned",
  identical(primary, c("ppml", "harvey")) &&
    all(vapply(primary, function(id) {
      spec <- paper_logvar_estimator_spec(id)
      identical(
        spec$response_scale,
        PAPER_LOGVAR_RESPONSE_SCALES[["variance"]]
      ) &&
        all(c(
          "set_bootstrap", "fitted_volatility"
        ) %in% spec$capabilities)
    }, logical(1))) &&
    identical(
      paper_logvar_estimator_ids(primary = FALSE),
      "lad"
    )
)
