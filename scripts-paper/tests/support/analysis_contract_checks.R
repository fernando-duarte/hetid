# Independent boundary checks for the canonical paper analysis contract.

contract <- PAPER_ANALYSIS_CONTRACT
model <- contract$model
tau <- contract$tau
variance_share <- contract$variance_share

check(
  "analysis contract baseline is the first display slack",
  identical(tau$baseline, tau$display[[1L]])
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
  ),
  caption = paper_path(
    "mean_equation", "variance_shares", "variance_share_caption.R"
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
  rendering = "render_degenerate_rtol",
  caption = "grid_points_per_axis"
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
    !grepl("<= 1e-9", share_code[["rendering"]], fixed = TRUE) &&
    !grepl("101 points per axis", share_code[["caption"]], fixed = TRUE)
)
paper_source_once(share_files[["caption"]])
set_id_mean_eq <- list(
  sample = list(n = 10L, span = as.Date(c("2000-01-01", "2002-04-01"))),
  tau_star = 0.5
)
impose_beta2r_null <- TRUE
share_notes <- build_var_share_notes(sd_c = 1)
check(
  "variance-share caption interpolates the configured grid resolution",
  grepl(
    sprintf("%d points per axis", variance_share$grid_points_per_axis),
    paste(share_notes, collapse = " "),
    fixed = TRUE
  )
)
rm(
  share_files, share_code, share_fields, share_notes,
  set_id_mean_eq, impose_beta2r_null
)
check(
  "instrument prose derives its canonical column",
  grepl(
    gsub("_", "\\_", contract$input$instrument$column, fixed = TRUE),
    paper_instrument_description(),
    fixed = TRUE
  )
)
