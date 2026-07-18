# PC score series of the quarterly SDF panels.
# Run via run_pipeline.R, after build_sdf_series.R has built expected_sdf / sdf_news;
# run_pipeline.R also defines the date_begin ~ date_end PCA sample window.

#' Helper function: PC score series of a panel, complete rows over the sample window
pc_scores <- function(df, output_names) {
  df <- df |>
    filter_window() |>
    tidyr::drop_na()
  pca <- paper_model_pca(
    df[value_cols(df)],
    PAPER_ANALYSIS_CONTRACT$model$preprocessing$sdf_pc
  )
  scores <- pca$x[, seq_along(output_names), drop = FALSE]
  colnames(scores) <- output_names
  dplyr::bind_cols(
    df[PAPER_ANALYSIS_CONTRACT$model$key_col],
    tibble::as_tibble(scores)
  )
}

model_axes <- PAPER_ANALYSIS_CONTRACT$model
stopifnot(model_axes$n_mean_pc == n_pc)
expected_sdf_pc <- pc_scores(
  expected_sdf,
  model_axes$expected_pc_cols
)
# a fresh PCA on the lagged panel, not a relabel of expected_sdf_pc: the fixed
# sample window shifts the estimation sample by one quarter, so the loadings
# differ slightly
lag_expected_sdf_pc <- pc_scores(
  lag_expected_sdf,
  model_axes$lag_expected_pc_cols
)
sdf_news_pc <- pc_scores(sdf_news, model_axes$news_pc_cols)

# PCA signs are arbitrary: flip any lagged component that correlates
# negatively with its unlagged counterpart at the matching quarter
aligned <- dplyr::inner_join(
  dplyr::mutate(expected_sdf_pc, qtr = qtr + lag_qtrs),
  lag_expected_sdf_pc,
  by = model_axes$key_col
)
ref_cols <- value_cols(expected_sdf_pc)
lag_cols <- value_cols(lag_expected_sdf_pc)
flip <- sign(diag(stats::cor(aligned[ref_cols], aligned[lag_cols])))
lag_expected_sdf_pc[lag_cols] <-
  sweep(as.matrix(lag_expected_sdf_pc[lag_cols]), 2, flip, `*`)

rm(pc_scores, model_axes, aligned, ref_cols, lag_cols, flip)
