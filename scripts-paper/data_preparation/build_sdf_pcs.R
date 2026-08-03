# PC score series of the quarterly SDF panels.
# Run via run_pipeline.R, after build_sdf_series.R has built expected_sdf / sdf_news;
# run_pipeline.R also defines the date_begin ~ date_end PCA sample window.

#' Helper function: PC score series of a panel, complete rows over the sample window
#' @param df data frame with the key column and PC input columns
#' @param output_names PC score column names to assign, one per component kept
#' @return tibble with the key column and one column per name in output_names
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
lag_ref_cor <- diag(stats::cor(aligned[ref_cols], aligned[lag_cols]))
# sign() is a {-1, 0, NA} map, not the {-1, +1} multiplier the flip needs: an
# exactly-zero correlation would multiply a whole lagged component by zero and an
# NA would void it, and either way the column enters the conditioning set as a
# dead regressor that lm silently aliases away. The sign is only defined when
# every correlation is finite and non-zero, so require that rather than guess.
stopifnot(
  "lagged expected-SDF PC correlation is not finite" =
    all(is.finite(lag_ref_cor)),
  "lagged expected-SDF PC correlation is exactly zero, so its sign is undefined" =
    all(lag_ref_cor != 0)
)
flip <- sign(lag_ref_cor)
lag_expected_sdf_pc[lag_cols] <-
  sweep(as.matrix(lag_expected_sdf_pc[lag_cols]), 2, flip, `*`)

rm(pc_scores, model_axes, aligned, ref_cols, lag_cols, lag_ref_cor, flip)
