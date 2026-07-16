# PC score series of the quarterly SDF panels.
# Run via run_pipeline.R, after build_sdf_series.R has built expected_sdf / sdf_news;
# run_pipeline.R also defines the date_begin ~ date_end PCA sample window.

#' Helper function: PC score series of a panel, complete rows over the sample window
pc_scores <- function(df, n_pc, prefix) {
  df <- df |>
    filter_window() |>
    tidyr::drop_na()
  pca <- stats::prcomp(df[value_cols(df)], center = TRUE, scale. = TRUE)
  scores <- pca$x[, seq_len(n_pc), drop = FALSE]
  colnames(scores) <- paste0(prefix, seq_len(n_pc))
  dplyr::bind_cols(df["qtr"], tibble::as_tibble(scores))
}

expected_sdf_pc <- pc_scores(expected_sdf, n_pc, "expected_sdf_pc")
# a fresh PCA on the lagged panel, not a relabel of expected_sdf_pc: the fixed
# sample window shifts the estimation sample by one quarter, so the loadings
# differ slightly
lag_expected_sdf_pc <- pc_scores(lag_expected_sdf, n_pc, "lag_expected_sdf_pc")
sdf_news_pc <- pc_scores(sdf_news, n_pc, "sdf_news_pc")

# PCA signs are arbitrary: flip any lagged component that correlates
# negatively with its unlagged counterpart at the matching quarter
aligned <- dplyr::inner_join(
  dplyr::mutate(expected_sdf_pc, qtr = qtr + lag_qtrs),
  lag_expected_sdf_pc,
  by = "qtr"
)
ref_cols <- value_cols(expected_sdf_pc)
lag_cols <- value_cols(lag_expected_sdf_pc)
flip <- sign(diag(stats::cor(aligned[ref_cols], aligned[lag_cols])))
lag_expected_sdf_pc[lag_cols] <-
  sweep(as.matrix(lag_expected_sdf_pc[lag_cols]), 2, flip, `*`)

rm(pc_scores, aligned, ref_cols, lag_cols, flip)
