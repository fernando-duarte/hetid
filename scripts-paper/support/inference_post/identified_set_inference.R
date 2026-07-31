# Sampling-uncertainty helpers shared by every consumer of the set-identified
# bootstrap draws: the percentile band, and the contract-owned minimum-repetition
# rule each rendering gate reads. The calibration module sourced here supplies
# the robust (MAD) scale both panels studentize by; its Stoye (2009) and
# Imbens-Manski (2004) critical values are retained for the Panel A diagnostics
# cross-check on the normal approximation and reach no published cell. The
# published intervals themselves are built in endpoint_target_cells.R. Consumed
# by scripts-paper/inference/run_bootstrap_stage.R and tested by the paper
# inference suite.
paper_source_once(paper_path(
  "support", "inference_post", "inference_calibration.R"
))

# Median and nominal percentile band of the finite draws.
boot_band <- function(x, alpha = PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha) {
  stopifnot(alpha > 0, alpha < 1)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(
      median = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      n = 0
    ))
  }
  c(
    median = stats::median(x),
    lower = unname(stats::quantile(x, alpha / 2)),
    upper = unname(stats::quantile(x, 1 - alpha / 2)),
    n = length(x)
  )
}
# Contract-owned reliability threshold shared by every rendering gate (the
# set cells, the tau = 0 point statistic, and the diagnostics table).
boot_min_reps <- function(
  b,
  inference = PAPER_ANALYSIS_CONTRACT$inference
) {
  ceiling(b * inference$minimum_valid_draw_share)
}
