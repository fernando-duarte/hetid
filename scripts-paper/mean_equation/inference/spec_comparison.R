# Endpoint bootstrap for the mean panel under every NON-published beta2R
# specification, so the comparison the diagnostics promise includes intervals
# and tau = 0 statistics rather than full-sample sets alone.
#
# The main bootstrap stage runs one specification, the published one, because
# the volatility panel is built on top of it and that half is the expensive one.
# The mean branch on its own is about two and a half minutes at ten thousand
# draws across the available cores, so the alternative costs almost nothing and
# is not worth threading through the stage's cache and provenance machinery.
#
# PAIRING IS THE POINT. The alternative must see the SAME resamples as the
# published run or the differences confound the specification with resampling
# noise. The stage hands its primary family over rather than this file rebuilding
# one: the ownership audit allows exactly one constructor of the index protocol,
# and a paired comparison should not rest on two builders agreeing. The digest is
# still checked against what the stage recorded, so a future refactor that routes
# the wrong family here stops the run instead of quietly producing an unpaired
# comparison, which would look exactly like a paired one.
#
# Run from run_pipeline.R after the bootstrap stage. Writes one diagnostics CSV
# holding every specification, published first, keyed by a spec column.

paper_source_once(paper_path(
  "mean_equation", "estimate_identified_set_core.R"
))
paper_source_once(paper_path(
  "support", "identification", "identified_set_bootstrap_collect.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_mean_result_inputs.R"
))
paper_source_once(paper_path("inference", "bootstrap_stage_draw.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))

mean_spec_comparison_draws <- function(fit, indices, cores) {
  spec <- list(
    y1_col = fit$y1_col, x_cols = fit$x_cols, y2_cols = fit$y2_cols,
    z_col = z_col, gamma = fit$gamma, impose_null = fit$impose_null,
    coefs = c(fit$beta1_table$coef, fit$theta_table$coef),
    taus = fit$tau_display
  )
  raw <- parallel::mclapply(indices, function(index) {
    tryCatch(
      set_id_boot_draw(fit$data[index, , drop = FALSE], spec),
      error = function(e) conditionMessage(e)
    )
  }, mc.cores = cores)
  set_id_boot_collect(raw, spec)
}

mean_spec_comparison_rows <- function(fit, collected, alpha, spec_name) {
  result <- mean_boot_results(collected, fit, alpha, set_id_boot$provenance)
  projected <- bootstrap_stage_mean_result_inputs(fit)
  rows <- set_id_boot_diagnostics(
    result, result$inference, projected$set_tables,
    fit$tau_display, result$point_t
  )
  cbind(
    spec = spec_name,
    published = identical(spec_name, paper_published_spec("mean")),
    impose_null = fit$impose_null,
    rows, row.names = NULL, stringsAsFactors = FALSE
  )
}

local({
  alternatives <- setdiff(PAPER_SPEC_PLAN$mean, paper_published_spec("mean"))
  published <- set_id_mean_eq_by_spec[[paper_published_spec("mean")]]
  frames <- list(mean_spec_comparison_rows(
    published, set_id_boot, PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
    paper_published_spec("mean")
  ))
  if (length(alternatives)) {
    family <- bootstrap_primary_family
    # handed over by the stage rather than rebuilt; the digest check survives as
    # a guard against a future refactor passing the wrong family through
    if (!identical(family$index_sha256, set_id_boot$provenance$index_sha256)) {
      stop(
        "spec comparison received a different index family than the stage used; ",
        "the comparison would not be paired",
        call. = FALSE
      )
    }
    for (name in alternatives) {
      fit <- set_id_mean_eq_by_spec[[name]]
      message(sprintf("  spec %s: mean endpoint bootstrap on the shared family", name))
      collected <- mean_spec_comparison_draws(fit, family$indices, boot_cores)
      frames[[length(frames) + 1L]] <- mean_spec_comparison_rows(
        fit, collected, PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha, name
      )
    }
  }
  out <- do.call(rbind, frames)
  paper_write_typed_csv(
    cbind(paper_inference_metadata_frame(nrow(out)), out),
    artifact_path("mean_spec_comparison"),
    "mean_spec_comparison"
  )
  message(sprintf(
    "mean spec comparison: %d specs, %d rows", length(unique(out$spec)), nrow(out)
  ))
})
