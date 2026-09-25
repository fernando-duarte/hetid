# Raw simultaneous containment diagnostic over the eligible live sides.
# This reaches the diagnostics CSV, not a published interval cell. The package
# also reports active sides and common-pool counts for direct users.
paper_source_once(paper_path("support", "inference_post", "endpoint_targets.R"))

logvar_simultaneous_critical <- function(draws, full,
                                         alpha = PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
                                         min_reps = boot_min_reps(nrow(draws$lower)),
                                         stability =
                                           PAPER_ANALYSIS_CONTRACT$inference$stability_share) {
  input <- paper_endpoint_inputs(draws, full)
  hetid::bootstrap_set_interval(
    input$full, input$draws, "containment",
    alpha, min_reps, stability
  )$simultaneous$critical
}
