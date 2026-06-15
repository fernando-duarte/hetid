# Stage-04 baseline spec stamp + downstream fail-closed consistency check.
#
# The default-pipeline identification numbers depend on three mode settings that
# change how the W1/W2 residuals and the instrument matrix are built:
#   * N_Y1_LAGS               -- own-lags of Y1 in the common conditioning vector
#   * impose_news_projection_zero() -- estimate B vs impose exact-news B = 0
#   * z_source_active()/HETID_Z_SOURCE -- which matrix is the instrument Z
# Stage 04 stamps the resolved values into the saved RDS's `spec` list; stages
# that LOAD that baseline and combine it with the CURRENT settings (stage 05's
# optimizer, stage 06's tables) assert the stamp matches before proceeding, so a
# stale baseline produced under a different mode cannot be silently reused.

# Resolve the spec the CURRENT process is configured for. Single source of truth
# for both the stage-04 stamp and the downstream check, so the two cannot drift.
current_baseline_spec <- function() {
  list(
    y1_lags = if (exists("N_Y1_LAGS")) N_Y1_LAGS else 0L,
    news_projection_mode = if (impose_news_projection_zero()) {
      "impose_b_zero"
    } else {
      "estimate_b"
    },
    z_source = if (z_source_active()) Sys.getenv("HETID_Z_SOURCE") else "pc"
  )
}

# Pure comparison core (testable without disk/cli): given the loaded spec list
# and the current spec list, return a character vector of human-readable
# mismatch lines (empty == OK). Backward-compat policy: a field ABSENT in the
# loaded spec (an old baseline saved before this stamp existed) is treated as a
# MISMATCH, not a pass -- fail toward safety so an ambiguous old artifact forces
# a Stage-04 re-run rather than silently combining with current settings.
baseline_spec_mismatches <- function(loaded, current) {
  fields <- c("y1_lags", "news_projection_mode", "z_source")
  msgs <- character(0)
  for (f in fields) {
    if (is.null(loaded[[f]])) {
      msgs <- c(msgs, sprintf(
        "%s: absent in baseline (pre-stamp artifact) vs current '%s'",
        f, format(current[[f]])
      ))
      next
    }
    if (!identical(loaded[[f]], current[[f]])) {
      msgs <- c(msgs, sprintf(
        "%s: baseline '%s' vs current '%s'",
        f, format(loaded[[f]]), format(current[[f]])
      ))
    }
  }
  msgs
}

# Fail-closed guard for stages that load the Stage-04 baseline RDS and combine
# it with the current settings. `spec` is the loaded `results$spec` list.
assert_baseline_spec_current <- function(spec) {
  msgs <- baseline_spec_mismatches(spec, current_baseline_spec())
  if (length(msgs)) {
    cli::cli_abort(c(
      "Stage-04 baseline was produced under different settings than the current run.",
      "x" = "{msgs}",
      "i" = paste(
        "Re-run Stage 04",
        "(scripts/04_identification_without_optimization/compute_identification.R)",
        "under the current settings before continuing."
      )
    ))
  }
  invisible(spec)
}
