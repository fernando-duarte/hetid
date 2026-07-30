# Subprocess test for the reset_pipeline_state.R CLI entrypoint.

local({
  reset_cli_output <- function(arguments = character(0)) {
    system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", paper_path("reset_pipeline_state.R"), arguments),
      stdout = TRUE, stderr = TRUE
    )
  }
  reset_cli_status <- function(output) {
    status <- attr(output, "status")
    if (is.null(status)) 0L else as.integer(status)
  }

  output <- reset_cli_output()
  check(
    "reset_pipeline_state.R runs to completion with no fixtures present",
    identical(reset_cli_status(output), 0L)
  )
  check(
    "reset_pipeline_state.R reports every compartment by name",
    all(vapply(
      c(
        "bootstrap_cache", "gate_state", "diagnostics",
        "tables", "figures", "reports", "latex_sidecars"
      ),
      function(name) any(grepl(name, output, fixed = TRUE)),
      logical(1)
    ))
  )

  keep_tracked_output <- reset_cli_output("--keep-tracked")
  check(
    "reset_pipeline_state.R --keep-tracked also runs to completion",
    identical(reset_cli_status(keep_tracked_output), 0L)
  )
})
