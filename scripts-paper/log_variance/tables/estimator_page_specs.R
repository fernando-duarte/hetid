# Page specifications for the extension estimators of the per-estimator
# document (render_estimator_pages.R): panel notation, cell policy, panel title,
# caption subject, and notes builder per registry id. The page renderer loops
# over the registry's extension table ids and reads each spec here, so adding an
# estimator page is one entry, not a new hand-written block. Definitions only;
# sourced by render_estimator_pages.R.

paper_source_once(paper_path("log_variance", "tables", "estimator_panel.R"))
paper_source_once(paper_path("log_variance", "tables", "lad_panel_notes.R"))
paper_source_once(paper_path("log_variance", "tables", "harvey_caption.R"))

LOGVAR_ESTIMATOR_PAGE_SPECS <- list(
  lad = list(
    panel_spec = LOGVAR_LAD_PANEL_SPEC,
    cells = PAPER_REPORTING_CONTROL$cells$lad,
    title = "Panel B: Log-variance equation (conditional median)",
    subject = "log-variance equation (conditional median)",
    notes = function(result, tau_baseline) {
      build_lad_panel_notes(
        result, tau_baseline, LOGVAR_LAD_CONTROL$grid_cap,
        LOGVAR_LAD_CONTROL$fit_budget
      )
    }
  )
)

logvar_estimator_page_spec <- function(id) {
  spec <- LOGVAR_ESTIMATOR_PAGE_SPECS[[id]]
  if (is.null(spec)) {
    stop(sprintf("No estimator page spec for %s", id), call. = FALSE)
  }
  spec
}

# An extension estimator's page is required exactly when its bounds artifact is;
# a conditional estimator (LAD behind its dependency gate) may be absent
logvar_estimator_page_required <- function(id) {
  bounds <- paper_logvar_estimator_spec(id)$artifacts[["bounds"]]
  identical(.artifact_record(bounds)$status[[1L]], PAPER_ARTIFACT_STATUS$required)
}

# The Harvey page notes: the panel notes with the search budgets the Harvey map
# ran at, read from the registered budget policy (the Harvey control list
# carries no budgets)
logvar_harvey_page_notes <- function(harvey, tau_baseline) {
  build_harvey_panel_notes(
    harvey, tau_baseline, paper_logvar_budget("harvey", "grid_cap"),
    paper_logvar_budget("harvey", "fit_budget"),
    se_type = logvar_harvey_se_type,
    se_hac_lags = logvar_harvey_se_hac_lags,
    set_endpoint_inference = TRUE
  )
}
