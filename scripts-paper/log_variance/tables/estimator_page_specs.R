# Page specifications for the extension estimators of the per-estimator
# document (render_estimator_pages.R): panel notation, cell policy, analytic SE
# choice, panel title, caption subject, and notes builder per registry id. The
# page renderer loops over the registry's extension table ids and reads each
# spec here, so adding an estimator page is one entry, not a new hand-written
# block. Definitions only; sourced by render_estimator_pages.R.

paper_source_once(paper_path("log_variance", "tables", "estimator_panel.R"))
paper_source_once(paper_path("log_variance", "tables", "lad_panel_notes.R"))
paper_source_once(paper_path("log_variance", "tables", "harvey_caption.R"))
paper_source_once(paper_path("log_variance", "tables", "log_projection_panel_notes.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "standard_errors.R"
))
paper_source_once(paper_path("log_variance", "estimators", "controls.R"))

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
  ),
  log_plus = list(
    panel_spec = LOGVAR_LOG_PLUS_PANEL_SPEC,
    cells = PAPER_REPORTING_CONTROL$cells$log_variance,
    se_type = PAPER_REPORTING_CONTROL$log_plus$se_type,
    se_types = LOGVAR_LOG_PROJECTION_SE_TYPES,
    title = paste(
      "Panel B: Log-variance equation (regularized log projection,",
      "additive threshold)"
    ),
    subject = "log-variance equation (regularized log projection, additive threshold)",
    notes = function(result, tau_baseline) {
      build_log_projection_panel_notes(result, tau_baseline)
    }
  ),
  log_fuller = list(
    panel_spec = LOGVAR_LOG_FULLER_PANEL_SPEC,
    cells = PAPER_REPORTING_CONTROL$cells$log_variance,
    se_type = PAPER_REPORTING_CONTROL$log_fuller$se_type,
    se_types = LOGVAR_LOG_PROJECTION_SE_TYPES,
    title = paste(
      "Panel B: Log-variance equation (regularized log projection,",
      "two-pass Fuller)"
    ),
    subject = "log-variance equation (regularized log projection, two-pass Fuller)",
    notes = function(result, tau_baseline) {
      build_log_projection_panel_notes(result, tau_baseline)
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

# Whether the registry puts an extension estimator in the set bootstrap
logvar_extension_in_boot <- function(id) {
  "set_bootstrap" %in% paper_logvar_estimator_spec(id)$capabilities
}

# Whether the bootstrap anchor reproduces the published set: the log
# projections' draws search the published map's full lattice unless their
# control caps the search (NA or a finite budget)
logvar_extension_anchor_matches <- function(id) {
  if (!"log_projection" %in% paper_logvar_estimator_spec(id)$capabilities) {
    return(TRUE)
  }
  budgets <- LOGVAR_LOG_PROJECTION_CONTROL[c("bootstrap_grid_cap", "bootstrap_fit_budget")]
  all(vapply(budgets, function(x) isTRUE(is.infinite(x)), logical(1)))
}

# An extension page's panel: the analytic SE choice from its page spec, and the
# bootstrap envelope and tau = 0 statistic exactly when the estimator is in the
# set bootstrap
logvar_extension_page_parts <- function(id, result, tau_display, boot) {
  spec <- logvar_estimator_page_spec(id)
  in_boot <- logvar_extension_in_boot(id)
  logvar_estimator_panel_parts(
    result, result$sample$n, tau_display, spec$panel_spec,
    spec$se_type, spec$se_types,
    if (in_boot) boot[[id]], spec$cells,
    if (in_boot) logvar_boot_point_stat(boot, id)
  )
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
