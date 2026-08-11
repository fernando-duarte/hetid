# One compilable document, one page per log-variance estimator. Each page is the
# mean equation over that estimator's log-variance equation, in the layout of
# render_combined_inference_table.R, followed by that estimator's own notes.
#
# The mean panel repeats on every page so each page stands alone: a reader
# comparing estimators never has to hold Panel A in their head from an earlier
# page. It is the same object every time, from the same builder, so the
# repetition cannot disagree with itself.
#
# The column header row is emitted once per page from the MEAN panel's headers,
# and each panel is named by a \multicolumn title rather than by its own header
# row. That is why an estimator's reference_header never appears here: the
# combined layout has no second header row for it to occupy, and the first
# column means the same thing in both panels -- the exogenous-news OLS case.
#
# Replaces the five per-estimator and per-variant table renderers.
# Run via run_pipeline.R after the bootstrap stage and the estimator runs.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "table_environment.R"))
paper_source_once(paper_path("support", "latex", "overleaf_panel_table.R"))
paper_source_once(paper_path(
  "log_variance", "tables", "render_variance_panel_fragments.R"
))
paper_source_once(paper_path("mean_equation", "tables", "structural_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "estimator_panel.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_captions.R"))
paper_source_once(paper_path("log_variance", "tables", "logols_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "harvey_caption.R"))
paper_source_once(paper_path("log_variance", "tables", "lad_panel_notes.R"))
paper_source_once(paper_path("log_variance", "tables", "set_inference_caption.R"))

PAGES_ID <- "structural_var_estimators_table"

local({
  panel_a <- structural_equation_table_parts(set_id_mean_eq, set_id_boot, n_pc)
  headers <- panel_a$headers
  n_col <- length(headers)
  n_obs <- log_var_eq$sample$n
  tau_display <- set_id_mean_eq$tau_display
  tau_baseline <- set_id_mean_eq$tau_baseline
  # rules sit after the intercept's statistic row and after the last slope's,
  # leaving the R-squared and N tail below the final rule
  coef_rules <- c(2L, 2L * (1L + n_pc_r))

  mean_panel <- paper_overleaf_panel_lines(
    "Panel A: Mean equation",
    panel_a$row_labels, panel_a$columns, headers,
    PAPER_OVERLEAF_SET_LABEL,
    blocks = c(2L, panel_a$rule_after[[1L]]),
    tail_after = panel_a$rule_after[[2L]]
  )

  # An estimator's own headers are discarded, so only the shape has to agree:
  # the column count, and the tau columns after the reference one. Comparing the
  # reference header too would halt on estimators that call it "Reference"
  # rather than "OLS" -- a difference in a value this layout never emits.
  variance_panel <- function(parts, title, set_label = PAPER_OVERLEAF_SET_LABEL) {
    stopifnot(
      length(parts$columns) == n_col,
      identical(parts$headers[-1L], headers[-1L]),
      all(vapply(parts$columns, length, integer(1)) == length(parts$rows))
    )
    paper_overleaf_panel_lines(
      title, parts$rows, parts$columns, headers, set_label,
      blocks = coef_rules[[1L]],
      tail_after = coef_rules[[2L]]
    )
  }

  # title heads the panel, subject names the estimator inside the caption
  # sentence; they are separate because the panel head carries the "Panel B:"
  # prefix and the caption reads as prose without it.
  page <- function(parts, title, subject, notes, component,
                   set_label = PAPER_OVERLEAF_SET_LABEL) {
    c(
      latex_table_environment(
        tabular_lines = paper_overleaf_panel_table(
          list(mean_panel, variance_panel(parts, title, set_label)),
          n_col
        ),
        caption = paste0(
          "Mean equation over the ", subject, ". Identified sets in brackets, ",
          "moving-block bootstrap confidence intervals in parentheses beneath."
        ),
        label = artifact_latex_label(PAGES_ID, component),
        notes = notes,
        fontsize = ""
      ),
      "\\clearpage"
    )
  }

  ppml <- paper_logvar_result("ppml")
  pages <- page(
    logvar_ppml_table_parts(
      ppml, tau_display, n_pc_r,
      se_type = logvar_ppml_se_type,
      envelope = log_var_eq_set_boot$ppml,
      point_stat = logvar_boot_point_stat(log_var_eq_set_boot, "ppml")
    ),
    "Panel B: Log-variance equation (quasi-maximum likelihood)",
    "log-variance equation (quasi-maximum likelihood)",
    c(
      build_ppml_panel_notes(
        ppml, tau_baseline, logvar_ppml_grid_cap, logvar_ppml_fit_budget,
        se_type = logvar_ppml_se_type, se_hac_lags = logvar_ppml_se_hac_lags,
        set_endpoint_inference = TRUE
      ),
      build_logvar_set_inference_notes(log_var_eq_set_boot)
    ),
    "ppml"
  )

  # the mean-log benchmark prints bare identified-set ranges, with no bootstrap
  # envelope beneath them, so its spanner does not promise one
  logols_parts <- logvar_logols_table_parts(n_obs)
  logols_parts$headers <- headers
  pages <- c(pages, page(
    logols_parts,
    "Panel B: Log-variance equation (mean-log benchmark)",
    "log-variance equation (mean-log benchmark)",
    build_logols_panel_notes(
      tau_baseline, log_var_eq$n_cross[[paper_tau_key(tau_baseline)]]
    ),
    "logols",
    PAPER_OVERLEAF_SET_LABEL_BARE
  ))

  harvey <- paper_logvar_result("harvey", required = FALSE)
  harvey_parts <- NULL
  if (!is.null(harvey)) {
    harvey_parts <- logvar_estimator_panel_parts(
      harvey, n_obs, tau_display, LOGVAR_HARVEY_PANEL_SPEC,
      logvar_harvey_se_type, LOGVAR_HARVEY_SE_TYPES,
      log_var_eq_set_boot$harvey,
      PAPER_REPORTING_CONTROL$cells$log_variance,
      logvar_boot_point_stat(log_var_eq_set_boot, "harvey")
    )
    pages <- c(pages, page(
      harvey_parts,
      "Panel B: Log-variance equation (Gaussian multiplicative variance)",
      "log-variance equation (Gaussian multiplicative variance)",
      c(
        build_harvey_panel_notes(
          harvey, tau_baseline, LOGVAR_HARVEY_CONTROL$grid_cap,
          LOGVAR_HARVEY_CONTROL$fit_budget,
          se_type = logvar_harvey_se_type,
          se_hac_lags = logvar_harvey_se_hac_lags,
          set_endpoint_inference = TRUE
        ),
        build_logvar_set_inference_notes(log_var_eq_set_boot)
      ),
      "harvey"
    ))
  }

  # LAD runs only behind the quantreg dependency gate, so its page is present
  # exactly when the estimator is. The document itself is always produced.
  lad <- paper_logvar_result("lad", required = FALSE)
  if (!is.null(lad)) {
    pages <- c(pages, page(
      logvar_estimator_panel_parts(
        lad, lad$sample$n, tau_display, LOGVAR_LAD_PANEL_SPEC,
        NULL, NULL, NULL,
        PAPER_REPORTING_CONTROL$cells$lad,
        NULL
      ),
      "Panel B: Log-variance equation (conditional median)",
      "log-variance equation (conditional median)",
      build_lad_panel_notes(
        lad, tau_baseline, LOGVAR_LAD_CONTROL$grid_cap,
        LOGVAR_LAD_CONTROL$fit_budget
      ),
      "lad",
      PAPER_OVERLEAF_SET_LABEL_BARE
    ))
  }

  logvar_publish_variance_panel_fragments(
    harvey_parts, logols_parts, headers, coef_rules
  )

  publish_latex_artifact(PAGES_ID, pages)
  cat(sprintf(
    "estimator pages: %d pages published\n", sum(pages == "\\clearpage")
  ))
})
