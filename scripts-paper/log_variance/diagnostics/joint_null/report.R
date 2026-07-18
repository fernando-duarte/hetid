# Console summary and artifact serialization for the joint-null theta_R = 0
# distance diagnostic. Definitions only: the compact per-tau print and the CSV
# plus versioned RDS writer with its typed round-trip guards. Sourced by the
# driver run.R (which the offline suite sources too), so these
# functions are available whether or not the pipeline objects exist.

paper_source_once(paper_path(
  "support", "artifacts", "typed_artifacts.R"
))
paper_source_once(paper_path(
  "support", "artifacts", "diagnostic_schema.R"
))
paper_source_once(paper_path("support", "reporting", "cells.R"))

# Compact console summary: the frozen sd-effect scales once, then one block per
# tau with the attained scaled distance, numerical status, membership verdict,
# and soft warnings (edge-riding arg-min, sparse grid, L-infinity sensitivity)
# beside the min |eps_hat| and attained intercept that flag a crossing-driven
# norm. Accepts either one flat row or a list of rows.
print_logvar_joint_null <- function(rows, scales) {
  if (!is.null(rows$status)) rows <- list(rows)
  fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  cat("Joint-null theta_R = 0 distance diagnostic (estimator logols)\n")
  cat(sprintf(
    "  frozen scales d_j = 1/sd(PC_R,j): %s\n",
    paste(sprintf("%.4g", scales$d), collapse = " ")
  ))
  for (row in rows) {
    membership_display <- if (
      identical(row$membership_result, "compatible_witness")
    ) {
      "witness"
    } else {
      "not demonstrated"
    }
    warn <- character(0)
    if (isTRUE(row$box_active)) warn <- c(warn, "arg-min on a display-box edge")
    if (isTRUE(row$sparse_grid)) warn <- c(warn, "sparse feasible grid")
    if (isTRUE(row$linf_trigger)) {
      warn <- c(warn, "L-inf sensitivity flagged")
    }
    tag <- if (length(warn)) sprintf("  [%s]", paste(warn, collapse = "; ")) else ""
    cat(sprintf(
      "  tau = %s: scaled d2 = %.4g (d_inf = %.4g) | %s | %s%s\n",
      paper_format_general(
        row$tau_display,
        PAPER_REPORTING_CONTROL$precision$console_significant
      ),
      row$scaled_l2,
      row$scaled_linf,
      row$status, membership_display, tag
    ))
    qtr_txt <- if (is.null(row$nearest_crossing_qtr) ||
      is.na(row$nearest_crossing_qtr)) {
      "--"
    } else {
      row$nearest_crossing_qtr
    }
    cat(sprintf(
      "    arg-min b = (%s); min |eps_hat| = %s at %s; intercept theta0 = %.4g\n",
      paste(
        sprintf("%.4g", paper_record_doubles(row, fields$mean)),
        collapse = ", "
      ),
      paper_format_general(
        row$min_abs_eps,
        PAPER_REPORTING_CONTROL$precision$console_significant
      ),
      qtr_txt,
      row$theta0
    ))
  }
  cat(paste0(
    "  Note: 'witness' means one feasible b_N makes all four slopes numerically ",
    "zero; 'not demonstrated' does not establish a global conclusion.\n"
  ))
  invisible(rows)
}

# Flatten the per-tau rows to one typed data.frame; a missing scalar becomes a
# typed NA promoted to the column type by the populated rows. Column order is
# the first row's field order, shared by the CSV write and the read-back.
# Write the versioned RDS and the flat CSV, then require both to round-trip:
# identical() for the whole RDS object and column-wise agreement for the CSV read
# back under the manifest colClasses. nearest_crossing_qtr is validated against
# the canonical year-quarter pattern (or NA) before the write.
write_logvar_joint_null_artifacts <- function(object, csv_path, rds_path) {
  validate <- function(frame) {
    crossing_qtr <- frame[["nearest_crossing_qtr"]]
    if (!is.null(crossing_qtr)) {
      paper_validate_yearquarter(
        crossing_qtr,
        "nearest_crossing_qtr",
        "log_var_eq_joint_null"
      )
    }
  }
  paper_write_diagnostic_artifacts(
    object,
    LOGVAR_JOINT_NULL_ROW_SCHEMA,
    csv_path,
    rds_path,
    "log_var_eq_joint_null",
    assembled = TRUE,
    validate_frame = validate
  )
}
