# Console summary and artifact serialization for the joint-null theta_R = 0
# distance diagnostic. Definitions only: the compact per-tau print and the CSV
# plus versioned RDS writer with its typed round-trip guards. Sourced by the
# driver run.R (which the offline suite sources too), so these
# functions are available whether or not the pipeline objects exist.

paper_source_once(paper_path(
  "support", "artifacts", "typed_artifacts.R"
))

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
    warn <- character(0)
    if (isTRUE(row$box_active)) warn <- c(warn, "arg-min on a display-box edge")
    if (isTRUE(row$sparse_grid)) warn <- c(warn, "sparse feasible grid")
    if (isTRUE(row$linf_trigger)) {
      warn <- c(warn, sprintf("L-inf sensitivity %s", row$linf_sensitivity_status))
    }
    tag <- if (length(warn)) sprintf("  [%s]", paste(warn, collapse = "; ")) else ""
    cat(sprintf(
      "  tau = %.3g: scaled d2 = %.4g (d_inf = %.4g) | %s | %s%s\n",
      row$tau_display, row$scaled_l2, row$scaled_linf,
      row$status, row$membership_result, tag
    ))
    qtr_txt <- if (is.null(row$nearest_crossing_qtr) ||
      is.na(row$nearest_crossing_qtr)) {
      "--"
    } else {
      row$nearest_crossing_qtr
    }
    cat(sprintf(
      "    arg-min b = (%s); min |eps_hat| = %.3g at %s; intercept theta0 = %.4g\n",
      paste(
        sprintf("%.4g", paper_record_doubles(row, fields$mean)),
        collapse = ", "
      ),
      row$min_abs_eps, qtr_txt, row$theta0
    ))
  }
  cat(paste0(
    "  Note: 'witness' means one feasible b_N makes all four slopes numerically ",
    "zero; 'not demonstrated' is not a test rejection or a globally certified ",
    "exclusion.\n"
  ))
  invisible(rows)
}

# Flatten the per-tau rows to one typed data.frame; a missing scalar becomes a
# typed NA promoted to the column type by the populated rows. Column order is
# the first row's field order, shared by the CSV write and the read-back.
.logvar_joint_null_flatten <- function(rows) {
  cols <- unique(unlist(lapply(rows, names)))
  vals <- lapply(cols, function(cn) {
    do.call(c, lapply(rows, function(r) if (is.null(r[[cn]])) NA else r[[cn]]))
  })
  names(vals) <- cols
  as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
}

# Write the versioned RDS and the flat CSV, then require both to round-trip:
# identical() for the whole RDS object and column-wise agreement for the CSV read
# back under the manifest colClasses. nearest_crossing_qtr is validated against
# the canonical year-quarter pattern (or NA) before the write.
write_logvar_joint_null_artifacts <- function(object, csv_path, rds_path) {
  paper_write_exact_rds(
    object,
    rds_path,
    "log_var_eq_joint_null"
  )
  flat <- .logvar_joint_null_flatten(object$rows)
  crossing_qtr <- flat[["nearest_crossing_qtr"]]
  if (!is.null(crossing_qtr)) {
    paper_validate_yearquarter(
      crossing_qtr,
      "nearest_crossing_qtr",
      "log_var_eq_joint_null"
    )
  }
  paper_write_typed_csv(
    flat,
    csv_path,
    "log_var_eq_joint_null"
  )
  invisible(object)
}
