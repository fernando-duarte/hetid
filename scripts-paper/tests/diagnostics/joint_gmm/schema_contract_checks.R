# Fixed-header, generated-field, and serialized-header checks for joint GMM.

check("joint-GMM rows reject fixed schema overrides", jg_try({
  inherits(
    try(
      logvar_joint_gmm_csv_row(list(schema_version = "stale")),
      silent = TRUE
    ),
    "try-error"
  )
}))

check("joint-GMM generated coefficient fields follow contract order", jg_try({
  fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  expected <- c(
    fields$mean,
    "theta0",
    fields$return,
    "a_l",
    "a_p",
    "gap_a_p_a_l",
    fields$variance
  )
  actual <- logvar_joint_gmm_csv_fields[
    logvar_joint_gmm_csv_fields %in% expected
  ]
  identical(actual, expected)
}))

check("joint-GMM omits controls with no executable consumer", jg_try({
  retired <- c(
    "objective_tol",
    "gmm_grid_cap",
    "candidate_eval_cap",
    "grid_n",
    "grid_floor"
  )
  spec <- logvar_joint_gmm_spec(
    logvar_joint_gmm_decision,
    sample_id = "sample",
    joint_input_id = "joint"
  )
  serialized <- c(
    names(spec$execution_constants),
    names(spec$controls),
    names(spec$budgets),
    logvar_joint_gmm_csv_fields
  )
  !any(retired %in% serialized) &&
    !any(retired %in% names(logvar_joint_gmm_constants))
}))

check("joint-GMM CSV and RDS share the fixed header", jg_try({
  header <- logvar_joint_gmm_schema_header()
  object <- c(
    header,
    list(rows = list(list(
      moment_block = "none",
      skip_reason = "gate_declined"
    )))
  )
  directory <- tempfile("joint-gmm-schema-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  csv_path <- file.path(directory, "joint_gmm.csv")
  rds_path <- file.path(directory, "joint_gmm.rds")
  write_joint_gmm_artifacts(object, csv_path, rds_path)
  csv <- utils::read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rds <- readRDS(rds_path)
  csv_header <- as.list(csv[1L, names(header), drop = FALSE])
  identical(rds[names(header)], header) &&
    identical(unname(csv_header), unname(header))
}))
