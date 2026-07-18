# Fixed-header, generated-field, and protocol round-trip checks for joint null.

check("joint-null rows reject fixed schema overrides", jn_try({
  inherits(
    try(
      .jn_assemble_row(list(schema_version = "stale")),
      silent = TRUE
    ),
    "try-error"
  )
}))

check("joint-null generated coefficient fields follow contract order", jn_try({
  fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  expected <- c(
    fields$mean,
    "theta0",
    fields$return,
    fields$scale
  )
  template_names <- names(.jn_row_template())
  actual <- template_names[template_names %in% expected]
  identical(actual, expected)
}))

check("joint-null CSV and RDS share a header and exact protocol", jn_try({
  test_control <- LOGVAR_JOINT_NULL_CONTROL
  test_control$grid_n <- test_control$grid_n - 2L
  row <- jn_at_tau(
    jn_box(fx$b_star, 0.5),
    fx$w1,
    fx$w2,
    fx$proj,
    di2,
    fx$eps_ref,
    fx$qs_ball,
    control = test_control
  )
  attr(row, "minima") <- NULL
  row$sample_id <- fx$sample_id
  row$tau_order <- 1L
  header <- logvar_joint_null_schema_header()
  object <- c(
    header,
    list(
      sample_id = fx$sample_id,
      protocol = test_control,
      rows = list(row)
    )
  )
  directory <- tempfile("joint-null-schema-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  csv_path <- file.path(directory, "joint_null.csv")
  rds_path <- file.path(directory, "joint_null.rds")
  write_logvar_joint_null_artifacts(object, csv_path, rds_path)
  csv <- utils::read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rds <- readRDS(rds_path)
  csv_header <- as.list(csv[1L, names(header), drop = FALSE])
  identical(rds[names(header)], header) &&
    identical(unname(csv_header), unname(header)) &&
    identical(rds$protocol, test_control)
}))
