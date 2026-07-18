# Versioned CSV/RDS serialization for the joint moment-compatibility artifacts
# (joint-GMM, logvar-joint-gmm). Definitions only, sourced by
# artifact_schema.R: the flat typed frame assembled from the fixed
# schema rows, its column-type manifest, the locale-independent encoding, the
# column-wise CSV round-trip guard, and the writer that requires both the RDS
# identical() round trip and the CSV agreement before it returns. It uses the
# established honest-artifact idiom so the machine-readable view and bit-exact
# RDS channel stay in step.

paper_source_once(paper_path(
  "support", "artifacts", "typed_artifacts.R"
))

# Flatten the per-row result lists to one typed data.frame through the fixed
# schema assembler, so every row carries every column at its declared type.
.jg_flatten <- function(rows) {
  do.call(rbind, lapply(rows, logvar_joint_gmm_csv_row))
}

# Write the versioned RDS and the flat CSV, then require both to round-trip:
# identical() for the whole RDS object and column-wise agreement for the CSV
# read back under the manifest colClasses. Only character columns are quoted so
# an id or enum with a comma stays protected while numeric fields stay bare.
write_joint_gmm_artifacts <- function(object, csv_path, rds_path) {
  paper_write_exact_rds(
    object,
    rds_path,
    "log_var_eq_joint_gmm"
  )
  flat <- .jg_flatten(object$rows)
  paper_write_typed_csv(
    flat,
    csv_path,
    "log_var_eq_joint_gmm"
  )
  invisible(object)
}
