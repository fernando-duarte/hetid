# Z-source hook: which matrix plays the INSTRUMENT role in the
# moments (the spec's Z_t). The structural first stage stays
# X_t = (1, PC_t) regardless -- the VFCI application defines it; see
# the spec's application section. Default (HETID_Z_SOURCE unset)
# returns the supplied default matrix unchanged, byte-for-byte.
#
# To run the pipeline on custom instruments, set HETID_Z_SOURCE to an
# R script that defines build_z(data) returning a numeric T x K
# matrix with unique column names, any K >= 1, where data is the
# stage's merged quarterly data frame. Baseline gammas come from
# HETID_BASELINE_GAMMA (vfci requires K = 4, or a path defining
# build_gamma(moments)); the spec-comparison grid runs only
# width-matching groups.

get_identification_z <- function(data, default) {
  src <- Sys.getenv("HETID_Z_SOURCE", "")
  if (!nzchar(src)) {
    return(default)
  }
  if (!file.exists(src)) {
    stop("HETID_Z_SOURCE points to a missing file: ", src)
  }
  env <- new.env(parent = globalenv())
  sys.source(src, envir = env)
  if (!is.function(env$build_z)) {
    stop("HETID_Z_SOURCE script must define build_z(data)")
  }
  z <- env$build_z(data)
  if (!is.matrix(z) || !is.numeric(z) || is.null(colnames(z)) ||
    nrow(z) != nrow(data)) {
    stop(
      "build_z(data) must return a numeric matrix with column ",
      "names and one row per data row"
    )
  }
  if (anyDuplicated(colnames(z)) > 0) {
    stop(
      "build_z(data) returned duplicated column names; the moments ",
      "labels and weight recipes key on unique instrument names"
    )
  }
  z
}

# TRUE when the pipeline is running on hook-supplied instruments. Display
# layers branch on this so default artifacts keep their historical PC
# labels byte-for-byte while custom runs are labeled honestly.
z_source_active <- function() {
  nzchar(Sys.getenv("HETID_Z_SOURCE", ""))
}

# Consistency guard for stages that REBUILD Z from disk in a process
# separate from the one that computed the moments (stage 05's
# optimizer): the rebuilt matrix must reproduce the stored moments
# EXACTLY (tolerance 0 -- same data artifact, same hook, same
# arithmetic), else the variance normalization would whiten against
# a different Var(Z) than the constraints were built from.
assert_z_matches_moments <- function(z, w1, w2, moments) {
  recomputed <- suppressMessages(
    compute_identification_moments(w1, w2, z)
  )
  if (!isTRUE(all.equal(recomputed, moments, tolerance = 0))) {
    stop(
      "rebuilt instrument matrix does not reproduce the stored ",
      "moments; likely cause: HETID_Z_SOURCE now differs from the ",
      "stage-04 run that wrote the baseline RDS -- re-run stage 04 ",
      "or restore the hook before optimizing"
    )
  }
  invisible(z)
}
