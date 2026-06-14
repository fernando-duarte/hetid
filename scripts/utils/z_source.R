# Z-source hook: which matrix plays the INSTRUMENT role in the
# moments (the spec's Z_t). The structural first stage stays
# X_t = (1, PC_t) regardless -- the VFCI application defines it; see
# the spec's application section.
#
# Both branches funnel through the exported generalized-instrument front door
# hetid::build_instrument_matrix(z, transforms = NULL, include_original = TRUE),
# so every instrument matrix the pipeline uses is validated and uniquely named
# by the same code path (the K_i = 1 baseline of subsec:general_scheme). For the
# default named PC matrix this is a no-op on values and column names -- the
# returned Z is identical to the supplied default -- so default artifacts keep
# their historical PC labels and numbers exactly.
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
    z <- default
  } else {
    if (!file.exists(src)) {
      stop("HETID_Z_SOURCE points to a missing file: ", src)
    }
    env <- new.env(parent = globalenv())
    sys.source(src, envir = env)
    if (!is.function(env$build_z)) {
      stop("HETID_Z_SOURCE script must define build_z(data)")
    }
    z <- env$build_z(data)
    # Keep the strict naming/shape guard BEFORE the front door: a custom Z must
    # supply its own column names (build_instrument_matrix would otherwise
    # auto-name unnamed columns z1..zJ, silently relaxing this contract).
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
  }
  # Single front door for both branches: one validated, uniquely named code
  # path so the two cannot drift. A no-op on the named default PCs (identical
  # values and names); for a custom Z it runs after the strict guard above.
  hetid::build_instrument_matrix(z, transforms = NULL, include_original = TRUE)
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
