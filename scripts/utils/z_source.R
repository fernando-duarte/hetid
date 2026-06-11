# Z-source hook: which matrix plays the INSTRUMENT role in the
# moments (the spec's Z_t). The structural first stage stays
# X_t = (1, PC_t) regardless -- the VFCI application defines it; see
# the spec's application section. Default (HETID_Z_SOURCE unset)
# returns the supplied default matrix unchanged, byte-for-byte.
#
# To run the pipeline on custom instruments, set HETID_Z_SOURCE to an
# R script that defines build_z(data) returning a numeric T x K
# matrix with column names, where data is the stage's merged
# quarterly data frame.

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
  if (ncol(z) != ncol(default)) {
    stop(
      "HETID_Z_SOURCE returned ", ncol(z), " instruments but the ",
      "pipeline's fixed-width specs (baseline gammas, tau sizing, ",
      "optimizer seeds) are built for ", ncol(default), ". The hook ",
      "supports same-width swaps only; for other widths use the ",
      "package API directly (see scripts/examples/custom_z_demo.R)"
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
