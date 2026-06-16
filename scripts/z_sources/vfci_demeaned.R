# HETID_Z_SOURCE hook: the actual de-meaned VFCI as a single instrument (J = 1).
# Z = vfci - mean(vfci). The PCs (pc1..pc4) stay in the common design X_t; the
# instrument is the VFCI level, INCLUDING its component orthogonal to the PCs
# (this is the substance of the "actual de-meaned VFCI" choice vs the
# PC-projection PC %*% gamma_vfci). De-meaning leaves the identified set
# UNCHANGED here because the pipeline's moments are centered covariances
# (centered_cov subtracts colMeans(Z) regardless): centered_cov(Z - c, .) ==
# centered_cov(Z, .). It is kept for interpretability (zero-mean instrument).
# NB: with an UNcentered J = 1 moment this would NOT be invariant -- the
# invariance is a property of the centered moments, not a general fact.
#
# Wired into compute_identification_residuals() via HETID_Z_SOURCE (set with
# withr::with_envvar in scripts/08_paper_spec/compute_paper_spec.R), so the
# instrument is date-aligned to the response dates by the same battle-tested path
# the PC instruments use.
build_z <- function(data) {
  if (!"vfci" %in% names(data)) {
    stop(
      "vfci_demeaned.R: data has no 'vfci' column; re-run ",
      "scripts/01_data_analysis/create_data.R (it carries vfci into data.rds)"
    )
  }
  v <- data$vfci
  if (!is.numeric(v) || anyNA(v) || !all(is.finite(v)) || stats::sd(v) <= 0) {
    stop(
      "vfci_demeaned.R: vfci must be finite, non-NA, and non-constant ",
      "(a constant instrument yields an all-zero Z and degenerates identification)"
    )
  }
  z <- matrix(v - mean(v), ncol = 1)
  colnames(z) <- "vfci_dm"
  z
}
