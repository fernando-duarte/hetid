# Artifact records for the 3D identified-region figures, generated from the tau
# contract. Sourced from artifact_manifest_data.R between the literal spec
# vector and the manifest assembly, so the drawn slacks stay a single source of
# truth: adding one to the contract creates its records, and no manifest row can
# survive a slack being dropped.
#
# Each slack is drawn once per unit system ("sd" scales every coefficient by its
# news-PC standard deviation, "b" plots the coefficient itself) and once with
# the OLS benchmark projected onto the walls. render_region_3d.R composes the
# same id through the helpers below and resolves its output path by that id
# rather than by a filename, so an unmanifested combination cannot be written.

REGION_FIGURE_UNITS <- c("sd", "b")
REGION_FIGURE_OLS <- c("none", "projected")

# canonical slack token, 0.05 -> "tau0p05". Scalar format() per tau: formatting
# the vector would pad 0.1 to "0.10" and rename the figure that is written.
region_figure_tau_token <- function(tau) {
  sprintf("tau%s", sub(".", "p", format(tau), fixed = TRUE))
}

region_figure_stem <- function(ols) {
  if (identical(ols, "projected")) "_ols_projected" else ""
}

region_figure_id <- function(ols, units, tau) {
  paste0(
    "mean_region", region_figure_stem(ols), "_", units, "_",
    region_figure_tau_token(tau)
  )
}

region_figure_basename <- function(ols, units, tau) {
  paste0(
    "set_id_region_3d", region_figure_stem(ols), "_", units, "_",
    region_figure_tau_token(tau), ".svg"
  )
}

.region_grid <- expand.grid(
  tau = PAPER_FIGURE_RENDER_CONTROL$region_3d$taus,
  units = REGION_FIGURE_UNITS,
  ols = REGION_FIGURE_OLS,
  stringsAsFactors = FALSE
)
.artifact_specs <- c(
  .artifact_specs,
  sprintf(
    "%s|%s|3|l|B|r",
    mapply(
      region_figure_id, .region_grid$ols, .region_grid$units,
      .region_grid$tau,
      USE.NAMES = FALSE
    ),
    mapply(
      region_figure_basename, .region_grid$ols, .region_grid$units,
      .region_grid$tau,
      USE.NAMES = FALSE
    )
  )
)
rm(.region_grid)
