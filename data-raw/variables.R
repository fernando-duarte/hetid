# Provenance for data/variables.RData.
#
# `variables` is imported VERBATIM from the VFCI macro_dynamics repository
# (https://github.com/VFCI/macro_dynamics) and ships as-is, by design. Do NOT
# transform, re-save, or re-date the file here: hetid must bundle exactly the
# artifact that repository produces. In particular its `date` column carries
# quarter-START labels (1962-01-01, ...); the package-wide period-end
# convention is applied by consumers at ingestion (get_bundled_variables()
# internally, to_period_end() for users merging with ACM extracts).
#
# To update: copy the new variables.RData from macro_dynamics into data/,
# then run this script from the package root as a sanity check:
#   Rscript data-raw/variables.R

source_env <- new.env()
load("data/variables.RData", envir = source_env)
variables <- source_env$variables

stopifnot(
  is.data.frame(variables),
  inherits(variables$date, "Date"),
  anyDuplicated(variables$date) == 0L
)
cat(
  "variables:", nrow(variables), "rows x", ncol(variables), "cols,",
  format(min(variables$date)), "to", format(max(variables$date)), "\n"
)
