# Regenerate data/variables.RData with canonical period-end dates.
#
# `variables` is a quarterly panel whose `date` column historically used the
# quarter-START convention (e.g. 1962-01-01). The package now uses a single
# period-END convention everywhere (last calendar day of the period), so the
# bundled dataset is normalized to the calendar quarter-end (1962-03-31, ...)
# and re-saved. Idempotent (to_period_end is idempotent).
#
# Run from the package root after changing the convention:
#   Rscript data-raw/variables.R
devtools::load_all(quiet = TRUE)

source_env <- new.env()
load("data/variables.RData", envir = source_env)
variables <- source_env$variables

variables$date <- to_period_end(variables$date, "quarterly")
stopifnot(anyDuplicated(variables$date) == 0L)

save(variables, file = "data/variables.RData", version = 2L)
