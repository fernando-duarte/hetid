# Result-bearing policy for the mean-equation heteroskedasticity diagnostics.

paper_source_once(paper_path("config", "reporting.R"))

PAPER_HETEROSKEDASTICITY_CONTROL <- list(
  fitted_sd_ratio_cutoff = 1e-3,
  suites = list(
    A = c("White", "BP", "GQ", "Harvey"),
    B = c("White", "BP", "GQ", "Harvey", "Anscombe")
  ),
  gq_deflator_position = 2L,
  gq_alternative = "two.sided",
  rejection_level = "two_stars",
  caption_tests = c("BP", "GQ", "ARCH")
)

paper_hetero_test_catalog <- function(
  control = PAPER_HETEROSKEDASTICITY_CONTROL
) {
  unique(unname(unlist(control$suites, use.names = FALSE)))
}

stopifnot(
  is.numeric(PAPER_HETEROSKEDASTICITY_CONTROL$fitted_sd_ratio_cutoff),
  length(PAPER_HETEROSKEDASTICITY_CONTROL$fitted_sd_ratio_cutoff) == 1L,
  is.finite(PAPER_HETEROSKEDASTICITY_CONTROL$fitted_sd_ratio_cutoff),
  PAPER_HETEROSKEDASTICITY_CONTROL$fitted_sd_ratio_cutoff > 0,
  identical(names(PAPER_HETEROSKEDASTICITY_CONTROL$suites), c("A", "B")),
  PAPER_HETEROSKEDASTICITY_CONTROL$gq_deflator_position >= 1L,
  PAPER_HETEROSKEDASTICITY_CONTROL$gq_alternative %in%
    c("greater", "less", "two.sided"),
  PAPER_HETEROSKEDASTICITY_CONTROL$rejection_level %in%
    names(PAPER_REPORTING_CONTROL$significance),
  length(PAPER_HETEROSKEDASTICITY_CONTROL$caption_tests) > 0L,
  !"CW" %in% paper_hetero_test_catalog()
)
