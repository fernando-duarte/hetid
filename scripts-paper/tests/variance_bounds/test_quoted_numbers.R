# Contract and derivation checks for the quoted-numbers builders (five-row
# name/value frame plus the derived markdown note). Run from the package root:
# Rscript scripts-paper/tests/variance_bounds/test_quoted_numbers.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path(
  "variance_bounds", "quoted", "build_quoted_numbers.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# Distinct per-column values; the q arm binds at 3 months only and carries a
# legitimate Inf fallback at 9 months; Variance_Bound is the pmin of the arms
fixture_df <- data.frame(
  Maturity = c(3, 6, 9),
  Variance_Bound = c(1e-9, 4e-9, 2e-9),
  Expected_SDF_Bound = c(2.5e-9, 1e-9, 9e-10),
  News_Envelope_Bound = c(2e-9, 4e-9, 2e-9),
  News_Q_Bound = c(1e-9, 5e-9, Inf)
)
fixture_y <- c(0.010, 0.012, 0.011, 0.013)

quoted <- variance_bounds_quoted_numbers(fixture_df, fixture_y)
check(
  "five rows carry the canonical names in order",
  identical(quoted$name, VARIANCE_BOUNDS_QUOTED_NAMES) &&
    nrow(quoted) == 5L
)
typical_fix <- stats::sd(diff(fixture_y))
max_sd_fix <- max(
  sqrt(fixture_df$Variance_Bound), sqrt(fixture_df$Expected_SDF_Bound)
)
check(
  "fixture arithmetic is exact for all five values",
  identical(quoted$value[[1L]], max(fixture_df$Variance_Bound)) &&
    identical(quoted$value[[2L]], max(fixture_df$Expected_SDF_Bound)) &&
    identical(quoted$value[[3L]], 1e4 * max_sd_fix) &&
    identical(quoted$value[[4L]], typical_fix) &&
    identical(quoted$value[[5L]], 100 * max_sd_fix / typical_fix)
)

md <- variance_bounds_quoted_md_lines(quoted, fixture_df, "fixture sample")
md_has <- function(txt) any(grepl(txt, md, fixed = TRUE))
check(
  "md derives the maturity count and grid from the frame",
  md_has("the 3 quarterly maturities i = 3, 6, ..., 9 months")
)
check(
  "md derives the binding-arm months and both argmax months",
  md_has("at 1 of the 3 maturities") && md_has("(months: 3)") &&
    md_has("the maximum is attained at 6 months.") &&
    md_has("The maximum is attained at 3 months.")
)
check(
  "md provenance names the pipeline driver",
  md_has("scripts-paper/variance_bounds/quoted/run.R")
)

# no-binding branch: the envelope arm wins everywhere
none_df <- data.frame(
  Maturity = c(3, 6, 9),
  Variance_Bound = c(1e-9, 2e-9, 3e-9),
  Expected_SDF_Bound = c(1e-9, 2e-9, 3e-9),
  News_Envelope_Bound = c(1e-9, 2e-9, 3e-9),
  News_Q_Bound = c(2e-9, 3e-9, 4e-9)
)
none_md <- variance_bounds_quoted_md_lines(
  variance_bounds_quoted_numbers(none_df, fixture_y), none_df, "s"
)
check(
  "md handles the no-binding branch explicitly",
  any(grepl("at none of the 3 maturities", none_md, fixed = TRUE))
)

rejects <- function(expr) {
  msg <- tryCatch(
    {
      force(expr)
      ""
    },
    error = conditionMessage
  )
  nzchar(msg)
}
mutate_df <- function(...) {
  out <- fixture_df
  patch <- list(...)
  for (nm in names(patch)) out[[nm]] <- patch[[nm]]
  out
}
bad_frames <- list(
  "missing arm column" = fixture_df[, setdiff(names(fixture_df), "News_Q_Bound")],
  "non-numeric column" = mutate_df(News_Envelope_Bound = c("a", "b", "c")),
  "nonpositive reported bound" = mutate_df(
    Variance_Bound = c(0, 4e-9, 2e-9), News_Q_Bound = c(0, 5e-9, Inf),
    News_Envelope_Bound = c(0, 4e-9, 2e-9)
  ),
  "NA q arm" = mutate_df(News_Q_Bound = c(NA_real_, 5e-9, Inf)),
  "unsorted maturities" = mutate_df(Maturity = c(3, 9, 6)),
  "non-constant maturity step" = mutate_df(Maturity = c(3, 6, 12)),
  "nonpositive maturities" = mutate_df(Maturity = c(-3, 0, 3)),
  "too-short frame" = fixture_df[1:2, ],
  "reported bound not the pmin of the arms" = mutate_df(
    Variance_Bound = c(2e-9, 4e-9, 2e-9)
  )
)
for (label in names(bad_frames)) {
  check(
    paste("frame contract rejects:", label),
    rejects(variance_bounds_quoted_numbers(bad_frames[[label]], fixture_y))
  )
}
check(
  "yield contract rejects NA values",
  rejects(variance_bounds_quoted_numbers(fixture_df, c(0.01, NA_real_, 0.02)))
)
check(
  "yield contract rejects too-short series",
  rejects(variance_bounds_quoted_numbers(fixture_df, c(0.01, 0.02)))
)
check(
  "md contract rejects a quoted/bounds split-brain mismatch",
  rejects(variance_bounds_quoted_md_lines(quoted, none_df, "s"))
)
bad_names <- quoted
bad_names$name[1L] <- "max_bound_sdf_newz"
check(
  "md contract rejects malformed quoted names",
  rejects(variance_bounds_quoted_md_lines(bad_names, fixture_df, "s"))
)
check(
  "md contract rejects an empty sample description",
  rejects(variance_bounds_quoted_md_lines(quoted, fixture_df, ""))
)

check(
  "manifest routes the csv to the diagnostics root",
  grepl(
    "output/diagnostics/approximation_error_quoted_numbers[.]csv$",
    artifact_path("quoted_numbers_csv")
  )
)
check(
  "manifest routes the md note to the reports root",
  grepl(
    "output/reports/approximation_error_quoted_numbers[.]md$",
    artifact_path("quoted_numbers_md")
  )
)

.test$finish()
