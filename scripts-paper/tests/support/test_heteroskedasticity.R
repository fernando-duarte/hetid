# Contract checks for the paper-owned heteroskedasticity test battery. Run from root:
# Rscript scripts-paper/tests/support/test_heteroskedasticity.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("support", "diagnostics", "heteroskedasticity_tests.R"))

.pass <- 0L
.fail <- 0L
check <- function(label, condition) {
  if (isTRUE(condition)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

suite <- c("White", "BP", "GQ", "Harvey", "Anscombe", "CW")
pvals <- list(
  White = c(0.01, 0.20, 0.03, NA),
  BP = c(0.04, 0.04, 0.04, NA),
  GQ = c(0.50, 0.60, 0.70, NA),
  Harvey = c(0.001, 0.30, 0.30, NA),
  Anscombe = c(0.001, 0.04, 0.20, NA),
  CW = c(0.10, 0.049, 0.30, NA)
)
tests_df <- data.frame(maturity = c(24, 60, 108, 120), stringsAsFactors = FALSE)
for (name in suite) {
  tests_df[[paste0(name, "_stat")]] <- c(1, 2, 3, NA)
  tests_df[[paste0(name, "_pval")]] <- pvals[[name]]
}

summary_df <- summarize_hetero_tests(tests_df, significance_level = 0.05)
check("summary covers the full test suite", nrow(summary_df) == 6L)
check("Anscombe row is present", "Anscombe" %in% summary_df$Test)
check(
  "Anscombe rejections use its own p-value column",
  summary_df$Rejections[summary_df$Test == "Anscombe"] == 2L
)
check(
  "rejection counts are correct across tests",
  identical(summary_df$Rejections, c(2L, 3L, 0L, 1L, 2L, 1L))
)
check("missing results are excluded from denominators", all(summary_df$Total == 3L))
check(
  "percentages use the computed-test denominators",
  isTRUE(all.equal(
    summary_df$Percentage,
    round(100 * c(2, 3, 0, 1, 2, 1) / 3, 1)
  ))
)

tests_all_na <- tests_df
tests_all_na$GQ_pval <- NA_real_
summary_na <- summarize_hetero_tests(tests_all_na)
check(
  "an all-missing test has zero total and missing percentage",
  summary_na$Total[summary_na$Test == "GQ"] == 0L &&
    is.na(summary_na$Percentage[summary_na$Test == "GQ"])
)

tests_missing <- tests_df[, setdiff(names(tests_df), "CW_pval")]
missing_error <- tryCatch(
  {
    summarize_hetero_tests(tests_missing)
    NULL
  },
  error = function(error) conditionMessage(error)
)
check(
  "a missing p-value column produces an informative error",
  !is.null(missing_error) && grepl("CW_pval", missing_error, fixed = TRUE)
)

summary_sub <- summarize_hetero_tests(
  tests_df,
  significance_level = 0.05,
  test_names = c("White", "BP", "GQ", "Harvey")
)
check("subset summary has one row per requested test", nrow(summary_sub) == 4L)
check(
  "subset summary excludes unrequested tests",
  !any(c("Anscombe", "CW") %in% summary_sub$Test)
)
subset_error <- tryCatch(
  {
    summarize_hetero_tests(tests_df, test_names = c("White", "Nope"))
    NULL
  },
  error = function(error) conditionMessage(error)
)
check(
  "an absent requested test produces an informative error",
  !is.null(subset_error) && grepl("Nope_pval", subset_error, fixed = TRUE)
)

# A synthetic fit exercises suite selection and Goldfeld-Quandt aiming.
set.seed(11)
synthetic <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
synthetic_fit <- lm(y ~ ., data = synthetic)
reduced <- c("White", "BP", "GQ", "Harvey")
result_sub <- perform_all_hetero_tests(
  synthetic_fit,
  "synthetic",
  tests = reduced,
  gq_deflator = "x2",
  gq_alternative = "two.sided"
)
check(
  "selected tests produce exactly their statistic and p-value columns",
  identical(
    sort(setdiff(names(result_sub), "Variable")),
    sort(paste0(rep(reduced, each = 2), c("_stat", "_pval")))
  )
)
check(
  "aimed Goldfeld-Quandt returns a proper p-value",
  result_sub$GQ_pval > 0 && result_sub$GQ_pval < 1
)
result_full <- perform_all_hetero_tests(synthetic_fit, "synthetic")
check(
  "default run retains the full test column set",
  all(paste0(rep(suite, each = 2), c("_stat", "_pval")) %in% names(result_full))
)
result_gq_x1 <- perform_all_hetero_tests(
  synthetic_fit,
  "synthetic",
  tests = "GQ",
  gq_deflator = "x1",
  gq_alternative = "two.sided"
)
check(
  "Goldfeld-Quandt honors the requested deflator",
  !isTRUE(all.equal(result_gq_x1$GQ_stat, result_sub$GQ_stat))
)
deflator_error <- tryCatch(
  {
    perform_all_hetero_tests(synthetic_fit, tests = "GQ", gq_deflator = "zz")
    NULL
  },
  error = function(error) conditionMessage(error)
)
check(
  "an unknown deflator error names it and the available regressors",
  !is.null(deflator_error) && grepl("zz", deflator_error, fixed = TRUE) &&
    grepl("x1", deflator_error, fixed = TRUE)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
