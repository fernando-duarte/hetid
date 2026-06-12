# Tests for hetero_test_utils.R: the summarize_hetero_tests pure function,
# plus the test-selection and Goldfeld-Quandt aiming arguments of
# perform_all_hetero_tests (exercised on small synthetic fits).
# Run from the package root: Rscript scripts/utils/tests/test_hetero_test_utils.R
source("scripts/utils/hetero_test_utils.R")

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

suite <- c("White", "BP", "GQ", "Harvey", "Anscombe", "CW")

# Three computed maturities plus one errored row (all-NA, the suite_na_row
# fallback shape used by stage 02)
pvals <- list(
  White = c(0.01, 0.20, 0.03, NA),
  BP = c(0.04, 0.04, 0.04, NA),
  GQ = c(0.50, 0.60, 0.70, NA),
  Harvey = c(0.001, 0.30, 0.30, NA),
  Anscombe = c(0.001, 0.04, 0.20, NA),
  CW = c(0.10, 0.049, 0.30, NA)
)
tests_df <- data.frame(maturity = c(24, 60, 108, 120), stringsAsFactors = FALSE)
for (nm in suite) {
  tests_df[[paste0(nm, "_stat")]] <- c(1, 2, 3, NA)
  tests_df[[paste0(nm, "_pval")]] <- pvals[[nm]]
}

summary_df <- summarize_hetero_tests(tests_df, significance_level = 0.05)

check("summary covers the full six-test suite", nrow(summary_df) == 6)
check("Anscombe row present", "Anscombe" %in% summary_df$Test)
check(
  "Anscombe rejections counted from its p-value column",
  summary_df$Rejections[summary_df$Test == "Anscombe"] == 2
)
check(
  "rejection counts correct across tests",
  identical(summary_df$Rejections, c(2L, 3L, 0L, 1L, 2L, 1L))
)
check(
  "NA rows excluded from each test's denominator",
  all(summary_df$Total == 3)
)
check(
  "percentages are rates among computed tests",
  isTRUE(all.equal(
    summary_df$Percentage,
    round(100 * c(2, 3, 0, 1, 2, 1) / 3, 1)
  ))
)

# A test whose column is entirely NA keeps Total = 0 and Percentage = NA
tests_all_na <- tests_df
tests_all_na$GQ_pval <- NA_real_
summary_na <- summarize_hetero_tests(tests_all_na)
check(
  "all-NA column yields Total 0 and Percentage NA",
  summary_na$Total[summary_na$Test == "GQ"] == 0 &&
    is.na(summary_na$Percentage[summary_na$Test == "GQ"])
)

# A missing suite column must error informatively, not return silent NA
tests_missing <- tests_df[, setdiff(names(tests_df), "CW_pval")]
err <- tryCatch(
  {
    summarize_hetero_tests(tests_missing)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "missing p-value column errors and names the column",
  !is.null(err) && grepl("CW_pval", err, fixed = TRUE)
)

# Suite subsetting: summarize only the requested tests
summary_sub <- summarize_hetero_tests(
  tests_df,
  significance_level = 0.05,
  test_names = c("White", "BP", "GQ", "Harvey")
)
check("subset summary has one row per requested test", nrow(summary_sub) == 4)
check(
  "subset summary excludes unrequested tests",
  !any(c("Anscombe", "CW") %in% summary_sub$Test)
)
err_sub <- tryCatch(
  {
    summarize_hetero_tests(tests_df, test_names = c("White", "Nope"))
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "requesting an absent test errors and names its column",
  !is.null(err_sub) && grepl("Nope_pval", err_sub, fixed = TRUE)
)

# perform_all_hetero_tests: test selection and the aimed Goldfeld-Quandt
set.seed(11)
syn <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
syn_fit <- lm(y ~ ., data = syn)
reduced <- c("White", "BP", "GQ", "Harvey")
res_sub <- perform_all_hetero_tests(
  syn_fit, "synthetic",
  tests = reduced,
  gq_deflator = "x2", gq_alternative = "two.sided"
)
check(
  "selected tests produce exactly their columns",
  identical(
    sort(setdiff(names(res_sub), "Variable")),
    sort(paste0(rep(reduced, each = 2), c("_stat", "_pval")))
  )
)
check(
  "aimed GQ returns a proper p-value",
  res_sub$GQ_pval > 0 && res_sub$GQ_pval < 1
)
res_full <- perform_all_hetero_tests(syn_fit, "synthetic")
check(
  "default run keeps the full six-test column set",
  all(paste0(rep(suite, each = 2), c("_stat", "_pval")) %in% names(res_full))
)
res_gq_x1 <- perform_all_hetero_tests(
  syn_fit, "synthetic",
  tests = "GQ", gq_deflator = "x1", gq_alternative = "two.sided"
)
check(
  "gq_deflator is honored (different ordering, different statistic)",
  !isTRUE(all.equal(res_gq_x1$GQ_stat, res_sub$GQ_stat))
)
err_defl <- tryCatch(
  {
    perform_all_hetero_tests(syn_fit, tests = "GQ", gq_deflator = "zz")
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "unknown gq_deflator errors and names the available regressors",
  !is.null(err_defl) && grepl("zz", err_defl, fixed = TRUE) &&
    grepl("x1", err_defl, fixed = TRUE)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
