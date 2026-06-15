# Stage-04 baseline spec stamp + downstream consistency check. The pure
# comparison core (baseline_spec_mismatches) is tested directly: a matching spec
# passes, a differing field fails, and an ABSENT field fails closed (old
# pre-stamp artifact). Run from the package root:
#   Rscript scripts/utils/tests/test_baseline_spec.R
source(here::here("scripts/utils/common_settings.R"))

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

cur <- list(y1_lags = 4L, news_projection_mode = "estimate_b", z_source = "pc")

# Matching spec -> no mismatches
check(
  "identical spec yields no mismatches",
  length(baseline_spec_mismatches(cur, cur)) == 0L
)

# Differing lag count -> a mismatch naming the field
m_lags <- baseline_spec_mismatches(
  modifyList(cur, list(y1_lags = 0L)), cur
)
check(
  "differing y1_lags is a mismatch",
  length(m_lags) == 1L && grepl("y1_lags", m_lags)
)

# Differing news-projection mode -> a mismatch
m_news <- baseline_spec_mismatches(
  modifyList(cur, list(news_projection_mode = "impose_b_zero")), cur
)
check(
  "differing news_projection_mode is a mismatch",
  length(m_news) == 1L && grepl("news_projection_mode", m_news)
)

# Differing z_source -> a mismatch
m_z <- baseline_spec_mismatches(
  modifyList(cur, list(z_source = "/path/to/hook.R")), cur
)
check(
  "differing z_source is a mismatch",
  length(m_z) == 1L && grepl("z_source", m_z)
)

# Absent field (old pre-stamp artifact) -> fail closed as a mismatch
old <- list(y1_lags = 4L, news_projection_mode = "estimate_b") # no z_source
m_absent <- baseline_spec_mismatches(old, cur)
check(
  "absent field fails closed (mismatch)",
  length(m_absent) == 1L && grepl("z_source.*absent|absent.*z_source", m_absent)
)

# Fully empty loaded spec -> every field flagged
m_empty <- baseline_spec_mismatches(list(), cur)
check(
  "empty loaded spec flags all three fields",
  length(m_empty) == 3L
)

# The live current resolver returns the three expected field names
live <- current_baseline_spec()
check(
  "current_baseline_spec returns the three stamped fields",
  identical(
    sort(names(live)),
    sort(c("y1_lags", "news_projection_mode", "z_source"))
  )
)

# assert_baseline_spec_current aborts on a mismatch and passes on a match
abort_msg <- tryCatch(
  {
    assert_baseline_spec_current(modifyList(live, list(y1_lags = 999L)))
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "assert_baseline_spec_current aborts on mismatch",
  is.character(abort_msg) && grepl("Stage-04 baseline", abort_msg)
)
check(
  "assert_baseline_spec_current returns invisibly on match",
  identical(assert_baseline_spec_current(live), live)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
