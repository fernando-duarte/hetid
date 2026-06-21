# W1/W2/instrument calendar-date alignment in compute_identification_residuals.
# Regression guard against the silent instrument shift that appears once W2 ALSO
# conditions on the common X_t and drops the same H-1 leading lag rows as W1:
# length checks alone pass while z_mat stays anchored to panel row 1, shifting
# every instrument by H-1 quarters. We assert ACTUAL calendar-date equality
# across w1, w2, and pcs_aligned for all three conditioning modes, and that the
# row count is derived (never hard-coded). Needs the Stage-01 data.rds fixture.
# Run from the package root:
#   Rscript scripts/utils/tests/test_identification_alignment.R
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

if (!file.exists(DATA_RDS_PATH)) {
  cat("SKIP  data.rds fixture absent at", DATA_RDS_PATH, "\n")
  cat("      run Stage 01 first; alignment test needs the merged panel\n")
  quit(status = 0L)
}

inputs <- load_identification_inputs()
data <- inputs$data

# Each mode pairs a y1_lags value with the news-projection switch state. The
# alignment must hold identically for all three, and the no-lag mode must stay
# byte-identical to the legacy PC-only behavior.
modes <- list(
  list(label = "y1_lags=0 (no common conditioning)", lags = 0L, b0 = FALSE),
  list(label = "y1_lags=4 estimate-B", lags = 4L, b0 = FALSE),
  list(label = "y1_lags=4 impose-B=0", lags = 4L, b0 = TRUE)
)

dates_of <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.character(x)
}

for (m in modes) {
  Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = if (m$b0) "TRUE" else "FALSE")
  resid <- suppressMessages(
    compute_identification_residuals(data, y1_lags = m$lags)
  )

  n1 <- length(resid$w1)
  n2 <- nrow(resid$w2)
  nz <- nrow(resid$pcs_aligned)
  check(
    paste0(m$label, ": w1, w2, pcs_aligned have EQUAL row counts"),
    n1 == n2 && n2 == nz && n1 > 0L
  )

  # The result must surface the common retained calendar dates (A5 needs them).
  rd <- resid$dates
  check(
    paste0(m$label, ": result exposes common retained dates of the right length"),
    !is.null(rd) && length(rd) == n1
  )

  # The real regression guard: ACTUAL date equality, not just equal counts. The
  # buggy length-offset path would leave pcs_aligned anchored to panel row 1
  # while w1/w2 start H quarters later, so their dates would diverge.
  w1_dates <- dates_of(resid$w1_result$dates)
  common_dates <- dates_of(rd)
  check(
    paste0(m$label, ": w1 residual dates equal the common retained dates"),
    identical(w1_dates, common_dates)
  )

  # pcs_aligned must carry the SAME calendar dates: the instrument row paired
  # with a residual is the one for that residual's period (never off by one).
  z_dates <- dates_of(attr(resid$pcs_aligned, "dates"))
  check(
    paste0(m$label, ": pcs_aligned carries dates identical to w1/common"),
    identical(z_dates, common_dates)
  )
  check(
    paste0(m$label, ": pcs_aligned first/last date equal w1 first/last date"),
    !is.null(z_dates) && length(z_dates) == length(w1_dates) &&
      z_dates[1] == w1_dates[1] &&
      z_dates[length(z_dates)] == w1_dates[length(w1_dates)]
  )
}

# y1_lags = 0 must remain byte-identical to the legacy PC-only path: the result
# residuals and instruments must equal a direct recomputation that bypasses the
# common-conditioning machinery entirely.
Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = "FALSE")
resid0 <- suppressMessages(compute_identification_residuals(data, y1_lags = 0L))
yc <- grep("^y[0-9]+$", names(data), value = TRUE)
tc <- grep("^tp[0-9]+$", names(data), value = TRUE)
pcs_mat <- as.matrix(data[, paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(4L))])
w1_legacy <- compute_w1_residuals(n_pcs = 4L, data = data, y1_lags = 0L)
w2_legacy <- compute_w2_residuals(
  data[, yc], data[, tc],
  maturities = DEFAULT_ID_MATURITIES, n_pcs = 4L, pcs = pcs_mat,
  step = NEWS_STEP, dates = data$date
)
w2_legacy_mat <- do.call(cbind, w2_legacy$residuals)
# Legacy alignment: same-length here (no lags), so the old offset trim was a
# no-op; the residuals line up directly.
check(
  "y1_lags=0: w1 residuals byte-identical to legacy PC-only path",
  identical(unname(resid0$w1), unname(w1_legacy$residuals))
)
check(
  "y1_lags=0: w2 residual matrix byte-identical to legacy PC-only path",
  identical(unname(resid0$w2), unname(w2_legacy_mat))
)

Sys.unsetenv("HETID_IMPOSE_NEWS_PROJECTION_ZERO")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
