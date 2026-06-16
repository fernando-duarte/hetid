# Paper-spec residuals contract: the news-PC collapse (Y2, I = 1) + single
# de-meaned VFCI instrument (Z, J = 1). The load-bearing check is the linearity
# identity W2 = Y2 - X . beta2R (the raw-projection combine equals residualizing
# the news PC on X_t, with the correct intercept). Run from the package root:
#   Rscript scripts/utils/tests/test_paper_spec_residuals.R
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

z_hook <- here::here("scripts/z_sources/vfci_demeaned.R")
data <- as.data.frame(readRDS(DATA_RDS_PATH))

r <- withr::with_envvar(
  c(HETID_Z_SOURCE = z_hook),
  compute_paper_spec_residuals(data, news_pc_scale = "correlation")
)

# Linearity identity (load-bearing): W2 == Y2 - cbind(1, X) %*% beta2R, where X
# is the aligned common design the function returns. Confirms the raw-projection
# combine equals residualizing the single news PC on X_t with the right intercept.
x_mat <- cbind(`(Intercept)` = 1, r$design_aligned)
pred <- as.numeric(x_mat %*% r$beta2r[colnames(x_mat)])
check(
  "linearity identity W2 = Y2 - X beta2R (raw-projection, correct intercept)",
  isTRUE(all.equal(r$w2, r$y2 - pred, tolerance = 1e-8))
)

# Centered-moment invariance: shifting the instrument's mean leaves the moments
# (hence the identified set) unchanged -- the settled Round-1 econometrics point.
zmat <- matrix(r$z, ncol = 1, dimnames = list(NULL, "vfci_dm"))
zmat_shift <- matrix(r$z + 7.5, ncol = 1, dimnames = list(NULL, "vfci_dm"))
w2mat <- matrix(r$w2, ncol = 1)
m_base <- compute_identification_moments(r$w1, w2mat, zmat)
m_shift <- compute_identification_moments(r$w1, w2mat, zmat_shift)
# Invariant up to floating-point: the centered cov-hat reorders the subtraction
# under a mean shift, so equality is to ~1e-12, not bit-exact. The identified set
# and tau* are unchanged at this scale -- de-meaning is moment-safe.
check(
  "centered moments invariant to instrument mean shift (de-meaning is moment-safe)",
  isTRUE(all.equal(m_base, m_shift, tolerance = 1e-10))
)

# Alignment: all series share one length; dates strictly increasing.
lens <- c(length(r$w1), length(r$w2), length(r$y2), length(r$z), length(r$dates))
check("all aligned series share one length", length(unique(lens)) == 1L)
check("dates strictly increasing", all(diff(as.Date(r$dates)) > 0))

# No feasible maturity skipped (used_maturities are numeric months).
check(
  "no feasible maturity skipped",
  base::setequal(r$used_maturities, hetid:::default_w2_maturities(NEWS_STEP))
)

# Sign determinism: re-running yields identical loadings (the sign rule pins the
# arbitrary PCA sign).
r2 <- withr::with_envvar(
  c(HETID_Z_SOURCE = z_hook),
  compute_paper_spec_residuals(data, news_pc_scale = "correlation")
)
check("sign convention is deterministic", isTRUE(all.equal(r$news_loadings, r2$news_loadings)))

# Scale switch: covariance PCA changes the loadings (long maturities dominate).
r_cov <- withr::with_envvar(
  c(HETID_Z_SOURCE = z_hook),
  compute_paper_spec_residuals(data, news_pc_scale = "covariance")
)
check(
  "covariance PCA differs from correlation PCA",
  !isTRUE(all.equal(unname(r$news_loadings), unname(r_cov$news_loadings)))
)
# ...and covariance PCA still satisfies the linearity identity.
x_cov <- cbind(`(Intercept)` = 1, r_cov$design_aligned)
pred_cov <- as.numeric(x_cov %*% r_cov$beta2r[colnames(x_cov)])
check(
  "linearity identity holds under covariance PCA too",
  isTRUE(all.equal(r_cov$w2, r_cov$y2 - pred_cov, tolerance = 1e-8))
)

# Column match: beta2R names equal beta1R names equal the common design.
expected_cols <- c("(Intercept)", paste0("pc", 1:4), c("l.y1", "l2.y1", "l3.y1", "l4.y1"))
check("beta2R names match beta1R names", identical(names(r$beta2r), names(r$beta1r)))
check("coefficient names are the common design", identical(names(r$beta2r), expected_cols))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
