# Paper-spec estimator contract: moments (I=1/J=1), the theta identified set,
# beta1(theta) recovery, tau*, the heteroskedasticity-relevance tests, and the
# moving-block bootstrap band. Run from the package root:
#   Rscript scripts/utils/tests/test_paper_spec_estimator.R
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
  compute_paper_spec_residuals(data)
)
e <- compute_paper_spec_estimator(r)

# Moments are a valid I=1 / J=1 container.
check("moments are a hetid_moments object", inherits(e$moments, "hetid_moments"))
check("I = 1 (one component)", attr(e$moments, "n_components") == 1L)
check("J = 1 (one instrument)", nrow(e$moments$r_i_0) == 1L)

# Scalar-theta recovery identity: beta1(theta) = beta1R - beta2R . theta.
rec <- r$beta1r - r$beta2r * e$theta_point
b1p <- stats::setNames(e$coef_table$point[match(names(r$beta1r), e$coef_table$coef)], names(r$beta1r))
check(
  "beta1(theta) point column = beta1R - beta2R . theta",
  isTRUE(all.equal(unname(rec), unname(b1p), tolerance = 1e-8))
)

# Coefficient table shape: design coefs + theta, with the three columns.
check("coef_table has design coefs + theta row", nrow(e$coef_table) == length(r$beta1r) + 1L)
check("theta row present", "theta" %in% e$coef_table$coef)
check(
  "OLS / point / set columns present",
  all(c("ols", "point", "set_lower", "set_upper") %in% names(e$coef_table))
)

# tau* and the identified set.
check("tau_star in [0, OPT_TAU_CAP]", is.finite(e$tau_star) && e$tau_star >= 0 && e$tau_star <= OPT_TAU_CAP)
check(
  "set_status is one of the four states",
  e$set_status %in% c("interval", "unbounded", "empty", "point")
)
check(
  "width finite when set_status == interval",
  e$set_status != "interval" || is.finite(e$width)
)

# Heteroskedasticity p-values are valid probabilities (NA allowed for a failed test).
pv <- e$hetero_pvals
check("hetero p-values in [0, 1] (or NA)", all(is.na(pv) | (pv >= 0 & pv <= 1)))

# kappa(Q) is NOT reported (vacuous at I = J = 1).
flat <- unlist(e, use.names = TRUE)
check(
  "kappa(Q) / cond is absent from the bundle",
  !any(grepl("cond|kappa", names(flat), ignore.case = TRUE))
)

# Relevance diagnostics present; Z lies in span(X_t) so its mean slope is ~0.
check(
  "relevance diagnostics present",
  all(c("cov_z_w2sq", "cor_z_w2sq", "mean_slope", "cor_w1_w2") %in% names(e$relevance))
)

# An uncertified, fail-closed solve is handled by the status classifier
# without error.
check(
  "set-status classifier handles a fail-closed solve",
  .classify_set_status(
    data.frame(
      lower = NA, upper = NA, width = NA,
      bounded_lower = FALSE, bounded_upper = FALSE,
      valid_lower = FALSE, valid_upper = FALSE
    )
  ) %in% c("unbounded", "unreliable")
)

# Bootstrap band runs (small reps) and returns finite summaries.
boot <- compute_paper_spec_bootstrap(r, b_reps = 8L, block = 15L)
check("bootstrap returns a tau_star band", is.finite(boot$tau_star["median"]))
check(
  "bootstrap draws have the four quantities",
  identical(colnames(boot$draws), c("tau_star", "lower", "upper", "cov"))
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
