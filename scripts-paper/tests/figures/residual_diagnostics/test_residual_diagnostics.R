# Contract and edge-case checks for the residual-diagnostic figures. Run from the
# package root:
#   Rscript scripts-paper/tests/figures/residual_diagnostics/test_residual_diagnostics.R
# Covers the series algebra and the fail-closed guards that keep an estimator
# from silently vanishing out of a figure that still renders as though three were
# compared. Panel geometry, the reference density, the reported moments and the
# render smoke test live in the sibling test_contracts.R.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis_contract.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path(
  "log_variance", "figures", "residual_diagnostics", "data.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "residual_diagnostics", "plot.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# A fixture whose log-variance algebra is known in closed form: with theta the
# zero vector every fitted variance is one, so xi is exactly log(eps^2).
set.seed(704L)
n_obs <- 40L
pcr <- matrix(stats::rnorm(n_obs * 2L), nrow = n_obs)
colnames(pcr) <- c("l.pc1", "l.pc2")
x_mat <- logvar_design_matrix(pcr)
p_cols <- ncol(x_mat)
w2 <- matrix(stats::rnorm(n_obs), ncol = 1L)
b_point <- 0.3
eps <- stats::rnorm(n_obs)
w1 <- drop(w2 %*% b_point) + eps
inputs <- list(w1 = w1, w2 = w2, pcr = pcr, qtr = seq_len(n_obs))

zero_theta <- stats::setNames(numeric(p_cols), colnames(x_mat))
thetas <- list(ppml = zero_theta, harvey = zero_theta, logols = zero_theta)
series <- logvar_resid_diag_series(inputs, b_point, thetas)

check(
  "series carries one row per quarter per estimator",
  nrow(series) == n_obs * length(LOGVAR_RESID_DIAG_ESTIMATORS)
)
check(
  "estimator levels keep the display order",
  identical(levels(series$estimator), unname(LOGVAR_RESID_DIAG_ESTIMATORS))
)
check(
  "xi is log(eps^2) when every fitted variance is one",
  max(abs(series$xi[series$estimator == "PPML"] - log(eps^2))) < 1e-12
)
check(
  "the three estimators share a series when they share a theta",
  max(abs(
    series$xi[series$estimator == "PPML"] -
      series$xi[series$estimator == "Harvey"]
  )) == 0
)

# Round trip: build the response FROM a theta, so that theta reproduces the log
# response exactly and xi must vanish. Solving least squares for a theta instead
# would only return the projection, whose residual is not zero on an
# overdetermined design.
exact_theta <- stats::setNames(
  seq(-0.4, 0.4, length.out = p_cols), colnames(x_mat)
)
inputs_exact <- inputs
inputs_exact$w1 <- drop(w2 %*% b_point) +
  exp(drop(x_mat %*% exact_theta) / 2)
exact <- logvar_resid_diag_series(
  inputs_exact, b_point,
  list(ppml = exact_theta, harvey = exact_theta, logols = exact_theta)
)
check(
  "xi vanishes when the fit reproduces the log response",
  max(abs(exact$xi)) < 1e-10
)

# Fail-closed guards.
zero_inputs <- inputs
zero_inputs$w1[3L] <- drop(w2[3L, , drop = FALSE] %*% b_point)
zero_message <- tryCatch(
  {
    logvar_resid_diag_series(zero_inputs, b_point, thetas)
    ""
  },
  error = conditionMessage
)
check(
  "an exactly zero residual stops the figure rather than producing -Inf",
  grepl("exactly zero", zero_message, fixed = TRUE)
)

# The two estimator objects expose the tau = 0 fit differently, so the fixture
# carries both real shapes: Harvey attaches point_fit, PPML exposes none and
# keeps its fit in the Lewbel-point start bundle. A fixture that used one shape
# for both passed while the pipeline failed, so both are pinned here.
fake_registry <- list(
  list(estimator = list(
    metadata = list(estimator = "ppml"),
    start_bundle = list(source = "lewbel_point", coef_original = zero_theta)
  )),
  list(estimator = list(
    metadata = list(estimator = "harvey"),
    point_fit = list(coef = zero_theta)
  )),
  list(estimator = list(
    metadata = list(estimator = "lad"),
    point_fit = NULL,
    start_bundle = list(source = "scale_anchor", coef_original = zero_theta)
  ))
)
check(
  "the Harvey shape resolves through point_fit",
  identical(
    logvar_resid_diag_point_coef(fake_registry, "harvey", p_cols),
    unname(zero_theta)
  )
)
check(
  "a scale-anchor bundle is refused, since it is a fit at a different b",
  inherits(
    tryCatch(
      logvar_resid_diag_point_coef(fake_registry, "lad", p_cols),
      error = function(e) e
    ),
    "error"
  )
)
check(
  "the PPML shape resolves through the Lewbel-point start bundle",
  identical(
    logvar_resid_diag_point_coef(fake_registry, "ppml", p_cols),
    unname(zero_theta)
  )
)
missing_message <- tryCatch(
  {
    logvar_resid_diag_point_coef(fake_registry, "lad", p_cols)
    ""
  },
  error = conditionMessage
)
check(
  "an unaccepted point fit stops the figure",
  grepl("no accepted tau = 0 point fit", missing_message, fixed = TRUE)
)
check(
  "an estimator absent from the registry stops the figure",
  inherits(
    tryCatch(
      logvar_resid_diag_entry(fake_registry, "logols"),
      error = function(e) e
    ),
    "error"
  )
)
check(
  "a point fit of the wrong length stops the figure",
  inherits(
    tryCatch(
      logvar_resid_diag_point_coef(fake_registry, "ppml", p_cols + 1L),
      error = function(e) e
    ),
    "error"
  )
)


.test$finish()
