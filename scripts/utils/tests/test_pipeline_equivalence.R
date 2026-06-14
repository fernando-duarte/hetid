# Numeric-invariance gate for routing the pipeline through the generalized
# instrument API. Proves, locally and by construction, that the migrated
# assembly path equals the legacy one on every output the pipeline consumes:
#   - build_pipeline_quadratic_system() == build_quadratic_system() on the
#     solver-consumable $quadratic and on the L_i/V_i/Q_i component values;
#   - the re-attached $components is a valid hetid_components (class + maturity
#     identity) so persisted RDS objects and the hetid quadratic-stage APIs stay
#     compatible;
#   - solve_point_identification() (tau = 0) returns identical theta/cond;
#   - the default Z front door build_instrument_matrix(z, transforms = NULL,
#     include_original = TRUE) returns the named PC matrix unchanged.
# Run from package root:
#   Rscript scripts/utils/tests/test_pipeline_equivalence.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")

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

# --- legacy-vs-general equivalence on representative (gamma, tau) -------------
# Cover the gamma shapes the pipeline actually uses: the VFCI unit-norm loading
# (stage 04/05/05c), a reduced-form-style dense gamma (stage 05b/06), and a
# random dense gamma; with tau = 0 (point ID), a homogeneous set-ID slack, and a
# heterogeneous per-component slack vector.
set.seed(11)
t_obs <- 180L
n_comp <- 3L
n_pcs <- 4L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * n_comp), nrow = t_obs, ncol = n_comp)
pcs <- matrix(rnorm(t_obs * n_pcs), nrow = t_obs, ncol = n_pcs)
colnames(pcs) <- paste0("pc", seq_len(n_pcs))
moments <- suppressMessages(compute_identification_moments(w1, w2, pcs))

gamma_vfci <- get_baseline_gamma("vfci", n_pcs = n_pcs, n_components = n_comp)
gamma_rf <- matrix(rnorm(n_pcs * n_comp), nrow = n_pcs, ncol = n_comp)
gamma_dense <- matrix(runif(n_pcs * n_comp, -1, 1), nrow = n_pcs, ncol = n_comp)

tau_cases <- list(
  point = rep(0, n_comp),
  set_homogeneous = rep(0.05, n_comp),
  set_heterogeneous = c(0.02, 0.1, 0.2)
)
gamma_cases <- list(vfci = gamma_vfci, reduced_form = gamma_rf, dense = gamma_dense)

for (gname in names(gamma_cases)) {
  for (tname in names(tau_cases)) {
    g <- gamma_cases[[gname]]
    tau <- tau_cases[[tname]]
    general <- build_pipeline_quadratic_system(g, tau, moments)
    legacy <- suppressMessages(build_quadratic_system(g, tau, moments))
    lab <- sprintf("%s / tau=%s", gname, tname)

    check(
      paste0("$quadratic identical -- ", lab),
      identical(general$quadratic, legacy$quadratic)
    )
    check(
      paste0("component leaves identical (L_i/V_i/Q_i) -- ", lab),
      identical(general$components$L_i, legacy$components$L_i) &&
        identical(general$components$V_i, legacy$components$V_i) &&
        identical(general$components$Q_i, legacy$components$Q_i)
    )
    check(
      paste0("$components is a valid hetid_components -- ", lab),
      inherits(general$components, "hetid_components") &&
        identical(
          attr(general$components, "maturities"),
          attr(legacy$components, "maturities")
        ) &&
        identical(
          attr(general$components, "n_components"),
          attr(legacy$components, "n_components")
        )
    )
    check(
      paste0("$components fully identical to legacy -- ", lab),
      identical(general$components, legacy$components)
    )
  }
}

# --- tau = 0 point identification is bit-identical ----------------------------
for (gname in names(gamma_cases)) {
  g <- gamma_cases[[gname]]
  general <- build_pipeline_quadratic_system(g, rep(0, n_comp), moments)
  legacy <- suppressMessages(build_quadratic_system(g, rep(0, n_comp), moments))
  pt_general <- solve_point_identification(general$components)
  pt_legacy <- solve_point_identification(legacy$components)
  check(
    paste0("solve_point_identification identical at tau=0 -- ", gname),
    identical(pt_general, pt_legacy)
  )
}

# --- default Z front door is a no-op on the named PC matrix --------------------
z_front <- build_instrument_matrix(pcs, transforms = NULL, include_original = TRUE)
check(
  "build_instrument_matrix(pcs, transforms=NULL) == pcs (values + names + type)",
  identical(z_front, pcs)
)

# --- the HETID_ASSERT_EQUIV shadow assertion accepts the migrated path --------
.prev_equiv <- Sys.getenv("HETID_ASSERT_EQUIV", unset = NA_character_)
Sys.setenv(HETID_ASSERT_EQUIV = "1")
ok <- tryCatch(
  {
    build_pipeline_quadratic_system(gamma_vfci, rep(0.05, n_comp), moments)
    TRUE
  },
  error = function(e) FALSE
)
check("HETID_ASSERT_EQUIV shadow assertion passes on real inputs", ok)
if (is.na(.prev_equiv)) {
  Sys.unsetenv("HETID_ASSERT_EQUIV")
} else {
  Sys.setenv(HETID_ASSERT_EQUIV = .prev_equiv)
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
