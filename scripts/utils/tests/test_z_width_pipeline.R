# Custom-width pipeline integration fixture: runs the rewired stage chain
# in-process under HETID_Z_SOURCE / HETID_BASELINE_GAMMA so nothing under
# scripts/output/ is written. Needs scripts/output/temp/data.rds (stage 01);
# prints a loud SKIP and exits 0 when absent so the sweep stays hermetic
# elsewhere. Run from the package root:
#   Rscript scripts/utils/tests/test_z_width_pipeline.R
source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/ixj_identification.R"))

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
  cat("SKIPPED: scripts/output/temp/data.rds missing (run stage 01 first)\n")
  quit(status = 0L)
}

z_file <- tempfile(fileext = ".R")
writeLines(c(
  "build_z <- function(data) {",
  "  z <- cbind(",
  "    as.matrix(data[, paste0(\"pc\", 1:4)]),",
  "    pc1_sq = data$pc1^2",
  "  )",
  "  z",
  "}"
), z_file)
gamma5_file <- tempfile(fileext = ".R")
writeLines(c(
  "build_gamma <- function(moments) {",
  "  set.seed(7)",
  "  j <- nrow(moments$r_i_0)",
  "  i <- attr(moments, \"n_components\")",
  "  matrix(rnorm(j * i), nrow = j, ncol = i)",
  "}"
), gamma5_file)

old_z <- Sys.getenv("HETID_Z_SOURCE", unset = NA)
Sys.setenv(HETID_Z_SOURCE = z_file)

inputs <- load_identification_inputs(mode = "maturities")
resid <- suppressMessages(
  compute_identification_residuals(inputs$data, mode = "maturities")
)
check(
  "hook width propagates into the aligned instrument matrix",
  ncol(resid$pcs_aligned) == 5L &&
    identical(colnames(resid$pcs_aligned)[5], "pc1_sq")
)

mom <- suppressMessages(
  compute_identification_moments(resid$w1, resid$w2, resid$pcs_aligned)
)
check(
  "moments container carries J = 5 with the hook's instrument names",
  nrow(mom$r_i_0) == 5L &&
    identical(rownames(mom$r_i_0), colnames(resid$pcs_aligned))
)

vfci_pipe_err <- tryCatch(
  {
    resolve_baseline_gamma("vfci", mom, resid$gamma_rf)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "vfci baseline refuses the five-instrument container instructively",
  is.character(vfci_pipe_err) && grepl("HETID_BASELINE_GAMMA", vfci_pipe_err)
)

gamma5 <- resolve_baseline_gamma(gamma5_file, mom, resid$gamma_rf)
check(
  "gamma hook is honored at the custom width",
  identical(dim(gamma5), c(5L, 3L)) && grepl("^custom:", attr(gamma5, "method"))
)

tau_specs <- get_tau_spec(tau_point = 0, tau_set = BASELINE_TAU, n_components = 3L)
qs <- suppressMessages(build_quadratic_system(gamma5, tau_specs$tau_set, mom))
bounds <- solve_all_profile_bounds(qs$quadratic)
check(
  "profile bounds at J = 5 carry the honest bounded/valid certificates",
  is.data.frame(bounds) &&
    all(c("bounded_lower", "bounded_upper", "valid_lower", "valid_upper") %in% names(bounds)) &&
    nrow(bounds) == 3L &&
    is.character(format_bound(bounds$lower, bounds$valid_lower))
)

ixj <- build_ixj_quadratic_system(mom, matrix(0.2, nrow = 5L, ncol = 3L))
check(
  "separate-instrument system derives J from the moments (fifteen constraints)",
  length(ixj$quadratic$A_i) == 15L && setequal(ixj$labels$instrument, 1:5)
)

opt <- suppressMessages(run_gamma_optimization(
  gamma5, mom, rep(BASELINE_TAU, 3L),
  n_starts = 2, seed = SEED, maxeval = 30L
))
check(
  "gamma optimizer runs at the custom width with an honest objective",
  identical(dim(opt$gamma_optimized), c(5L, 3L)) &&
    (is.infinite(opt$objective_final) || opt$objective_final < 1e12)
)

# Narrow width too: two instruments must flow through the same chain
z2_file <- tempfile(fileext = ".R")
writeLines(c(
  "build_z <- function(data) {",
  "  as.matrix(data[, c(\"pc1\", \"pc2\")])",
  "}"
), z2_file)
Sys.setenv(HETID_Z_SOURCE = z2_file)
resid2 <- suppressMessages(
  compute_identification_residuals(inputs$data, mode = "maturities")
)
mom2 <- suppressMessages(
  compute_identification_moments(resid2$w1, resid2$w2, resid2$pcs_aligned)
)
gamma2_file <- tempfile(fileext = ".R")
writeLines(
  "build_gamma <- function(m) matrix(1, nrow(m$r_i_0), attr(m, \"n_components\"))",
  gamma2_file
)
gamma2 <- resolve_baseline_gamma(gamma2_file, mom2, resid2$gamma_rf)
qs2 <- suppressMessages(build_quadratic_system(gamma2, tau_specs$tau_set, mom2))
check(
  "a narrower-than-default Z flows through residuals, moments, gamma, and the system",
  nrow(mom2$r_i_0) == 2L && identical(dim(gamma2), c(2L, 3L)) &&
    length(qs2$quadratic$A_i) == 3L
)

if (is.na(old_z)) Sys.unsetenv("HETID_Z_SOURCE") else Sys.setenv(HETID_Z_SOURCE = old_z)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
