# Offline checks for the base-R log-variance dynamics gate: the
# pure decision on a prescribed series (white-noise stop, persistent-DGP proceed,
# lag-4-only rule), the construction fail-closed paths (exact residual zero, no
# Lewbel point, tie-back guard), qtr-join invariance under shuffled rows, the
# full section-2.4 record shape, the descriptive sensitivity set, the always-
# present status manifest, and the executable base-R promise (no heavy-dependency
# reference in any gate file). This feature entrypoint mirrors the Harvey suite;
# run from the worktree root:
#   Rscript scripts-paper/tests/diagnostics/dynamics/test_gate.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("support", "identification", "profile_solver_core.R"))
source(paper_path("support", "identification", "profile_bounds_api.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "diagnostics", "dynamics", "gate_core.R"))

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
grepl_any <- function(x, pat) any(grepl(pat, x, fixed = TRUE))
df_named <- function(d, nm) is.data.frame(d) && identical(names(d), nm)
lag3 <- c("lag1", "lag4", "lag8")
lag8n <- sprintf("lag%d", 1:8)
na_named <- function(x) all(is.na(x)) && identical(names(x), lag3)
approx <- "approximate specification diagnostic"
gate_rds <- "log_var_eq_dynamics_gate.rds"

# a small synthetic b_N -> xi_hat system with residuals bounded away from zero,
# so construction is well defined; table_point is the tie-back target. An
# optional exact zero can be injected at one row.
build_system <- function(n = 240L, k = 2L, p = 3L, seed = 101L, zero_at = NULL) {
  set.seed(seed)
  pcr <- scale(matrix(stats::rnorm(n * p), n, p), center = TRUE, scale = FALSE)
  w2 <- matrix(stats::rnorm(n * k), n, k)
  colnames(w2) <- paste0("b", seq_len(k))
  b_point <- stats::rnorm(k)
  e_star <- sample(c(-1, 1), n, TRUE) * (1 + abs(stats::rnorm(n)))
  if (!is.null(zero_at)) e_star[zero_at] <- 0
  w1 <- drop(e_star + w2 %*% b_point)
  proj <- logvar_projection(pcr)
  inputs <- list(w1 = w1, w2 = w2, pcr = pcr, qtr = seq_len(n))
  list(
    inputs = inputs, b_point = b_point, proj = proj,
    table_point = logvar_theta_hat(b_point, w1, w2, proj)
  )
}

eval_fin <- function(sys, schema = NULL, b_point = sys$b_point) {
  logvar_gate_evaluate(
    sys$inputs, b_point, sys$proj, sys$table_point, schema, b_point,
    sample_id = "test_sample", benchmark_commit = "deadbeef"
  )
}

# The pure decision: white-noise, AR(1), and the lag-4-only rule ------------
set.seed(20260713)
d_wn <- logvar_gate_decide(stats::rnorm(240))
check("white-noise lag4 p > 0.20", d_wn$p_values[["lag4"]] > 0.20)
check("white-noise verdict non_reject", identical(d_wn$verdict, "non_reject"))
check("decide q_stats named lag1/lag4/lag8", identical(names(d_wn$q_stats), lag3))
check("decide p_values named lag1/lag4/lag8", identical(names(d_wn$p_values), lag3))
check("decide acf runs through lag 8", identical(names(d_wn$acf), lag8n))

set.seed(20260714)
xi_ar <- numeric(240)
for (t in 2:240) xi_ar[t] <- 0.8 * xi_ar[t - 1L] + stats::rnorm(1L)
d_ar <- logvar_gate_decide(xi_ar)
check("AR(1) lag4 p < 1e-6", d_ar$p_values[["lag4"]] < 1e-6)
check("AR(1) verdict reject", identical(d_ar$verdict, "reject"))

set.seed(20260014)
xi_l4 <- numeric(240)
for (t in 2:240) xi_l4[t] <- 0.12 * xi_l4[t - 1L] + stats::rnorm(1L)
d_l4 <- logvar_gate_decide(xi_l4)
check("lag-4-only: lag 1 rejects", d_l4$p_values[["lag1"]] < 0.05)
check("lag-4-only: lag 4 does not reject", d_l4$p_values[["lag4"]] >= 0.05)
check("lag-4-only: verdict non_reject (lag 4 sole gate)", identical(d_l4$verdict, "non_reject"))

# Finite-system record shape and types -------------------------------------
sys <- build_system(seed = 101L)
g <- eval_fin(sys)
req <- c(
  "schema_version", "sample_id", "benchmark_commit", "b_point", "n",
  "tested_lags", "q_stats", "p_values", "gate_lag", "gate_alpha", "verdict",
  "min_abs_eps", "crossing_status", "crossing_qtr", "max_abs_xi",
  "max_abs_xi_qtr", "acf", "xi_hat", "sensitivity", "notes"
)
check("record carries every section-2.4 field", all(req %in% names(g)))
check("verdict is reject or non_reject", g$verdict %in% c("reject", "non_reject"))
check("tested_lags identical c(1L,4L,8L)", identical(g$tested_lags, c(1L, 4L, 8L)))
check("gate_lag identical 4L integer", identical(g$gate_lag, 4L))
check("gate_alpha is 0.05", identical(g$gate_alpha, 0.05))
check("q_stats and p_values names aligned", identical(names(g$q_stats), names(g$p_values)))
check("q/p vectors named lag1/lag4/lag8", identical(names(g$q_stats), lag3))
check("xi_hat keyed by qtr", df_named(g$xi_hat, c("qtr", "xi_hat")))
check("xi_hat has one row per observation", nrow(g$xi_hat) == length(sys$inputs$w1))
check("acf field runs through lag 8", identical(names(g$acf), lag8n))
check("notes carry approx-diagnostic caveat", grepl_any(g$notes, approx))

# Exact-zero fail-closed ----------------------------------------------------
sysz <- build_system(seed = 202L, zero_at = 50L)
conz <- logvar_gate_construct(sysz$inputs, sysz$b_point, sysz$proj, sysz$table_point)
check("exact zero -> construct unreliable", identical(conz$status, "unreliable"))
check("exact zero reason recorded", identical(conz$reason, "exact residual zero"))
check("exact zero min_abs_eps == 0", identical(conz$min_abs_eps, 0))
check("exact zero crossing quarter present", length(conz$crossing_qtr) >= 1L)
gz <- eval_fin(sysz)
check("exact zero -> evaluate verdict unreliable", identical(gz$verdict, "unreliable"))
check("unreliable q_stats NA but named", na_named(gz$q_stats))
check("unreliable p_values NA", all(is.na(gz$p_values)))

# anyNA(b_point) fail-closed ------------------------------------------------
gna <- eval_fin(build_system(seed = 303L), b_point = c(NA_real_, 1))
check("no Lewbel point -> verdict unreliable", identical(gna$verdict, "unreliable"))
check("no Lewbel -> no_lewbel_point status", identical(gna$crossing_status, "no_lewbel_point"))

# Shuffled input rows give an identical result after qtr joins --------------
sys2 <- build_system(seed = 404L)
g1 <- eval_fin(sys2)
set.seed(999)
o <- sample(length(sys2$inputs$w1))
sh <- list(
  w1 = sys2$inputs$w1[o], w2 = sys2$inputs$w2[o, , drop = FALSE],
  pcr = sys2$inputs$pcr[o, , drop = FALSE], qtr = sys2$inputs$qtr[o]
)
g2 <- logvar_gate_evaluate(
  sh, sys2$b_point, logvar_projection(sh$pcr), sys2$table_point, NULL,
  sys2$b_point, "test_sample", "deadbeef"
)
check("shuffle: identical q_stats", isTRUE(all.equal(g1$q_stats, g2$q_stats)))
check("shuffle: identical p_values", isTRUE(all.equal(g1$p_values, g2$p_values)))
check("shuffle: identical verdict", identical(g1$verdict, g2$verdict))
check("shuffle: identical xi_hat by qtr", isTRUE(all.equal(g1$xi_hat, g2$xi_hat)))

# Tie-back assertion is exercised ------------------------------------------
sys3 <- build_system(seed = 505L)
tie_ok <- function(tp) {
  tryCatch(
    {
      logvar_gate_construct(sys3$inputs, sys3$b_point, sys3$proj, tp)
      TRUE
    },
    error = function(e) FALSE
  )
}
check("tie-back passes when theta_hat == table_point to 1e-10", tie_ok(sys3$table_point))
check("tie-back guard fires on a mismatched table_point", !tie_ok(sys3$table_point + 1))

# Descriptive sensitivity set ----------------------------------------------
sysS <- build_system(seed = 606L, k = 2L)
sc <- data.frame(
  coef = c("theta_0", "theta_pc1"),
  lower_status = c("bounded", "bounded"),
  upper_status = c("unbounded", "bounded"), stringsAsFactors = FALSE
)
sc$arg_lower <- I(list(sysS$b_point + c(1e-3, 0), c(NA_real_, 0)))
sc$arg_upper <- I(list(c(0, 0), sysS$b_point - c(0, 1e-3)))
sens <- logvar_gate_sensitivity(sysS$inputs, sysS$proj, list(tau_0.05 = sc), sysS$b_point)
check("sensitivity evaluates >= 1 witness row", nrow(sens$rows) >= 1L)
check("sensitivity rows carry the lag-4 p-value", "p_lag4" %in% names(sens$rows))
check("sensitivity includes the b_seed witness", "b_seed" %in% sens$rows$source)
check("sensitivity logs excluded witness", any(sens$exclusions$reason == "nonfinite_or_dim"))

# Notes wording and the status manifest ------------------------------------
nt <- logvar_gate_notes("non_reject")
check("non_reject notes: approx diagnostic", grepl_any(nt, approx))
check("non_reject notes: insufficient evidence", grepl_any(nt, "insufficient evidence"))
man <- logvar_gate_status_manifest(g)
check("manifest verdict matches the record", identical(man$gate_verdict, g$verdict))
check("manifest decision_pending is logical", is.logical(man$decision_pending))
check("manifest points at gate record path", grepl_any(man$gate_record_path, gate_rds))

# The base-R promise is executable: no heavy-dependency reference -----------
gate_files <- c(
  paper_path("log_variance", "diagnostics", "dynamics", "gate_core.R"),
  paper_path("log_variance", "diagnostics", "dynamics", "gate_record.R"),
  paper_path("log_variance", "diagnostics", "dynamics", "gate_sensitivity.R"),
  paper_path("log_variance", "diagnostics", "dynamics", "run_gate.R"),
  paper_path("log_variance", "extensions", "egarch", "cleanup.R")
)
has_dep <- any(vapply(gate_files, function(f) {
  grepl_any(readLines(f), "rugarch")
}, logical(1)))
check("no heavy-dependency reference in the gate + cleanup files", !has_dep)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
