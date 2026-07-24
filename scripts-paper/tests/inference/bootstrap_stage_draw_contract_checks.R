source(file.path("scripts-paper", "config", "paths.R"))
PAPER_ENDPOINT_STATUS <- c(bounded = "bounded", failed = "failed")
PAPER_INFERENCE_SEARCH_CONTROL <- list(tau_star = list(bootstrap_bisection_iterations = 4L))
PAPER_ANALYSIS_CONTRACT <- list(
  model = list(
    key_col = "when",
    preprocessing = list(return_pc = list(center = TRUE, scale = FALSE))
  )
)
LOGVAR_SEARCH_CONTROL <- list(iterations = 4L)
LOGVAR_PPML_CONTROL <- list(glm_maxit = 5L)
LOGVAR_HARVEY_CONTROL <- list(optim_maxit = 6L)
LOGVAR_NORMAL_LOG_SQUARE_GAP <- 1.25
BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY <- list(
  shared_rows = "all",
  timing = "after_shared_estimation",
  columns_role = "pc_cols",
  predicate_id = "stats::complete.cases",
  subset_roles = c("w1", "w2", "key", "pc_data")
)
paper_logvar_estimator_spec <- function(id) {
  list(dependencies = if (identical(id, "harvey")) "ppml" else character())
}
paper_normalize_model_matrix <- function(data, policy) as.matrix(data)
paper_source_once(paper_path("inference", "bootstrap_stage_specs.R"))
paper_source_once(paper_path("support", "inference", "bootstrap_stage_logvar_contract.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("support", "identification", "identified_set_bootstrap.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_core.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_draw.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_builders.R"))
paper_source_once(paper_path("inference", "bootstrap_stage_draw.R"))
coef_interval_tables_from_quadratic <- function(qs, beta1r, beta2r) {
  list(
    beta1 = data.frame(coef = names(beta1r), set_lower = 0, set_upper = 1, status = "bounded"),
    theta = data.frame(coef = rownames(beta2r), set_lower = 0, set_upper = 1, status = "bounded")
  )
}
tau_quadratic_system <- function(gamma, tau, moments) list(tau = tau)
sweep_fixed_gamma <- function(...) data.frame(tau = c(0, 0.1))
tau_star_fixed <- function(...) list(tau_star = 0.1, capped = FALSE)
logvar_engine_set_at_tau <- function(...) {
  list(schema = data.frame(
    coef = "c1", lower = 0, upper = 1,
    lower_status = "bounded", upper_status = "bounded"
  ))
}
est <- list(
  beta1r = c(b1 = 1),
  w1 = c(1, 2, 3),
  beta2r = matrix(0, 1L, 1L, dimnames = list("th1", "b1")),
  w2 = matrix(c(2, 3, 4), ncol = 1L),
  z = c(-1, 0, 1),
  moments = list(token = "moments"),
  point0 = NULL,
  tau0_quadratic = list(tau = 0)
)
stopifnot(identical(
  names(est)[seq_len(7L)],
  c("beta1r", "w1", "beta2r", "w2", "z", "moments", "point0")
))
estimate_set_id_system <- function(...) est
dat <- data.frame(when = 1:3, pc = c(4, NA, 6), row.names = c("a", "b", "c"))
geometry <- set_id_boot_geometry(est, matrix(1, 1L, 1L), c(0, 0.1), 0.1)
mean_spec <- list(
  coefs = c("b1", "th1"), gamma = matrix(1, 1L, 1L), taus = 0.1,
  tau_grid = c(0, 0.1), tau_star_iterations = 4L
)
mean_branch_spec <- list(
  coefs = mean_spec$coefs,
  tau_star_grid = mean_spec$tau_grid,
  tau_star_iterations = mean_spec$tau_star_iterations
)
mean_direct <- set_id_boot_draw_from_est(est, geometry, mean_branch_spec)
mean_wrapper <- set_id_boot_draw(dat, mean_spec)
stopifnot(identical(mean_direct, mean_wrapper))
builder <- function(...) list(token = "fit")
log_spec <- list(
  coefs = "c1", pc_cols = "pc", complete_case_policy = BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY,
  grid_cap = 2L, fit_budget = 2L, estimator_ids = c("ppml", "harvey"),
  estimator_dependencies = list(ppml = character(), harvey = "ppml"),
  response_scale = 1, logols_coef = c(c1 = 1),
  pc_preprocessing = list(center = TRUE, scale = FALSE),
  search_control = LOGVAR_SEARCH_CONTROL, ppml_control = LOGVAR_PPML_CONTROL,
  harvey_control = LOGVAR_HARVEY_CONTROL,
  normal_log_square_gap = LOGVAR_NORMAL_LOG_SQUARE_GAP,
  se_types = c(ppml = "hac", harvey = "hac"),
  taus = c(0, 0.1), key_col = "when",
  builders = list(ppml = builder, harvey = builder)
)
stage_full <- list(
  system = list(gamma = matrix(1, 1L, 1L)),
  tau = list(display = 0.1, union = c(0, 0.1)),
  mean = list(
    coefs = mean_spec$coefs,
    tau_star_grid = mean_spec$tau_grid,
    tau_star_iterations = mean_spec$tau_star_iterations
  ),
  frame = list(key_col = "when"),
  log_variance = log_spec[setdiff(
    names(log_spec),
    c("taus", "key_col", "builders")
  )]
)
eval_specs <- bootstrap_stage_eval_specs(stage_full)
logvar_harvey_estimator <- function(..., normal_log_square_gap) {
  normal_log_square_gap
}
logvar_ppml_estimator <- function(...) {
  list(
    metadata = list(spec_id = "fixture"),
    start_bundle = NULL,
    fit_at_b = NULL
  )
}
controlled_builders <- bootstrap_stage_builders(stage_full)
LOGVAR_NORMAL_LOG_SQUARE_GAP <- 101.25
controlled_gap <- controlled_builders$harvey(
  est$w1, est$w2, matrix(1, nrow = 3L), dat$when,
  NULL, list(ppml = NULL)
)
stopifnot(identical(controlled_gap, 1.25))
geometry$key_col <- "when"
volatility <- logvar_set_boot_draw_from_est(
  dat, est, geometry, eval_specs$log_variance
)
stopifnot(identical(names(volatility), c("ppml", "harvey")))
stopifnot(identical(logvar_set_boot_draw(dat, log_spec), volatility))
effects <- 0L
bad_guard <- eval_specs$log_variance
bad_guard$search_control$iterations <- 99L
bad_guard$builders$ppml <- function(...) {
  effects <<- effects + 1L
  list()
}
guard_error <- tryCatch(
  logvar_set_boot_draw_from_est(dat, est, geometry, bad_guard),
  error = function(error) error
)
stopifnot(
  identical(class(guard_error), c(
    "bootstrap_stage_search_control_error", "bootstrap_stage_error", "error", "condition"
  )), identical(
    conditionMessage(guard_error),
    "bootstrap stage search-control snapshot differs from its runtime owner"
  ),
  identical(effects, 0L)
)
ppml_failure <- eval_specs$log_variance
ppml_failure$builders$ppml <- function(...) stop("ppml fixture")
predecessor <- tryCatch(
  logvar_set_boot_draw_from_est(dat, est, geometry, ppml_failure),
  error = function(error) error
)
stopifnot(
  identical(conditionMessage(predecessor), "all(dependencies %in% names(built)) is not TRUE"),
  identical(class(predecessor), c("simpleError", "error", "condition"))
)
terminal <- eval_specs$log_variance
terminal$builders$harvey <- function(...) stop("harvey fixture")
terminal_out <- logvar_set_boot_draw_from_est(dat, est, geometry, terminal)
stopifnot(
  identical(terminal_out$ppml, volatility$ppml),
  all(terminal_out$harvey[[1L]]$lower_status == "failed"),
  all(terminal_out$harvey[[1L]]$upper_status == "failed")
)
stage_spec <- list(
  frame = list(data = dat, key_col = "when", sample_size = 3L),
  system = list(gamma = matrix(1, 1L, 1L)),
  tau = list(union = c(0, 0.1), display = 0.1),
  mean = stage_full$mean,
  log_variance = stage_full$log_variance
)
context <- list(
  index = c(1, 2, 3),
  draw_id = 1L,
  row_order = rownames(dat),
  quarter_keys = dat$when
)
deps <- list(
  estimate = function(...) est,
  geometry = function(...) geometry,
  mean = function(...) list(branch = "mean"),
  volatility = function(...) logvar_set_boot_draw_from_est(dat, est, geometry, ppml_failure)
)
joint <- bootstrap_stage_primary_draw(dat, stage_spec, context, deps)
stopifnot(
  identical(names(joint), c("mean", "volatility")),
  identical(joint$mean, list(branch = "mean")),
  identical(joint$volatility, "all(dependencies %in% names(built)) is not TRUE")
)
deps$volatility <- function(...) terminal_out
joint_terminal <- bootstrap_stage_primary_draw(dat, stage_spec, context, deps)
stopifnot(
  identical(joint_terminal$mean, list(branch = "mean")),
  identical(joint_terminal$volatility, terminal_out)
)
cat("bootstrap_stage_draw_contract_checks: PASS\n")
