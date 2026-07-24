source(file.path("scripts-paper", "config", "paths.R"))
PAPER_SERIALIZATION_CONTROL <- list(rds_version = 3L)
paper_mbb_protocol <- function() {
  list(family_names = c(
    primary = "primary",
    sensitivity = "doubled_block_sensitivity"
  ))
}
PAPER_INFERENCE_SEARCH_CONTROL <- list(
  bootstrap = list(fatal_failure_share = 0.2, progress_report_every = 2L),
  tau_star = list(bootstrap_bisection_iterations = 4L)
)
PAPER_ANALYSIS_CONTRACT <- list(
  model = list(
    key_col = "when",
    return_pc_cols = "l.pc1",
    preprocessing = list(return_pc = list(center = TRUE, scale = FALSE))
  ),
  tau = list(bootstrap_step = 0.1)
)
LOGVAR_SEARCH_CONTROL <- list(iterations = 4L)
LOGVAR_PPML_CONTROL <- list(glm_maxit = 5L)
LOGVAR_HARVEY_CONTROL <- list(optim_maxit = 6L)
LOGVAR_NORMAL_LOG_SQUARE_GAP <- 1.25
PAPER_REPORTING_CONTROL <- list(
  ppml = list(se_type = "hac"),
  harvey = list(se_type = "hac")
)
paper_mbb_block_len <- function(sample_size) 2L
paper_tau_key <- function(tau) paste0("tau_", tau)
paper_sha256_object <- function(value) paste0("hash-", length(serialize(value, NULL)))
paper_bootstrap_failure_limit <- function(n_draws, control) n_draws
paper_logvar_estimator_ids <- function(...) c("ppml", "harvey")
paper_logvar_estimator_spec <- function(id) {
  list(dependencies = if (identical(id, "harvey")) "ppml" else character())
}

paper_source_once(paper_path("inference", "bootstrap_stage_specs.R"))
stopifnot(requireNamespace("tibble", quietly = TRUE))
mean_data <- tibble::tibble(
  when = 1:3,
  y1 = c(1, 2, 3),
  x = c(2, 3, 4),
  w2 = c(3, 4, 5),
  z = c(4, 5, 6)
)
lag_pc <- tibble::tibble(when = c(1L, 3L, 4L), l.pc1 = c(9, 7, 6))
mean_eq <- list(
  data = mean_data,
  sample = list(n = 3L),
  gamma = matrix(1, 1L, 1L, dimnames = list(NULL, "w2")),
  y1_col = "y1",
  x_cols = "x",
  y2_cols = "w2",
  tau_baseline = 0.1,
  tau_display = 0.1,
  tau_cap = 0.25,
  beta1r = c("(Intercept)" = 1, x = 2),
  beta2r = matrix(1, 1L, 1L, dimnames = list("w2", "x")),
  beta1_table = data.frame(
    coef = c("(Intercept)", "x"),
    point = c(1, 2)
  ),
  theta_table = data.frame(coef = "w2", point = 3),
  set_tables = list(tau_0.1 = list(
    beta1 = data.frame(
      coef = c("(Intercept)", "x"),
      set_lower = c(0, 1),
      set_upper = c(2, 3),
      status = c("bounded", "bounded")
    ),
    theta = data.frame(
      coef = "w2",
      set_lower = 2,
      set_upper = 4,
      status = "bounded"
    )
  ))
)
log_var_eq <- list(table = data.frame(
  coef = c("(Intercept)", "l.pc1"),
  ols = c(1, 2),
  stringsAsFactors = FALSE
))
sets <- list(tau_0.1 = data.frame(
  coef = c("(Intercept)", "l.pc1"),
  set_lower = c(0, 1),
  set_upper = c(2, 3),
  status = c("bounded", "bounded")
))
point_se <- data.frame(
  coef = c("(Intercept)", "l.pc1"),
  hac = c(0.1, 0.2)
)
estimator_results <- list(
  ppml = list(
    estimator = list(metadata = list(response_scale_value = 1)),
    sets = sets,
    se = list(point = point_se)
  ),
  harvey = list(sets = sets, se = list(point = point_se))
)
spec <- bootstrap_stage_spec(
  mean_eq, log_var_eq, lag_pc, estimator_results, 2L, 77L, "z", TRUE, 5L, 6L
)
stopifnot(
  identical(names(spec), BOOTSTRAP_STAGE_ROOTS),
  identical(names(spec$frame), BOOTSTRAP_STAGE_FIELDS$frame),
  identical(names(spec$log_variance), BOOTSTRAP_STAGE_FIELDS$log_variance),
  identical(spec$frame$data$l.pc1, c(9, NA, 7)),
  identical(spec$mean$coefs, c("(Intercept)", "x", "w2")),
  identical(spec$mean$tau_star_grid, c(0, 0.1, 0.2, 0.25)),
  !any(c(
    names(spec$frame),
    names(spec$system),
    names(spec$tau),
    "taus",
    "tau_grid"
  ) %in% c(names(spec$mean), names(spec$log_variance))),
  identical(spec$tau$union, c(0, spec$tau$display)),
  identical(
    spec$design$sensitivity$block_length,
    bootstrap_stage_sensitivity_block_length(spec$design$primary$block_length)
  ),
  identical(spec$log_variance$normal_log_square_gap, LOGVAR_NORMAL_LOG_SQUARE_GAP),
  identical(
    spec$design$primary$family,
    paper_mbb_protocol()$family_names[["primary"]]
  ),
  identical(
    spec$design$sensitivity$family,
    paper_mbb_protocol()$family_names[["sensitivity"]]
  )
)
lag_pc_duplicate <- cbind(lag_pc, l.pc1_duplicate = lag_pc$l.pc1)
names(lag_pc_duplicate)[[3L]] <- "l.pc1"
duplicate_error <- try(bootstrap_stage_frame(mean_eq, lag_pc_duplicate), silent = TRUE)
stopifnot(inherits(duplicate_error, "try-error"))
PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share <- 0.3
bootstrap_stage_spec_validate(spec, bootstrap_stage_expected())
stopifnot(identical(spec$design$failure_control$fatal_failure_share, 0.2))
cat("bootstrap_stage_spec_contract_checks: PASS\n")
