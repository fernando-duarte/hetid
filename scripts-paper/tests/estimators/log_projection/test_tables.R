# Offline rendering checks for the regularized log-projection exhibits: the
# tuning appendix table's shape, headers, and coefficient alignment, the
# no-inference Panel B, and the notes builders. Run from the package root:
#   Rscript scripts-paper/tests/estimators/log_projection/test_tables.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path(
  "log_variance", "tables", "render_log_projection_tuning_table.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

tt_coef <- c("(Intercept)", "l.pc1")
tt_multipliers <- c(0.5, 1, 2)
tt_tuning <- do.call(rbind, lapply(c("log_plus", "log_fuller"), function(id) {
  do.call(rbind, lapply(tt_multipliers, function(m) {
    data.frame(
      id = id, multiplier = m, coef = rev(tt_coef),
      point = if (m == 2 && id == "log_plus") c(NA, NA) else c(0.1, -1.2),
      set_lower = c(0.05, -1.3), set_upper = c(0.15, -1.1),
      status = if (m == 0.5 && id == "log_fuller") "unreliable" else "bounded",
      stringsAsFactors = FALSE
    )
  }))
}))
tt_specs <- list(
  log_plus = LOGVAR_LOG_PLUS_PANEL_SPEC,
  log_fuller = LOGVAR_LOG_FULLER_PANEL_SPEC
)
tt_lines <- logvar_log_projection_tuning_lines(
  tt_tuning, tt_specs, tt_coef, tt_multipliers, 0.05
)
tt_body <- tt_lines[grepl("^\\$\\\\theta", tt_lines)]
check(
  "tuning table has one row per coefficient per method and six data columns",
  length(tt_body) == 2L * length(tt_coef) &&
    all(lengths(gregexpr(" & ", tt_body, fixed = TRUE)) == 6L)
)
check(
  "tuning table aligns rows by coefficient, not by input order",
  grepl("-1.2", tt_body[[1L]], fixed = TRUE) &&
    grepl("0.1", tt_body[[2L]], fixed = TRUE)
)
check(
  "tuning table headers use the tau labels of the estimator pages",
  any(grepl("$\\tau{=}0$", tt_lines, fixed = TRUE)) &&
    any(grepl("$\\tau{=}0.05$", tt_lines, fixed = TRUE))
)
check(
  "a missing point renders the NA token and an unreliable set its status",
  any(grepl(PAPER_NA_TOKEN, tt_body, fixed = TRUE)) &&
    any(grepl("unreliable", tt_body, fixed = TRUE))
)

tt_set <- data.frame(
  coef = tt_coef, set_lower = c(-1.3, 0.05), set_upper = c(-1.1, 0.15),
  status = "bounded", stringsAsFactors = FALSE
)
tt_result <- list(
  table = data.frame(
    coef = tt_coef, reference = c(-1.2, 0.1), point = c(-1.25, 0.12),
    stringsAsFactors = FALSE
  ),
  sets = stats::setNames(list(tt_set), paper_tau_key(0.05))
)
tt_parts <- logvar_estimator_panel_parts(
  tt_result, 12L, 0.05, LOGVAR_LOG_PLUS_PANEL_SPEC, NULL, NULL, NULL,
  PAPER_REPORTING_CONTROL$cells$log_variance, NULL
)
tt_cells <- unlist(tt_parts$columns)
check(
  "the regularized Panel B carries no statistics, stars, or envelopes",
  !any(grepl("*", tt_cells, fixed = TRUE)) &&
    !any(grepl("^\\(", tt_cells)) &&
    identical(
      tt_parts$rows[nzchar(tt_parts$rows)][1:2],
      c("$\\theta^{L}_0$", "$\\theta^{L}_{1,R}$")
    )
)

tt_endpoints <- data.frame(
  id = c("log_plus", "log_fuller", "log_plus"), multiplier = 1,
  tau = c(NA, NA, 0.05), role = c("point", "point", "endpoint"),
  h_T = c(0.01, NA, 0.01), c_T = c(NA, 0.004, NA),
  share_small = c(0.02, 0.03, 0.05),
  profile_log_ratio_min = c(NA, -0.4, NA), profile_log_ratio_max = c(NA, 0.6, NA),
  stringsAsFactors = FALSE
)
tt_lp_result <- function(method, certified) {
  list(
    estimator = list(metadata = list(fit_control = list(method = method))),
    endpoints = tt_endpoints[tt_endpoints$id == method, , drop = FALSE],
    multiplier = 1,
    scale = list(
      n_mean = 256L, n_vol = 255L, s_hat = 0.6, scale_lower_certified = certified
    )
  )
}
tt_plus_notes <- paste(
  build_log_projection_panel_notes(tt_lp_result("log_plus", TRUE), 0.05),
  collapse = " "
)
tt_fuller_notes <- paste(
  build_log_projection_panel_notes(tt_lp_result("log_fuller", FALSE), 0.05),
  collapse = " "
)
check(
  "panel notes explain the OLS column, the tuning, and the missing inference",
  grepl("OLS column", tt_plus_notes, fixed = TRUE) &&
    grepl("h_T = 0.01", tt_plus_notes, fixed = TRUE) &&
    grepl("No inference is reported for Panel B", tt_plus_notes, fixed = TRUE) &&
    grepl("different candidates", tt_plus_notes, fixed = TRUE)
)
check(
  "Fuller notes say uncertified when the scale bound is not certified",
  grepl("uncertified", tt_fuller_notes, fixed = TRUE) &&
    grepl("c_T = 0.004", tt_fuller_notes, fixed = TRUE)
)
tt_tuning_notes <- paste(
  build_log_projection_tuning_notes(tt_tuning, 0.05, tt_endpoints),
  collapse = " "
)
check(
  "tuning notes give both formulas and the endpoint CSV",
  grepl("c_T = m^2 / T", tt_tuning_notes, fixed = TRUE) &&
    grepl("quadruples", tt_tuning_notes, fixed = TRUE) &&
    grepl("log\\_var\\_eq\\_log\\_projection\\_endpoints.csv", tt_tuning_notes,
      fixed = TRUE
    )
)

.test$finish()
