# Contract-test stubs for the shared draw geometry: the minimal contract and
# control objects the draw modules read at load time, kept in their own file so
# bootstrap_stage_draw_contract_checks.R stays inside the test-file line budget.
# The endpoint vocabulary is sourced rather than stubbed, so the four-word
# closed set is the real one: a three-word stub made
# PAPER_ENDPOINT_STATUS[["unbounded"]] a subscript error, which the tau = 0 point
# guards read. Must load before the draw modules, whose defaults capture these.

paper_source_once(paper_path(
  "support", "identification", "status_contract.R"
))

PAPER_INFERENCE_SEARCH_CONTROL <- list(
  tau_star = list(bootstrap_bisection_iterations = 4L)
)
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
