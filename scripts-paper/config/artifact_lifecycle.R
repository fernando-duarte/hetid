# Manifest-driven cleanup and absence checks for conditional artifacts.

paper_source_once(
  paper_path("support", "artifacts", "typed_artifacts.R")
)
paper_source_once(paper_path("config", "logvar_estimators.R"))

PAPER_CONDITIONAL_ROUTE_SCHEMA_VERSION <- "1.1.0"

cleanup_conditional_artifacts <- function(status) {
  if (!status %in% PAPER_CONDITIONAL_ARTIFACT_STATUSES) {
    stop(
      sprintf("Refusing to clean nonconditional status: %s", status),
      call. = FALSE
    )
  }
  records <- artifact_records_by_status(status)
  paths <- records$new_path
  existed <- file.exists(paths)
  unlink(paths)
  gone <- !file.exists(paths)
  if (!all(gone)) {
    stop(
      sprintf("Could not remove %s artifacts", status),
      call. = FALSE
    )
  }
  list(
    status = status,
    artifacts = data.frame(
      id = records$id,
      path = paths,
      existed = existed,
      deleted = existed & gone,
      stringsAsFactors = FALSE
    ),
    n_existed = sum(existed),
    n_deleted = sum(existed & gone),
    all_absent = all(gone)
  )
}

assert_artifacts_absent <- function(status) {
  paths <- artifact_paths_by_status(status)
  if (any(file.exists(paths))) {
    stop(
      sprintf("Stale %s artifacts remain", status),
      call. = FALSE
    )
  }
  invisible(paths)
}

# Router permission and producer execution are separate states. A producer may
# mark itself as run only after every artifact owned by its manifest status exists.
build_conditional_route_status <- function(
  lad_gate,
  egarch_route,
  cleanup_audits,
  egarch_producer_ran = FALSE
) {
  conditional <- artifact_manifest[
    artifact_manifest$status %in%
      PAPER_CONDITIONAL_ARTIFACT_STATUSES, ,
    drop = FALSE
  ]
  routes <- stats::setNames(
    list(
      list(
        decision = lad_gate$decision,
        requested = isTRUE(lad_gate$source_lad),
        ran = paper_logvar_result_exists("lad")
      ),
      list(
        decision = egarch_route$terminal_status,
        requested = isTRUE(egarch_route$run_dynamic),
        ran = isTRUE(egarch_producer_ran)
      )
    ),
    c(
      PAPER_ARTIFACT_STATUS$conditional_lad,
      PAPER_ARTIFACT_STATUS$conditional_egarch
    )
  )
  stopifnot(setequal(
    names(routes),
    PAPER_CONDITIONAL_ARTIFACT_STATUSES
  ))
  artifacts <- data.frame(
    id = conditional$id,
    status = conditional$status,
    path = conditional$new_path,
    present = file.exists(conditional$new_path),
    stringsAsFactors = FALSE
  )
  for (status in names(routes)) {
    present <- artifacts$present[
      artifacts$status == status
    ]
    route <- routes[[status]]
    if (isTRUE(route$ran) && !isTRUE(route$requested)) {
      stop(
        sprintf(
          "Conditional route %s ran without a producer request",
          status
        ),
        call. = FALSE
      )
    }
    if (isTRUE(route$ran) && !all(present)) {
      stop(
        sprintf(
          "Conditional route %s ran but manifest outputs are missing",
          status
        ),
        call. = FALSE
      )
    }
    if (!isTRUE(route$ran) && any(present)) {
      stop(
        sprintf(
          "Conditional route %s did not run but artifacts exist",
          status
        ),
        call. = FALSE
      )
    }
  }
  list(
    schema_version = PAPER_CONDITIONAL_ROUTE_SCHEMA_VERSION,
    routes = routes,
    cleanup = cleanup_audits,
    artifacts = artifacts
  )
}
