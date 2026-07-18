# Isolated transition checks for manifest-owned conditional artifacts.

local({
  original_manifest <- artifact_manifest
  root <- tempfile("paper-artifact-lifecycle-")
  dir.create(root, recursive = TRUE)
  on.exit(
    {
      artifact_manifest <<- original_manifest
      unlink(root, recursive = TRUE)
    },
    add = TRUE
  )

  test_manifest <- artifact_manifest
  conditional <- test_manifest$status %in%
    PAPER_CONDITIONAL_ARTIFACT_STATUSES
  test_manifest$new_path[conditional] <- file.path(
    root,
    test_manifest$status[conditional],
    test_manifest$basename[conditional]
  )
  assign("artifact_manifest", test_manifest, envir = .GlobalEnv)

  create_status_files <- function(status) {
    paths <- artifact_paths_by_status(status)
    invisible(vapply(paths, function(path) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      writeLines("fixture", path)
      file.exists(path)
    }, logical(1)))
  }

  audits <- lapply(
    PAPER_CONDITIONAL_ARTIFACT_STATUSES,
    function(status) {
      created <- create_status_files(status)
      audit <- cleanup_conditional_artifacts(status)
      check(
        sprintf("%s fixture files are created", status),
        all(created)
      )
      check(
        sprintf("%s cleanup removes every fixture", status),
        audit$all_absent &&
          audit$n_existed == length(created) &&
          audit$n_deleted == length(created) &&
          !any(file.exists(artifact_paths_by_status(status)))
      )
      audit
    }
  )
  names(audits) <- PAPER_CONDITIONAL_ARTIFACT_STATUSES

  check(
    "conditional cleanup refuses required artifacts",
    inherits(
      try(cleanup_conditional_artifacts("required"), silent = TRUE),
      "try-error"
    )
  )

  disabled_lad <- list(
    decision = "declined",
    source_lad = FALSE
  )
  route_state <- function(egarch_route, producer_ran = FALSE) {
    build_conditional_route_status(
      lad_gate = disabled_lad,
      egarch_route = egarch_route,
      cleanup_audits = audits,
      egarch_producer_ran = producer_ran
    )
  }
  route_status <- route_state(list(
    terminal_status = "gate_non_reject",
    run_dynamic = FALSE
  ))
  check(
    "disabled conditional routes report no present artifacts",
    !route_status$routes$conditional_lad$requested &&
      !route_status$routes$conditional_lad$ran &&
      !route_status$routes$conditional_egarch$requested &&
      !route_status$routes$conditional_egarch$ran &&
      !any(route_status$artifacts$present)
  )

  writer_calls <- character(0)
  route_writers <- lapply(
    names(route_status$routes),
    function(status) {
      force(status)
      function() writer_calls <<- c(writer_calls, status)
    }
  )
  names(route_writers) <- names(route_status$routes)
  for (status in names(route_status$routes)) {
    if (isTRUE(route_status$routes[[status]]$ran)) {
      route_writers[[status]]()
    }
  }
  check(
    "disabled conditional routes invoke no conditional writer",
    identical(writer_calls, character(0))
  )

  approved_route <- list(
    terminal_status = "approved_pending_dynamic",
    run_dynamic = TRUE
  )
  approved_pending <- route_state(approved_route)
  check(
    "approved EGARCH route is requested but remains pending",
    approved_pending$routes$conditional_egarch$requested &&
      !approved_pending$routes$conditional_egarch$ran &&
      !any(approved_pending$artifacts$present[
        approved_pending$artifacts$status == "conditional_egarch"
      ])
  )

  missing_outputs <- try(
    route_state(approved_route, producer_ran = TRUE),
    silent = TRUE
  )
  check(
    "EGARCH cannot report ran before every manifest output exists",
    inherits(missing_outputs, "try-error") &&
      grepl("manifest outputs are missing", missing_outputs, fixed = TRUE)
  )

  created_egarch <- create_status_files("conditional_egarch")
  completed_route <- route_state(
    approved_route,
    producer_ran = TRUE
  )
  check(
    "EGARCH reports ran only after its producer outputs are complete",
    all(created_egarch) &&
      completed_route$routes$conditional_egarch$requested &&
      completed_route$routes$conditional_egarch$ran &&
      all(completed_route$artifacts$present[
        completed_route$artifacts$status == "conditional_egarch"
      ])
  )
  cleanup_conditional_artifacts("conditional_egarch")

  unrequested_run <- try(
    route_state(
      list(
        terminal_status = "gate_non_reject",
        run_dynamic = FALSE
      ),
      producer_ran = TRUE
    ),
    silent = TRUE
  )
  check(
    "EGARCH producer execution requires an enabled route",
    inherits(unrequested_run, "try-error") &&
      grepl("without a producer request", unrequested_run, fixed = TRUE)
  )

  route_status_path <- file.path(root, "conditional-route-status.rds")
  written_status <- paper_write_exact_rds(
    route_status,
    route_status_path,
    "conditional-route-status-fixture"
  )
  restored_status <- readRDS(route_status_path)
  check(
    "conditional route status has an exact RDS round trip",
    identical(written_status, route_status) &&
      identical(restored_status, route_status)
  )
  check(
    "recorded conditional artifact presence matches the output tree",
    identical(
      restored_status$artifacts$present,
      unname(file.exists(restored_status$artifacts$path))
    )
  )

  for (status in PAPER_CONDITIONAL_ARTIFACT_STATUSES) {
    create_status_files(status)
    cleanup_conditional_artifacts(status)
  }
  check(
    "repeated disabled transitions cannot leave stale artifacts",
    all(vapply(
      PAPER_CONDITIONAL_ARTIFACT_STATUSES,
      function(status) {
        !any(file.exists(artifact_paths_by_status(status)))
      },
      logical(1)
    ))
  )
})
