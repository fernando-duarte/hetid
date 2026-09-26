# Shared full-sample and bootstrap refinement over checked feasible points.
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_evidence.R"))
paper_source_once(paper_path("support", "identification", "profile_point_pool.R"))
paper_source_once(paper_path("support", "identification", "widen_beta1_from_args.R"))

solve_theta_bound_from <- function(qs, k, direction, theta_start,
                                   box = PAPER_QUADRATIC_CONTROL$solver_boxes[[1L]],
                                   feas_tol = PAPER_QUADRATIC_CONTROL$feasibility_tolerance,
                                   evidence = NULL) {
  if (is.null(theta_start)) {
    return(NULL)
  }
  dimension <- ncol(qs$A_i[[1]])
  if (is.null(evidence)) evidence <- paper_profile_evidence(qs, diag(dimension))
  sign_mult <- if (direction == "min") 1 else -1
  objective <- numeric(dimension)
  objective[k] <- 1
  delta <- .derive_theta_scale(qs)
  result <- solve_scaled_quadratic_program(
    quadratic = qs, x0 = theta_start,
    objective = function(theta) sign_mult * sum(objective * theta),
    gradient = function(theta) sign_mult * objective,
    lower = rep(-delta * box, dimension), upper = rep(delta * box, dimension),
    method = "slsqp", objective_scale = "variable", catch_errors = FALSE
  )
  candidate <- profile_checked_candidate(evidence, result$theta)
  if (is.null(candidate)) {
    return(NULL)
  }
  list(
    bound = candidate$theta[k], theta = candidate$theta,
    contraction = candidate$contraction, movement = candidate$movement
  )
}

widen_theta_box <- function(qs, theta_tab, warm = NULL,
                            max_rounds = PAPER_QUADRATIC_CONTROL$box_multistart_rounds,
                            evidence = NULL) {
  dimension <- nrow(theta_tab)
  if (is.null(evidence)) {
    evidence <- paper_profile_evidence(qs, diag(dimension),
      points = profile_point_matrix(warm, dimension)
    )
  }
  anchors <- lapply(seq_len(nrow(evidence$feasible_points)), function(i) {
    evidence$feasible_points[i, ]
  })
  accepted <- Filter(evidence$check_point, .dedup_theta_starts(c(warm, anchors)))
  if (!is.null(evidence$strict_direction)) {
    return(list(
      tab = profile_apply_theta_tails(theta_tab, evidence),
      args = accepted, evidence = evidence
    ))
  }
  queue <- theta_box_start_pool(qs, c(warm, anchors))
  solved <- character()
  corrections <- list()
  for (round in seq_len(max_rounds)) {
    queue <- Filter(function(point) !.theta_start_key(point) %in% solved, queue)
    if (!length(queue)) break
    solved <- c(solved, vapply(queue, .theta_start_key, character(1)))
    found <- list()
    for (k in seq_len(dimension)) {
      for (side in c("min", "max")) {
        for (start in queue) {
          candidate <- solve_theta_bound_from(qs, k, side, start, evidence = evidence)
          if (!is.null(candidate)) {
            found[[length(found) + 1L]] <- candidate$theta
            corrections[[length(corrections) + 1L]] <-
              profile_correction_record(candidate, k, side, "multistart")
          }
        }
      }
    }
    accepted <- .dedup_theta_starts(c(accepted, found))
    queue <- .dedup_theta_starts(found)
  }
  if (is.null(evidence$boundedness) && length(accepted)) {
    evidence <- paper_profile_evidence(qs, evidence$objectives,
      points = profile_point_matrix(accepted, dimension),
      directions = profile_point_matrix(accepted, dimension)
    )
  }
  theta_tab <- profile_widen_theta_points(profile_apply_theta_tails(theta_tab, evidence), accepted)
  list(
    tab = theta_tab, args = accepted, evidence = evidence,
    corrections = do.call(rbind, corrections)
  )
}

coef_interval_tables_widened <- function(qs, beta1r, beta2r, points = NULL, warm = NULL) {
  dimension <- nrow(beta2r)
  evidence <- paper_profile_evidence(qs, cbind(diag(dimension), beta2r), points)
  tables <- coef_interval_tables_from_quadratic(qs, beta1r, beta2r, evidence = evidence)
  corrections <- attr(tables, "profile_corrections")
  starts <- c(warm, attr(tables, "profile_points"))
  widened <- widen_theta_box(qs, tables$theta, starts, evidence = evidence)
  statuses <- unlist(lapply(tables, function(tab) c(tab$lower_status, tab$upper_status)))
  retry <- any(statuses == PAPER_ENDPOINT_STATUS[["unreliable"]]) &&
    length(widened$args) > 0L
  if (retry) {
    # Newly checked points may repair a boundary candidate within the existing
    # displacement cap. Reuse the same geometry and endpoint acceptance rules.
    widened$evidence <- paper_profile_evidence(qs, evidence$objectives,
      points = profile_point_matrix(widened$args, dimension)
    )
  }
  if (retry || !identical(evidence$summary, widened$evidence$summary)) {
    tables <- coef_interval_tables_from_quadratic(qs, beta1r, beta2r,
      evidence = widened$evidence
    )
    corrections <- rbind(corrections, attr(tables, "profile_corrections"))
  }
  tables$theta <- profile_widen_theta_points(tables$theta, widened$args)
  tables$beta1 <- widen_beta1_from_args(tables$beta1, beta1r, beta2r, widened$args)
  attr(tables, "profile_points") <- widened$args
  attr(tables, "profile_corrections") <- rbind(
    corrections,
    widened$corrections
  )
  tables
}
