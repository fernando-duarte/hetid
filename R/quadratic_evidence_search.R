quadratic_objective_names <- function(objectives) {
  objective_names <- colnames(objectives)
  if (is.null(objective_names)) {
    objective_names <- paste0("objective_", seq_len(ncol(objectives)))
  }
  missing_name <- is.na(objective_names) | !nzchar(objective_names)
  objective_names[missing_name] <- paste0("objective_", which(missing_name))
  make.unique(objective_names)
}

quadratic_collect_points <- function(quadratic, points, certificate, maxit) {
  center <- quadratic_certificate_center(quadratic, certificate)
  points <- rbind(points, rep(0, nrow(quadratic$A_i[[1L]])), center)
  feasible <- vapply(seq_len(nrow(points)), function(i) {
    quadratic_verified_point(quadratic, points[i, ])
  }, logical(1))
  if (!any(feasible)) {
    interior <- quadratic_find_point(quadratic, certificate, center, maxit)
    if (!is.null(interior)) {
      points <- rbind(points, interior)
      feasible <- c(feasible, TRUE)
    }
  }
  points[feasible, , drop = FALSE]
}

quadratic_strict_tail_result <- function(direction, constant) {
  list(
    lower = !constant, upper = !constant, nonempty = TRUE, strict = direction,
    tails = lapply(seq_along(constant), function(j) {
      if (constant[j]) NULL else list(list(type = "strict_curvature", direction = direction))
    })
  )
}

quadratic_collect_tails <- function(quadratic, objectives, input, points,
                                    candidate_search, n_dir, maxit) {
  count <- ncol(objectives)
  dimension <- nrow(objectives)
  constant <- colSums(objectives != 0) == 0L
  result <- list(
    lower = rep(FALSE, count), upper = rep(FALSE, count),
    nonempty = FALSE, strict = NULL, tails = vector("list", count)
  )
  if (!is.null(candidate_search$certificate)) {
    return(result)
  }
  origins <- if (nrow(points)) points else matrix(0, 1, dimension)
  directions <- quadratic_candidate_directions(quadratic, input, candidate_search, maxit)
  for (i in seq_len(nrow(directions))) {
    direction <- quadratic_normalize_direction(directions[i, ])
    if (is.null(direction)) next
    if (quadratic_strict_direction(quadratic, direction)) {
      return(quadratic_strict_tail_result(direction, constant))
    }
    for (row in seq_len(nrow(origins))) {
      line_result <- quadratic_line_evidence(quadratic, origins[row, ], direction, objectives)
      result$lower <- result$lower | line_result$lower
      result$upper <- result$upper | line_result$upper
      result$nonempty <- result$nonempty || line_result$nonempty
      result$tails <- Map(c, result$tails, line_result$evidence)
    }
  }
  quadratic_sample_tails(quadratic, result, constant, n_dir)
}

quadratic_sample_tails <- function(quadratic, result, constant, n_dir) {
  if (n_dir > 0L && !all((result$lower & result$upper) | constant)) {
    sampled <- recession_direction(quadratic, n_dir)
    if (!is.null(sampled) && quadratic_strict_direction(quadratic, sampled)) {
      return(quadratic_strict_tail_result(sampled, constant))
    }
  }
  result
}

quadratic_candidate_directions <- function(quadratic, input, candidate_search, maxit) {
  dimension <- nrow(quadratic$A_i[[1L]])
  directions <- rbind(input$directions, diag(dimension), candidate_search$directions, input$points)
  if (maxit > 0L) {
    polished <- quadratic_polish_direction(
      quadratic, candidate_search$directions[nrow(candidate_search$directions), ], maxit
    )
    if (!is.null(polished)) directions <- rbind(polished, directions)
  }
  directions
}
