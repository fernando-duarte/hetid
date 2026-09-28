# Validate a direction independently of how it was proposed
quadratic_strict_direction <- function(quadratic, direction) {
  all(vapply(quadratic$A_i, function(a) {
    magnitude <- max(abs(a))
    if (magnitude == 0) {
      return(FALSE)
    }
    margin <- quadratic_curvature_margin(a / magnitude, direction)
    is.finite(margin["value"]) && margin["value"] < -margin["error"]
  }, logical(1)))
}

quadratic_normalize_direction <- function(direction) {
  magnitude <- max(abs(direction))
  if (magnitude == 0) {
    return(NULL)
  }
  direction <- direction / magnitude
  direction / sqrt(sum(direction^2))
}

quadratic_polish_direction <- function(quadratic, direction, maxit) {
  if (maxit == 0L) {
    return(NULL)
  }
  if (length(direction) == 1L) {
    return(if (quadratic_strict_direction(quadratic, 1)) 1 else NULL)
  }
  matrices <- lapply(quadratic$A_i, function(a) {
    magnitude <- max(abs(a))
    if (magnitude == 0) a else a / magnitude
  })
  objective <- function(candidate) {
    candidate <- quadratic_normalize_direction(candidate)
    if (is.null(candidate) || any(!is.finite(candidate))) {
      return(.Machine$double.xmax)
    }
    max(vapply(matrices, function(a) sum(candidate * (a %*% candidate)), 0))
  }
  result <- stats::optim(direction, objective,
    control = list(maxit = maxit, reltol = HETID_CONSTANTS$QUADRATIC_SEARCH_RTOL)
  )
  candidate <- quadratic_normalize_direction(result$par)
  if (!is.null(candidate) && quadratic_strict_direction(quadratic, candidate)) {
    candidate
  } else {
    NULL
  }
}

# A line tail is accepted only when its leading nonzero sign is resolved
quadratic_verify_line_tail <- function(quadratic, origin, direction, side) {
  all(vapply(seq_along(quadratic$c_i), function(i) {
    quadratic_constraint_tail(
      quadratic$A_i[[i]], quadratic$b_i[[i]], quadratic$c_i[i], origin, direction, side
    )
  }, logical(1)))
}

quadratic_constraint_tail <- function(a, b, constant, origin, direction, side) {
  magnitude <- max(abs(a), abs(b), abs(constant))
  if (magnitude == 0) {
    return(TRUE)
  }
  aa <- a / magnitude
  bb <- b / magnitude
  cc <- constant / magnitude
  if (any(a != 0 & aa == 0) || any(b != 0 & bb == 0) ||
    (constant != 0 && cc == 0)) {
    return(FALSE)
  }
  curvature <- quadratic_curvature_margin(aa, direction)
  if (curvature["value"] < -curvature["error"]) {
    return(TRUE)
  }
  if (curvature["value"] > curvature["error"]) {
    return(FALSE)
  }
  active <- direction != 0
  if (!all(a[active, active, drop = FALSE] == 0)) {
    return(FALSE)
  }
  products <- c(2 * outer(direction, origin) * aa, direction * bb)
  if (any(!is.finite(products))) {
    return(FALSE)
  }
  quadratic_linear_tail(a, b, constant, products, origin, direction, side)
}

quadratic_linear_tail <- function(a, b, constant, products, origin, direction, side) {
  linear <- sum(products) * side
  error <- max(
    .Machine$double.xmin,
    HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps *
      length(direction) * sum(abs(products))
  )
  if (linear < -error) {
    return(TRUE)
  }
  if (linear > error) {
    return(FALSE)
  }
  active <- direction != 0
  structural_zero <- all(b[active] == 0) &&
    all(a[active, origin != 0, drop = FALSE] == 0)
  if (!structural_zero) {
    return(FALSE)
  }
  one <- list(A_i = list(a), b_i = list(b), c_i = constant)
  quadratic_verified_point(one, origin)
}

quadratic_line_evidence <- function(quadratic, origin, direction, objectives) {
  hull <- line_feasible_hull(origin, direction, quadratic)
  count <- ncol(objectives)
  lower <- upper <- rep(FALSE, count)
  evidence <- vector("list", count)
  nonempty <- FALSE
  if (is.null(hull)) {
    return(list(
      lower = lower, upper = upper, evidence = evidence, nonempty = nonempty
    ))
  }
  projection <- vapply(seq_len(count), function(j) {
    objective <- objectives[, j]
    magnitude <- max(abs(objective))
    if (magnitude == 0) {
      return(0)
    }
    products <- (objective / magnitude) * direction
    value <- sum(products)
    error <- max(
      .Machine$double.xmin,
      HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps *
        length(direction) * sum(abs(products))
    )
    if (abs(value) <= error) 0 else sign(value)
  }, 0)
  for (side in c(-1, 1)) {
    endpoint <- if (side < 0) hull[1] else hull[2]
    if (!is.infinite(endpoint) ||
      !quadratic_verify_line_tail(quadratic, origin, direction, side)) {
      next
    }
    nonempty <- TRUE
    lower <- lower | side * projection < 0
    upper <- upper | side * projection > 0
    for (j in which(projection != 0)) {
      evidence[[j]] <- c(evidence[[j]], list(list(
        type = "line_tail", origin = origin, direction = direction, side = side
      )))
    }
  }
  list(lower = lower, upper = upper, evidence = evidence, nonempty = nonempty)
}
