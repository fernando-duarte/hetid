# Numerical certificate search. Optimizer output is only a candidate
quadratic_curvature_margin <- function(a, direction) {
  products <- outer(direction, direction) * a
  value <- sum(products)
  error <- max(
    .Machine$double.xmin,
    HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps * nrow(a) * sum(abs(products))
  )
  c(value = value, error = error)
}

quadratic_weighted_matrix <- function(matrices, combination_weights) {
  Reduce(`+`, Map(`*`, matrices, combination_weights))
}

quadratic_pd_candidate <- function(matrices, combination_weights) {
  if (any(!is.finite(combination_weights)) || any(combination_weights < 0) ||
    sum(combination_weights) <= 0) {
    return(NULL)
  }
  combination_weights <- combination_weights / sum(combination_weights)
  combined <- quadratic_weighted_matrix(matrices, combination_weights)
  eig <- eigen(combined, symmetric = TRUE)
  error <- HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps * nrow(combined) *
    sum(vapply(Map(`*`, matrices, combination_weights), function(a) sum(abs(a)), 0))
  if (min(eig$values) <= error) {
    return(NULL)
  }
  list(
    type = "positive_definite_combination", weights = combination_weights,
    matrix = combined, eigenvalues = eig$values, error = error
  )
}

quadratic_boundedness_search <- function(quadratic,
                                         maxit = HETID_CONSTANTS$QUADRATIC_EVIDENCE_MAXIT) {
  scales <- vapply(quadratic$A_i, function(a) max(abs(a)), 0)
  scales[scales == 0] <- 1
  matrices <- Map(`/`, quadratic$A_i, scales)
  count <- length(matrices)
  combination_weights <- rep(1 / count, count)
  certificate <- quadratic_pd_candidate(matrices, combination_weights)
  if (is.null(certificate)) {
    for (i in seq_len(count)) {
      w <- numeric(count)
      w[i] <- 1
      certificate <- quadratic_pd_candidate(matrices, w)
      if (!is.null(certificate)) break
    }
  }
  if (is.null(certificate) && count > 1 && maxit > 0) {
    objective <- function(w) {
      candidate <- quadratic_weighted_matrix(matrices, c(w, 1 - sum(w)))
      -min(eigen(candidate, symmetric = TRUE, only.values = TRUE)$values)
    }
    if (count == 2L) {
      parameter <- stats::optimize(objective, c(0, 1),
        tol = HETID_CONSTANTS$QUADRATIC_SEARCH_RTOL
      )$minimum
    } else {
      candidate_search <- stats::constrOptim(rep(1 / count, count - 1), objective, NULL,
        ui = rbind(diag(count - 1), rep(-1, count - 1)),
        ci = c(rep(0, count - 1), -1),
        control = list(maxit = maxit, reltol = HETID_CONSTANTS$QUADRATIC_SEARCH_RTOL)
      )
      parameter <- candidate_search$par
    }
    combination_weights <- c(parameter, 1 - sum(parameter))
    certificate <- quadratic_pd_candidate(matrices, combination_weights)
  }
  if (!is.null(certificate)) certificate$scales <- scales
  combined <- quadratic_weighted_matrix(matrices, combination_weights)
  directions <- t(eigen(combined, symmetric = TRUE)$vectors)
  list(certificate = certificate, directions = directions, matrices = matrices)
}

# Candidate center of the containing ellipsoid; feasibility is checked separately
quadratic_certificate_center <- function(quadratic, certificate) {
  if (is.null(certificate)) {
    return(NULL)
  }
  combination_weights <- certificate$weights / certificate$scales
  linear <- Reduce(`+`, Map(`*`, quadratic$b_i, combination_weights))
  if (any(!is.finite(linear))) {
    return(NULL)
  }
  eig <- eigen(certificate$matrix, symmetric = TRUE)
  center <- drop(eig$vectors %*% (crossprod(eig$vectors, -linear / 2) / eig$values))
  if (all(is.finite(center))) center else NULL
}
