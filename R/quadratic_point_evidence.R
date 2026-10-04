# Keep only the point-independent constraint terms in the membership closure's
# environment: the scaling is computed once, not on every call
quadratic_point_verifier <- function(quadratic) {
  dimension <- nrow(quadratic$A_i[[1L]])
  constraint_terms <- quadratic_point_terms(quadratic)
  function(point) {
    if (!is.numeric(point) || is.complex(point) || !is.null(dim(point)) ||
      length(point) != dimension || any(!is.finite(point))) {
      return(FALSE)
    }
    quadratic_terms_verify(constraint_terms, point)
  }
}

quadratic_find_point <- function(quadratic, certificate, center, maxit) {
  if (is.null(certificate) || is.null(center) || maxit == 0L) {
    return(NULL)
  }
  if (length(center) == 1L) {
    return(NULL)
  }
  scales <- vapply(seq_along(quadratic$c_i), function(i) {
    max(abs(quadratic$A_i[[i]]), abs(quadratic$b_i[[i]]), abs(quadratic$c_i[i]))
  }, 0)
  scales[scales == 0] <- 1
  objective <- function(point) {
    values <- vapply(seq_along(scales), function(i) {
      sum(outer(point, point) * (quadratic$A_i[[i]] / scales[i])) +
        sum(point * (quadratic$b_i[[i]] / scales[i])) + quadratic$c_i[i] / scales[i]
    }, 0)
    if (any(!is.finite(values))) .Machine$double.xmax else max(values)
  }
  result <- stats::optim(center, objective,
    control = list(maxit = maxit, reltol = HETID_CONSTANTS$QUADRATIC_SEARCH_RTOL)
  )
  if (quadratic_verified_point(quadratic, result$par)) result$par else NULL
}

# Point-independent pieces of each constraint: its max-abs scaling and whether
# that scaling loses a nonzero coefficient; NULL for an all-zero constraint
quadratic_point_terms <- function(quadratic) {
  lapply(seq_along(quadratic$c_i), function(i) {
    a <- quadratic$A_i[[i]]
    b <- quadratic$b_i[[i]]
    constant <- quadratic$c_i[i]
    magnitude <- max(abs(a), abs(b), abs(constant))
    if (magnitude == 0) {
      return(NULL)
    }
    a_scaled <- a / magnitude
    b_scaled <- b / magnitude
    c_scaled <- constant / magnitude
    list(
      a = a, b = b, zero_constant = constant == 0,
      a_scaled = a_scaled, b_scaled = b_scaled, c_scaled = c_scaled,
      lost = any(a != 0 & a_scaled == 0) || any(b != 0 & b_scaled == 0) ||
        (constant != 0 && c_scaled == 0)
    )
  })
}

quadratic_verified_point <- function(quadratic, point) {
  quadratic_terms_verify(quadratic_point_terms(quadratic), point)
}

quadratic_terms_verify <- function(constraint_terms, point) {
  square <- outer(point, point)
  for (term in constraint_terms) {
    if (!is.null(term) && !quadratic_term_holds(term, point, square)) {
      return(FALSE)
    }
  }
  TRUE
}

# Accept zero only when coefficients or point coordinates make every term zero
quadratic_term_holds <- function(term, point, square) {
  if (term$lost) {
    return(FALSE)
  }
  products <- c(square * term$a_scaled, point * term$b_scaled, term$c_scaled)
  if (any(!is.finite(products))) {
    return(FALSE)
  }
  error <- quadratic_sign_error(products, length(point))
  if (sum(products) < -error) {
    return(TRUE)
  }
  active <- point != 0
  term$zero_constant && all(term$b[active] == 0) &&
    all(term$a[active, active, drop = FALSE] == 0)
}
