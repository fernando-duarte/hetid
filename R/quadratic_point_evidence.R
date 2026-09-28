# Keep only the quadratic system in the membership closure's environment
quadratic_point_verifier <- function(quadratic) {
  force(quadratic)
  function(point) {
    if (!is.numeric(point) || is.complex(point) || !is.null(dim(point)) ||
      length(point) != nrow(quadratic$A_i[[1L]]) || any(!is.finite(point))) {
      return(FALSE)
    }
    quadratic_verified_point(quadratic, point)
  }
}

# Propose an interior point when the containing ellipsoid's center is outside S.
# Failure of this finite search says nothing about whether S is empty
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

# Require a strict numerical margin except for a structurally zero polynomial
quadratic_verified_point <- function(quadratic, point) {
  all(vapply(seq_along(quadratic$c_i), function(i) {
    a <- quadratic$A_i[[i]]
    b <- quadratic$b_i[[i]]
    magnitude <- max(abs(a), abs(b), abs(quadratic$c_i[i]))
    if (magnitude == 0) {
      return(TRUE)
    }
    active <- point != 0
    structural_zero <- quadratic$c_i[i] == 0 && all(b[active] == 0) &&
      all(a[active, active, drop = FALSE] == 0)
    a_scaled <- a / magnitude
    b_scaled <- b / magnitude
    constant <- quadratic$c_i[i] / magnitude
    if (any(a != 0 & a_scaled == 0) || any(b != 0 & b_scaled == 0) ||
      (quadratic$c_i[i] != 0 && constant == 0)) {
      return(FALSE)
    }
    products <- c(outer(point, point) * a_scaled, point * b_scaled, constant)
    if (any(!is.finite(products))) {
      return(FALSE)
    }
    value <- sum(products)
    error <- max(
      .Machine$double.xmin,
      HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps *
        length(point) * sum(abs(products))
    )
    value < -error || structural_zero
  }, logical(1)))
}
