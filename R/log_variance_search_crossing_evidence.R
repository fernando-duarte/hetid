lv_log_line_constraint <- function(a, b, constant, origin, direction) {
  d <- length(origin)
  left <- rep(origin, d)
  right <- rep(origin, each = d)
  v_left <- rep(direction, d)
  v_right <- rep(direction, each = d)
  list(
    a = lv_log_terms(cbind(as.vector(a), v_left, v_right)),
    b = lv_log_terms(rbind(
      cbind(as.vector(a), left, v_right, 2),
      cbind(b, direction, 1, 1)
    )),
    c = lv_log_terms(rbind(
      cbind(as.vector(a), left, right),
      cbind(b, origin, 1), c(constant, 1, 1)
    ))
  )
}

lv_log_line_residual <- function(w1, w2, origin, direction) {
  list(c = lv_log_dot(c(w1, -w2), c(1, origin)), d = lv_log_dot(-w2, direction))
}

lv_log_polynomial <- function(coefs, interval) {
  linear <- lv_log_op(lv_log_op(coefs$a, interval, "*"), coefs$b, "+")
  lv_log_op(lv_log_op(linear, interval, "*"), coefs$c, "+")
}

lv_log_separated <- function(affine_forms, interval) {
  all(vapply(affine_forms, function(residual) {
    value <- lv_log_op(residual$c, lv_log_op(residual$d, interval, "*"), "+")
    value[1L] > 0 || value[2L] < 0
  }, logical(1)))
}

lv_log_constraint_side <- function(coefs, width) {
  if (coefs$c[2L] < 0) {
    return(lv_log_polynomial(coefs, c(0, width))[2L] < 0)
  }
  if (!all(coefs$c == 0)) {
    return(FALSE)
  }
  if (coefs$b[2L] < 0) {
    remainder <- lv_log_op(rep(width, 2L), rep(max(coefs$a[2L], 0), 2L), "*")
    return(lv_log_op(coefs$b, remainder, "+")[2L] < 0)
  }
  all(coefs$b == 0) && (coefs$a[2L] < 0 || all(coefs$a == 0))
}

lv_log_boundary_approach <- function(quadratic, w1, w2, rows, origin, direction, target) {
  if (target$c[1L] != target$c[2L] || target$d[1L] != target$d[2L]) {
    return(FALSE)
  }
  root <- -target$c[1L] / target$d[1L]
  zero <- lv_log_exact(rbind(c(target$c[1L], 1), c(target$d[1L], root)))
  if (is.na(zero) || zero != 0) {
    return(FALSE)
  }
  point <- vapply(seq_along(origin), function(j) {
    lv_log_exact(rbind(c(origin[j], 1), c(root, direction[j])))
  }, numeric(1))
  if (anyNA(point)) {
    return(FALSE)
  }
  others <- setdiff(seq_along(w1), rows)
  for (side in c(-1, 1)) {
    side_direction <- side * direction
    constraints <- lapply(seq_along(quadratic$c_i), function(i) {
      lv_log_line_constraint(
        quadratic$A_i[[i]], quadratic$b_i[[i]],
        quadratic$c_i[i], point, side_direction
      )
    })
    affine_forms <- lapply(others, function(i) {
      lv_log_line_residual(w1[i], w2[i, ], point, side_direction)
    })
    if (lv_log_side_segment(constraints, affine_forms)) {
      return(TRUE)
    }
  }
  FALSE
}

lv_log_side_segment <- function(constraints, affine_forms) {
  for (k in seq_len(.Machine$double.digits)) {
    width <- 2^(1L - k)
    if (all(vapply(constraints, lv_log_constraint_side, logical(1), width = width)) &&
      lv_log_separated(affine_forms, c(0, width))) {
      return(TRUE)
    }
  }
  FALSE
}

lv_log_line_forms <- function(quadratic, w1, w2, origin, direction) {
  list(
    constraints = lapply(seq_along(quadratic$c_i), function(i) {
      lv_log_line_constraint(
        quadratic$A_i[[i]], quadratic$b_i[[i]], quadratic$c_i[i], origin, direction
      )
    }),
    residuals = lapply(seq_along(w1), function(i) {
      lv_log_line_residual(w1[i], w2[i, ], origin, direction)
    })
  )
}

lv_set_line_approach <- function(quadratic, w1, w2, rows, origin, direction,
                                 line_forms = NULL) {
  if (any(!is.finite(c(origin, direction))) || all(direction == 0)) {
    return(FALSE)
  }
  target_row <- rows[1L]
  target <- lv_log_line_residual(w1[target_row], w2[target_row, ], origin, direction)
  if (target$d[1L] <= 0 && target$d[2L] >= 0) {
    return(FALSE)
  }
  root <- lv_log_op(-rev(target$c), target$d, "/")
  if (any(!is.finite(root))) {
    return(FALSE)
  }
  others <- setdiff(seq_along(w1), rows)
  if (is.null(line_forms)) {
    constraints <- lapply(seq_along(quadratic$c_i), function(i) {
      lv_log_line_constraint(
        quadratic$A_i[[i]], quadratic$b_i[[i]], quadratic$c_i[i], origin, direction
      )
    })
    affine_forms <- lapply(others, function(i) {
      lv_log_line_residual(w1[i], w2[i, ], origin, direction)
    })
  } else {
    forms <- line_forms()
    constraints <- forms$constraints
    affine_forms <- forms$residuals[others]
  }
  if (lv_log_interior_segment(root, constraints, affine_forms)) {
    return(TRUE)
  }
  lv_log_boundary_approach(quadratic, w1, w2, rows, origin, direction, target)
}

lv_log_interior_segment <- function(root, constraints, affine_forms) {
  exponent <- min(floor(log2(max(1, abs(root)))), .Machine$double.max.exp - 1L)
  for (k in seq_len(.Machine$double.digits)) {
    width <- 2^(exponent + 1L - k)
    interval <- c(root[1L] - width, root[2L] + width)
    if (!all(is.finite(interval)) || interval[1L] >= root[1L] || interval[2L] <= root[2L]) {
      next
    }
    feasible <- all(vapply(constraints, function(coefs) {
      all(unlist(coefs) == 0) || lv_log_polynomial(coefs, interval)[2L] < 0
    }, logical(1)))
    if (feasible && lv_log_separated(affine_forms, interval)) {
      return(TRUE)
    }
  }
  FALSE
}
