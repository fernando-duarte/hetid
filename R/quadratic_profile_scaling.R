profile_theta_scale <- function(quadratic) {
  tryCatch(
    {
      spectral <- vapply(quadratic$A_i, function(a) {
        max(abs(eigen((a + t(a)) / 2, symmetric = TRUE, only.values = TRUE)$values))
      }, numeric(1))
      a_ref <- stats::median(spectral)
      c_ref <- stats::median(abs(unlist(quadratic$c_i)))
      if (!is.finite(a_ref) || a_ref <= 0 || !is.finite(c_ref) || c_ref <= 0) {
        return(1)
      }
      sqrt(c_ref / a_ref)
    },
    error = profile_numeric_error
  )
}

profile_constraint_scales <- function(quadratic, delta, control) {
  tryCatch(
    {
      magnitudes <- vapply(seq_along(quadratic$A_i), function(i) {
        a <- (quadratic$A_i[[i]] + t(quadratic$A_i[[i]])) / 2
        rho <- max(abs(eigen(a, symmetric = TRUE, only.values = TRUE)$values)) * delta^2
        bmag <- sqrt(sum((delta * quadratic$b_i[[i]])^2))
        max(rho, bmag, abs(quadratic$c_i[i]))
      }, numeric(1))
      magnitudes[!is.finite(magnitudes)] <- 0
      pos <- magnitudes[magnitudes > 0]
      floor_val <- if (length(pos)) {
        control$CONSTRAINT_SCALE_FLOOR_RTOL * stats::median(pos)
      } else {
        0
      }
      w <- pmax(magnitudes, floor_val)
      w[!is.finite(w) | w <= 0] <- 1
      w
    },
    error = profile_numeric_error
  )
}

profile_assert_symmetric <- function(quadratic, control) {
  for (matrix_i in quadratic$A_i) {
    matrix_scale <- max(1, max(abs(matrix_i)))
    if (max(abs(matrix_i - t(matrix_i))) > control$SYMMETRY_RTOL * matrix_scale) {
      stop_bad_argument("A_i must be symmetric for the analytic Jacobian",
        arg = "quadratic"
      )
    }
  }
  invisible(quadratic)
}

profile_numeric_error <- function(error) {
  if (inherits(error, "hetid_error")) stop(error)
  stop(new_hetid_error(conditionMessage(error), "hetid_error_solver", parent = error))
}

profile_scaled_bounds <- function(delta, box_width, dimension) {
  lower <- rep(-delta * box_width, dimension)
  upper <- rep(delta * box_width, dimension)
  if (!is.finite(delta) || delta <= 0 || any(!is.finite(lower)) || any(!is.finite(upper))) {
    stop(new_hetid_error(
      "Derived quadratic profile bounds exceed the numeric range",
      "hetid_error_solver"
    ))
  }
  list(lower = lower, upper = upper)
}
