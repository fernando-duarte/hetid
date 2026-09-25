# Reuse constraint-relative rounding checks for bounds and candidate membership.
make_relative_feasibility_checker <- function(quadratic) {
  checker <- make_system_checker(quadratic)
  function(theta) {
    residual <- checker(theta)
    magnitude <- vapply(seq_along(quadratic$c_i), function(i) {
      sum(abs(theta) * drop(abs(quadratic$A_i[[i]]) %*% abs(theta))) +
        sum(abs(quadratic$b_i[[i]] * theta)) + abs(quadratic$c_i[i])
    }, numeric(1))
    if (any(!is.finite(c(residual, magnitude)))) {
      stop_hetid("Constraint-relative feasibility arithmetic exceeds the numeric range")
    }
    all(residual <= IDENTIFIED_SET_CONTROL$FEAS_TOL * magnitude)
  }
}
