lv_logols_checked <- function(prep, b, jacobian) {
  result <- evaluate_log_projection(prep, b, "log", jacobian = jacobian)
  if (result$status != "ok" || any(!is.finite(result$coef))) {
    return(NULL)
  }
  if (jacobian && (is.null(result$jacobian) || any(!is.finite(result$jacobian)))) {
    return(NULL)
  }
  result
}

lv_logols_value <- function(prep, groups, j, b, representatives, log_offset, grouped) {
  fit <- lv_logols_checked(prep, b, FALSE)
  if (is.null(fit)) {
    return(NaN)
  }
  if (!grouped) {
    return(unname(fit$coef[j]))
  }
  e <- drop(prep$w1 - prep$w2 %*% matrix(b, ncol = 1L))[representatives]
  if (any(!is.finite(e)) || any(e == 0)) {
    return(NaN)
  }
  value <- sum(2 * groups$weights[j, ] * log(abs(e))) + log_offset[[j]]
  if (is.finite(value)) value else NaN
}

lv_logols_gradient <- function(prep, groups, j, b, representatives, grouped) {
  fit <- lv_logols_checked(prep, b, TRUE)
  unavailable <- rep(NaN, ncol(prep$w2))
  if (is.null(fit)) {
    return(unavailable)
  }
  if (!grouped) {
    return(fit$jacobian[j, ])
  }
  e <- drop(prep$w1 - prep$w2 %*% matrix(b, ncol = 1L))[representatives]
  if (any(!is.finite(e)) || any(e == 0)) {
    return(unavailable)
  }
  value <- drop(-2 * crossprod(
    groups$weights[j, ] / e,
    prep$w2[representatives, , drop = FALSE]
  ))
  if (any(!is.finite(value))) {
    return(unavailable)
  }
  stats::setNames(value, colnames(prep$w2))
}

lv_logols_objective <- function(prep, groups, j) {
  force(j)
  representatives <- vapply(groups$rows, `[`, integer(1), 1L)
  grouped <- any(lengths(groups$rows) > 1L)
  log_offset <- drop(prep$projection %*% (2 * log(abs(groups$factors))))
  list(
    admit = function(b) all(is.finite(b)) && !is.null(lv_logols_checked(prep, b, TRUE)),
    fn = function(b) lv_logols_value(prep, groups, j, b, representatives, log_offset, grouped),
    gr = function(b) lv_logols_gradient(prep, groups, j, b, representatives, grouped)
  )
}
