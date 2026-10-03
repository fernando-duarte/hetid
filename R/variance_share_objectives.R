variance_share_quadratic <- function(p_mat, q_vec, r_val) {
  list(
    value = function(pts) rowSums((pts %*% p_mat) * pts) + drop(pts %*% q_vec) + r_val,
    grad = function(theta) drop(2 * (p_mat %*% theta) + q_vec)
  )
}

variance_share_objectives <- function(fit, x_names, covariance) {
  s_e <- covariance$s_e
  s_n <- covariance$s_n
  s_en <- covariance$s_en
  var_c <- covariance$var_c
  beta1r_e <- fit$beta1r[x_names]
  beta2r_e <- fit$beta2r[, x_names, drop = FALSE]
  b_map <- t(beta2r_e)
  m_s_en <- crossprod(b_map, s_en)
  list(
    expected = variance_share_quadratic(
      100 * crossprod(b_map, s_e %*% b_map) / var_c,
      drop(-200 * beta2r_e %*% (s_e %*% beta1r_e)) / var_c,
      100 * drop(beta1r_e %*% s_e %*% beta1r_e) / var_c
    ),
    news = variance_share_quadratic(100 * s_n / var_c, numeric(nrow(beta2r_e)), 0),
    combined = variance_share_quadratic(
      100 * (crossprod(b_map, s_e %*% b_map) - (m_s_en + t(m_s_en)) + s_n) / var_c,
      drop(-200 * beta2r_e %*% (s_e %*% beta1r_e) +
        200 * crossprod(s_en, beta1r_e)) / var_c,
      100 * drop(beta1r_e %*% s_e %*% beta1r_e) / var_c
    )
  )
}

variance_share_fixed <- function(b_e, b_n, covariance) {
  s_e <- covariance$s_e
  s_n <- covariance$s_n
  s_joint <- rbind(cbind(s_e, covariance$s_en), cbind(t(covariance$s_en), s_n))
  block_share <- function(coefs, s_block) {
    100 * rowSums((coefs %*% s_block) * coefs) / covariance$var_c
  }
  unname(c(
    block_share(matrix(b_e, 1), s_e), 100 * b_e^2 * diag(s_e) / covariance$var_c,
    block_share(matrix(b_n, 1), s_n), 100 * b_n^2 * diag(s_n) / covariance$var_c,
    block_share(matrix(c(b_e, b_n), 1), s_joint)
  ))
}
