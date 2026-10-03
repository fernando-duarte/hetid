lv_set_recession_self_test <- function(x_mat, control) {
  n <- nrow(x_mat)
  positive <- lv_set_recession(rep(1, n), x_mat, control)
  all_zero <- lv_set_recession(rep(0, n), x_mat, control)
  tol <- positive$rate_tol
  gmat <- rbind(diag(2), -diag(2))
  hvec <- rep(1, 4)
  lambda <- rep(0, 4)
  e_j <- c(1, 0)
  sign_j <- 1
  nu <- -2 * tol
  c_vec <- c(0, 10 * tol)
  raw_bound <- -sum(hvec * lambda) - sign_j * nu
  corrected_bound <- lv_set_facet_dual_bound(
    c_vec, gmat, hvec, e_j, sign_j,
    lambda, nu
  )
  checks <- c(
    positive = identical(positive$classification, "pass"),
    all_zero = identical(all_zero$classification, "negative_recession"),
    residual_adjust = tol > 0 && raw_bound > tol && corrected_bound < tol &&
      abs(corrected_bound + 10 * tol) <=
        64 * .Machine$double.eps * max(tol, abs(corrected_bound), 10 * tol)
  )
  names(checks)[!checks]
}
