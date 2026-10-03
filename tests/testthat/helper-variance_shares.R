variance_share_fixture <- function(seed = 3L) {
  with_rng_scope(
    {
      n <- 220L
      z <- rnorm(n)
      x <- stats::prcomp(matrix(rnorm(n * 5L), n, 5L))$x[, 1:3]
      colnames(x) <- paste0("expected_sdf_pc", 1:3)
      y2 <- stats::prcomp(sqrt(exp(0.4 + 0.9 * z)) * matrix(rnorm(n * 5L), n, 5L))$x[, 1:3]
      colnames(y2) <- paste0("sdf_news_pc", 1:3)
      e1 <- rnorm(n) + 0.4 * y2[, 1L] - 0.2 * y2[, 3L]
      y <- drop(0.3 + x %*% c(0.2, -0.1, 0.4) + y2 %*% c(0.5, -0.3, 0.2) + e1)
      list(y = y, x = x, y2 = y2, z = matrix(z, ncol = 1L, dimnames = list(NULL, "z")))
    },
    seed = seed,
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
}

variance_share_flatten <- function(result, scenario) {
  taus <- as.numeric(names(result$set_cols))
  piece <- function(tau, field, value, status = "") {
    data.frame(
      scenario = scenario, tau = tau, field = field, row = seq_along(value),
      value = unname(value), status = status, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, c(
    list(
      piece(NA_real_, "sd_c", result$sd_c), piece(NA_real_, "share_ols", result$ols),
      piece(NA_real_, "share_point", result$point)
    ),
    lapply(seq_along(taus), function(k) {
      cc <- result$set_cols[[k]]
      st <- result$sets[[k]]
      rbind(
        piece(taus[k], "share_lo", cc$lo, cc$status), piece(taus[k], "share_hi", cc$hi),
        piece(taus[k], "theta_lower", st$theta$set_lower, st$theta$lower_status),
        piece(taus[k], "theta_upper", st$theta$set_upper, st$theta$upper_status),
        piece(taus[k], "outer_lower", st$theta$outer_lower),
        piece(taus[k], "outer_upper", st$theta$outer_upper),
        piece(taus[k], "beta1_lower", st$beta1$set_lower, st$beta1$lower_status),
        piece(taus[k], "beta1_upper", st$beta1$set_upper, st$beta1$upper_status)
      )
    })
  ))
}

variance_share_ball_table <- function(dimension = 2L, status = "bounded") {
  data.frame(
    set_lower = rep(-1, dimension), set_upper = rep(1, dimension),
    status = status, outer_lower = rep(-1, dimension), outer_upper = rep(1, dimension)
  )
}
