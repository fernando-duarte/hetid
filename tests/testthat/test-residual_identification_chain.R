test_that("dated residuals with lags and interior gaps reach the correct constraints", {
  fx <- residual_chain_fixture()
  data <- fx$aligned
  expect_false(identical(fx$source$date, fx$bond$date))
  expect_identical(data$date, fx$dates)
  for (raw in list(fx$source, fx$bond)) {
    columns <- setdiff(names(raw), "date")
    expect_identical(
      unname(as.matrix(data[, columns])),
      unname(as.matrix(raw[match(fx$dates, raw$date), columns]))
    )
  }
  w1 <- compute_w1_residuals(n_pcs = 2L, data = data, y1_lags = 2L)
  w2 <- compute_w2_residuals(
    data[, paste0("y", fx$step * seq_len(5L))],
    data[, paste0("tp", fx$step * seq_len(5L))],
    maturities = fx$mats, n_pcs = 2L, pcs = as.matrix(data[, c("pc1", "pc2")]),
    dates = data$date, step = fx$step, y1 = data$gr1.pcecc96, y1_lags = 2L
  )
  labels <- paste0("maturity_", fx$mats)
  expected_rows <- list(
    w1 = setdiff(3:80, c(12, 17, 18, 19)),
    w2_first = setdiff(3:80, c(12, 18, 19, 44)),
    w2_second = setdiff(3:80, c(12, 18, 19, 32, 43))
  )
  expect_identical(w1$dates, fx$dates[expected_rows$w1])
  expect_identical(w2$dates[[labels[1]]], fx$dates[expected_rows$w2_first])
  expect_identical(w2$dates[[labels[2]]], fx$dates[expected_rows$w2_second])
  expect_length(w2$skipped, 0L)
  expect_identical(
    colnames(w2$coefficients),
    c("(Intercept)", "pc1", "pc2", "l.y1", "l2.y1")
  )
  predictors <- head(fx$conditioning, -1L)
  manual_w1 <- residual_chain_ols(tail(data$gr1.pcecc96, -1L), predictors)
  manual_w2 <- lapply(fx$mats, function(i) {
    residual_chain_ols(residual_chain_news(data, i, fx$step), predictors)
  })
  expect_identical(which(manual_w1$keep) + 1L, expected_rows$w1)
  for (k in seq_along(manual_w2)) {
    expect_identical(which(manual_w2[[k]]$keep) + 1L, expected_rows[[k + 1L]])
  }
  expect_identical(which(w1$kept_idx), expected_rows$w1 - 1L)
  expect_residual_chain_close(unname(w1$residuals), unname(manual_w1$residuals))
  expect_residual_chain_close(unname(w1$coefficients), unname(manual_w1$coefficients))
  for (k in seq_along(labels)) {
    expect_identical(which(w2$kept_idx[[labels[k]]]), expected_rows[[k + 1L]] - 1L)
    expect_residual_chain_close(
      unname(w2$residuals[[labels[k]]]), unname(manual_w2[[k]]$residuals)
    )
    expect_residual_chain_close(unname(w2$coefficients[k, ]), unname(manual_w2[[k]]$coefficients))
  }
  common <- data.frame(date = w1$dates, w1 = as.numeric(w1$residuals))
  for (k in seq_along(labels)) {
    one <- data.frame(date = w2$dates[[labels[k]]], value = w2$residuals[[labels[k]]])
    names(one)[2] <- paste0("w2_", k)
    common <- merge(common, one[rev(seq_len(nrow(one))), ], by = "date", sort = TRUE)
  }
  retained <- setdiff(3:80, c(12, 17, 18, 19, 32, 43, 44))
  expect_identical(common$date, fx$dates[retained])
  expect_identical(nrow(common), 71L)
  instruments <- data.frame(date = tail(data$date, -1L), predictors)
  z <- as.matrix(instruments[match(common$date, instruments$date), -1L])
  ww <- as.matrix(common[, c("w2_1", "w2_2")])
  expected_w1 <- manual_w1$residuals[match(retained, which(manual_w1$keep) + 1L)]
  expected_ww <- do.call(cbind, lapply(manual_w2, function(fit) {
    fit$residuals[match(retained, which(fit$keep) + 1L)]
  }))
  expected_z <- fx$conditioning[retained - 1L, , drop = FALSE]
  expected_gamma <- vapply(manual_w2, function(fit) fit$coefficients[-1L], numeric(4))
  gamma <- t(w2$coefficients[, colnames(z), drop = FALSE])
  expect_residual_chain_close(unname(common$w1), unname(expected_w1))
  expect_residual_chain_close(unname(ww), unname(expected_ww))
  expect_residual_chain_close(unname(z), unname(expected_z))
  expect_residual_chain_close(unname(gamma), unname(expected_gamma))
  expect_identical(colnames(gamma), labels)
  moments <- compute_identification_moments(common$w1, ww, z)
  expected <- residual_chain_moments(expected_w1, expected_ww, expected_z)
  expect_identical(rownames(moments$r_i_0), rownames(gamma))
  expect_identical(names(moments$s_i_0), paste0("maturity_", 1:2))
  expect_identical(attr(moments, "n_obs"), 71L)
  expect_identical(attr(moments, "n_components"), 2L)
  expect_identical(attr(moments, "n_instruments"), 4L)
  expect_identical(attr(moments, "maturities"), c(1L, 2L))
  expect_residual_chain_moments(moments, expected)
  thetas <- list(c(0, 0), c(0.3, -0.8), c(-1.1, 0.2))
  for (order in list(c(1L, 2L), c(2L, 1L))) {
    ordered <- compute_identification_moments(common$w1, ww, z, maturities = order)
    expect_identical(attr(ordered, "maturities"), order)
    for (tau in list(c(0, 0), c(0.15, 0.4))) {
      system <- build_quadratic_system(gamma, tau, ordered)
      checker <- make_system_checker(system$quadratic)
      expect_identical(names(system$quadratic$A_i), paste0("maturity_", order))
      for (theta in thetas) {
        terms <- vapply(order, function(i) {
          product <- (expected_w1 - drop(expected_ww %*% theta)) * expected_ww[, i]
          projection <- drop(expected_z %*% expected_gamma[, i])
          numerator <- drop(residual_chain_cov(projection, product))^2
          slack <- tau[i]^2 * drop(residual_chain_cov(projection, expected_ww[, i]^2))^2 /
            drop(residual_chain_cov(expected_ww[, i]^2))
          c(numerator = numerator, penalty = slack * drop(residual_chain_cov(product)))
        }, numeric(2))
        direct <- unname(terms["numerator", ] - terms["penalty", ])
        scale <- unname(colSums(abs(terms)))
        info <- paste(
          "theta", paste(theta, collapse = ","), "tau", paste(tau, collapse = ","),
          "maturities", paste(order, collapse = ",")
        )
        expect_residual_chain_close(unname(checker(theta)), direct, info = info, scale = scale)
      }
    }
  }
})
