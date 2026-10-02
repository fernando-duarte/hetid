residual_chain_fixture <- function() {
  row_id <- seq_len(80L)
  dates <- to_period_end(
    seq(as.Date("2000-01-01"), by = "quarter", length.out = 80L),
    "quarterly"
  )
  macro_source <- data.frame(
    date = dates, pc1 = sin(row_id * 0.37) + cos(row_id * 0.13),
    pc2 = cos(row_id * 0.29) + row_id / 400, gr1.pcecc96 = sin(row_id * 0.71) +
      0.4 * cos(row_id * 0.17) + row_id / 100
  )
  macro_source$pc1[11] <- NA_real_
  macro_source$gr1.pcecc96[17] <- NA_real_
  news_step <- HETID_CONSTANTS$MONTHS_PER_QUARTER
  mats <- news_step * seq_len(5L)
  bond <- data.frame(date = dates)
  for (j in seq_along(mats)) {
    bond[[paste0("y", mats[j])]] <- 2 + 0.03 * j +
      0.12 * sin(row_id * (0.08 + 0.015 * j)) + 0.04 * cos(row_id * 0.19 + j)
    bond[[paste0("tp", mats[j])]] <- 0.15 + 0.01 * j +
      0.03 * cos(row_id * (0.12 + 0.01 * j)) + 0.01 * sin(row_id * 0.31 + j)
  }
  bond[[paste0("y", 5L * news_step)]][31] <- NA_real_
  bond[[paste0("tp", 3L * news_step)]][43] <- NA_real_
  macro_source <- macro_source[rev(row_id), ]
  bond <- bond[c(seq(2L, 80L, 2L), seq(1L, 79L, 2L)), ]
  aligned <- merge(macro_source, bond, by = "date", sort = TRUE)
  conditioning <- cbind(
    pc1 = aligned$pc1, pc2 = aligned$pc2, l.y1 = aligned$gr1.pcecc96,
    l2.y1 = c(NA_real_, head(aligned$gr1.pcecc96, -1L))
  )
  list(
    source = macro_source, bond = bond, aligned = aligned, dates = dates,
    conditioning = conditioning, mats = news_step * c(2L, 4L), step = news_step
  )
}

residual_chain_news <- function(data, i, news_step) {
  # covers horizons above one news period (i > news_step)
  year_units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  percent <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  level <- function(horizon) {
    premium <- if (horizon == news_step) 0 else data[[paste0("tp", horizon)]]
    (horizon * (data[[paste0("y", horizon)]] - premium) -
      (horizon + news_step) * (data[[paste0("y", horizon + news_step)]] -
        data[[paste0("tp", horizon + news_step)]])) / year_units / percent
  }
  current <- level(i)
  previous <- level(i - news_step)
  delta <- tail(previous, -1L) - head(current, -1L)
  weight <- exp(head(current, -1L))
  center <- 0.5 * mean(weight * delta^2, na.rm = TRUE)
  weight * (delta + 0.5 * delta^2) - center
}

residual_chain_ols <- function(y, x) {
  keep <- complete.cases(y, x)
  design <- cbind(1, x[keep, , drop = FALSE])
  beta_hat <- qr.coef(qr(design), y[keep])
  list(
    keep = keep, coefficients = beta_hat,
    residuals = y[keep] - drop(design %*% beta_hat)
  )
}

residual_chain_cov <- function(x, y = x) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  crossprod(sweep(x, 2L, colMeans(x)), sweep(y, 2L, colMeans(y))) / nrow(x)
}

residual_chain_moments <- function(w1, w2, z) {
  slots <- lapply(seq_len(ncol(w2)), function(i) {
    product <- w1 * w2[, i]
    squared <- w2[, i]^2
    multivariate <- w2 * w2[, i]
    list(
      s_i_0 = drop(residual_chain_cov(product)),
      sigma_i_sq = drop(residual_chain_cov(squared)),
      r_i_0 = drop(residual_chain_cov(z, product)),
      r_i_1 = residual_chain_cov(z, multivariate),
      p_i_0 = drop(residual_chain_cov(z, squared)),
      s_i_1 = drop(residual_chain_cov(multivariate, product)),
      s_i_2 = residual_chain_cov(multivariate)
    )
  })
  list(
    s_i_0 = vapply(slots, `[[`, 0, "s_i_0"),
    sigma_i_sq = vapply(slots, `[[`, 0, "sigma_i_sq"),
    r_i_0 = do.call(cbind, lapply(slots, `[[`, "r_i_0")),
    r_i_1 = lapply(slots, `[[`, "r_i_1"),
    p_i_0 = do.call(cbind, lapply(slots, `[[`, "p_i_0")),
    s_i_1 = lapply(slots, `[[`, "s_i_1"),
    s_i_2 = lapply(slots, `[[`, "s_i_2")
  )
}

expect_residual_chain_close <- function(actual, expected, info = NULL,
                                        scale = max(abs(expected))) {
  expect_identical(dim(actual), dim(expected), info = info)
  expect_length(actual, length(expected))
  expect_true(all(is.finite(actual)) && all(is.finite(expected)), info = info)
  expect_true(length(scale) > 0L && all(is.finite(scale)) && all(scale > 0), info = info)
  expect_lt(max(abs(actual - expected) / scale), 1e-9, label = info)
}

expect_residual_chain_moments <- function(actual, expected) {
  for (field in names(expected)) {
    if (is.list(expected[[field]])) {
      for (k in seq_along(expected[[field]])) {
        expect_residual_chain_close(unname(actual[[field]][[k]]), unname(expected[[field]][[k]]),
          info = field
        )
      }
    } else {
      expect_residual_chain_close(unname(actual[[field]]), unname(expected[[field]]),
        info = field
      )
    }
  }
}
