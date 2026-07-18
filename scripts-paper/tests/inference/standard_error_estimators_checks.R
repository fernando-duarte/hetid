# Offline checks for the shared standard-error scaffolding
# (scripts-paper/log_variance/inference/standard_error_estimators.R): the
# normalized fail-closed inverse
# against the identity and a column-rescaled matrix a raw rcond would reject, the
# Bartlett HAC meat against a brute-force double sum, and the SE-frame / NA-frame
# shape. These helpers back both logvar_ppml_vcov and logvar_harvey_vcov.

seu_d <- function(a, b) max(abs(unname(a) - unname(b)))
# a fixed SPD bread and a fixed 4x3 score matrix (deterministic, no seed needed)
seu_m <- crossprod(matrix(
  c(2, 0.3, -0.1, 0.3, 1.5, 0.2, -0.1, 0.2, 1.1), 3
))
seu_g <- matrix(
  c(0.5, -0.2, 0.1, 0.3, -0.4, 0.2, 0.6, -0.1, 0.05, -0.3, 0.4, -0.2),
  ncol = 3, byrow = TRUE
)

check("se norm_inv inverts an SPD bread (m %*% inv == I)", {
  inv <- logvar_se_norm_inv(seu_m, LOGVAR_PPML_CONTROL$rcond_tol)
  !is.null(inv) && seu_d(seu_m %*% inv, diag(3)) < 1e-10
})
check("se norm_inv accepts a column-rescaled bread a raw rcond would reject", {
  s <- diag(c(1, 1e6, 1))
  m <- s %*% seu_m %*% s # raw rcond(m) ~ 1e-12, normalized is well conditioned
  inv <- logvar_se_norm_inv(m, LOGVAR_PPML_CONTROL$rcond_tol)
  rcond(m) < 1e-10 && !is.null(inv) && seu_d(m %*% inv, diag(3)) < 1e-6
})
check("se norm_inv returns NULL on a singular bread", {
  is.null(logvar_se_norm_inv(
    matrix(c(1, 1, 1, 1), 2),
    LOGVAR_PPML_CONTROL$rcond_tol
  ))
})
check("se norm_inv returns NULL on a non-finite bread", {
  is.null(logvar_se_norm_inv(
    matrix(c(1, NA, NA, 1), 2),
    LOGVAR_PPML_CONTROL$rcond_tol
  ))
})
check("se norm_inv obeys the supplied conditioning tolerance", {
  is.null(logvar_se_norm_inv(seu_m, 1))
})

check("se bartlett meat at lag 0 is the outer-product meat", {
  seu_d(logvar_se_bartlett_meat(seu_g, 0L), crossprod(seu_g)) < 1e-12
})
check("se bartlett meat matches a brute-force Bartlett double sum at lag 2", {
  n <- nrow(seu_g)
  lag <- 2L
  meat <- matrix(0, ncol(seu_g), ncol(seu_g))
  for (a in seq_len(n)) {
    for (b in seq_len(n)) {
      w <- max(0, 1 - abs(a - b) / (lag + 1))
      meat <- meat + w * tcrossprod(seu_g[a, ], seu_g[b, ])
    }
  }
  seu_d(logvar_se_bartlett_meat(seu_g, lag), meat) < 1e-12
})

check("se frame is one row per coef, sqrt of the vcov diagonal", {
  fr <- logvar_se_frame(list(a = diag(c(4, 9)), b = diag(c(1, 16))), c("x1", "x2"))
  identical(fr$coef, c("x1", "x2")) &&
    isTRUE(all.equal(fr$a, c(2, 3))) && isTRUE(all.equal(fr$b, c(1, 4)))
})
check("se frame renders NA on a negative diagonal", {
  fr <- logvar_se_frame(list(a = diag(c(-1, 4))), c("x1", "x2"))
  is.na(fr$a[1]) && isTRUE(all.equal(fr$a[2], 2))
})
check("se na frame is all-NA with the given types", {
  fr <- logvar_se_na_frame(c("x1", "x2"), c("p", "q"))
  identical(fr$coef, c("x1", "x2")) && all(is.na(fr$p)) && all(is.na(fr$q))
})

check("se preflight returns dimensions, means, and variant names", {
  x <- cbind("(Intercept)" = 1, x = c(-1, 0, 1, 2))
  coef <- c(0.2, -0.1)
  y <- c(1, 2, 3, 4)
  pre <- logvar_se_preflight(coef, y, x, 2L, c("a", "b"))
  pre$ok &&
    identical(c(pre$n, pre$p), c(4L, 2L)) &&
    identical(names(pre$na_out), c("a", "b")) &&
    identical(pre$hac_lags, 2L) &&
    isTRUE(all.equal(pre$mu, exp(drop(x %*% coef))))
})
check("se preflight fails closed with named all-NA matrices", {
  x <- cbind("(Intercept)" = 1, x = c(-1, 0, 1, 2))
  pre <- logvar_se_preflight(
    c(0, 0),
    c(1, -1, 2, 3),
    x,
    0L,
    c("a", "b")
  )
  !pre$ok &&
    identical(names(pre$na_out), c("a", "b")) &&
    all(vapply(
      pre$na_out,
      function(value) all(is.na(value)),
      logical(1)
    ))
})

rm(seu_d, seu_m, seu_g)
