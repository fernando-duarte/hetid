bootstrap_fixture <- function(n = 100L) {
  v <- seq(-0.2, 0.2, length.out = n)
  m <- function(x) matrix(x, n, 1, dimnames = list(NULL, "a"))
  list(
    full = data.frame(
      coef = "a", lower = -2, upper = -1,
      lower_status = "bounded", upper_status = "bounded"
    ),
    draws = list(
      lower = m(-2 + v), upper = m(-1 + v),
      lower_status = m("bounded"), upper_status = m("bounded")
    )
  )
}

bootstrap_fixture_fit <- function(x, ...) {
  bootstrap_set_interval(x$full, x$draws, "pointwise", 0.1, 50, 0.85, ...)
}
