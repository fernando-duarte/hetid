# Hand-built hetid_theta_box parts, so the container tests can break one
# piece at a time without running a search
make_box_parts <- function(n_components = 2L, n_obs = 40L) {
  set.seed(42)
  w2 <- matrix(rnorm(n_obs * n_components), n_obs, n_components)
  colnames(w2) <- paste0("news", seq_len(n_components))
  list(
    bounds = data.frame(
      coef = colnames(w2),
      lower = rep(-1, n_components),
      upper = rep(1, n_components),
      row.names = NULL
    ),
    arg_lower = matrix(0, n_components, n_components),
    arg_upper = matrix(0, n_components, n_components),
    beta1_bounds = data.frame(
      coef = c("(Intercept)", "x1"),
      lower = c(0.3, -0.2),
      upper = c(0.3, 0.1),
      row.names = NULL
    ),
    beta1_arg_lower = matrix(0, 2L, n_components),
    beta1_arg_upper = matrix(0, 2L, n_components),
    null_loading = c("(Intercept)" = TRUE, x1 = FALSE),
    w1 = rnorm(n_obs),
    w2 = w2,
    quadratic = list(
      A_i = list(diag(n_components)),
      b_i = list(rep(0, n_components)),
      c_i = -1
    ),
    tau = 0.05,
    n_grid = 21L,
    n_obs = n_obs
  )
}

build_box <- function(...) {
  parts <- utils::modifyList(make_box_parts(), list(...))
  do.call(new_hetid_theta_box, parts)
}
