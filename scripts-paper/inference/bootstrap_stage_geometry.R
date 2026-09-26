set_id_boot_geometry <- function(
  est,
  gamma,
  union_taus,
  display_taus,
  quadratic_builder = tau_quadratic_system,
  table_builder = coef_interval_tables_widened
) {
  display_slots <- seq_along(display_taus) + 1L
  stopifnot(
    is.list(est), is.list(est$tau0_quadratic), is.function(quadratic_builder),
    is.function(table_builder), length(display_taus) > 0L,
    identical(union_taus, c(0, display_taus)), !anyNA(union_taus),
    all(is.finite(union_taus)), all(display_taus > 0), !anyDuplicated(display_taus),
    identical(union_taus[display_slots], display_taus)
  )
  quadratics <- vector("list", length(union_taus))
  tables <- vector("list", length(union_taus))
  quadratics[[1L]] <- est$tau0_quadratic
  for (index in seq_along(display_taus)) {
    slot <- display_slots[[index]]
    quadratics[[slot]] <- quadratic_builder(gamma, display_taus[[index]], est$moments)
    points <- if (is.null(est$point0)) NULL else matrix(est$point0$theta, nrow = 1L)
    tables[[slot]] <- table_builder(quadratics[[slot]], est$beta1r, est$beta2r,
      points = points
    )
  }
  list(
    gamma = gamma, taus = union_taus, tau0_slot = 1L,
    display_slots = display_slots, quadratics = quadratics, tables = tables
  )
}
