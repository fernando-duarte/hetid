# Draw B4112 has a thin bounded set at tau 0.10. Its original anchor cannot
# repair the pc1 endpoint within the cap, but another checked multistart can.
pool_quadratic <- profile_fixture_quadratic("B4112")
pool_anchor <- matrix(c(
  0.13486407730035194, -1.800865985763606, 12.404050447622586
), 1L)
pool_loading <- diag(3)
dimnames(pool_loading) <- list(paste0("pc", seq_len(3L)), paste0("b", seq_len(3L)))
pool_tables <- coef_interval_tables_widened(
  pool_quadratic, setNames(numeric(3), colnames(pool_loading)), pool_loading,
  points = pool_anchor
)
pool_theta <- pool_tables$theta
check(
  "checked multistart anchors recover the thin bounded pc1 endpoint",
  pool_theta$lower_status[1L] == "bounded" &&
    is.finite(pool_theta$set_lower[1L]) &&
    abs(pool_theta$set_lower[1L] + 0.229026215125) < 1e-7
)
check(
  "retried endpoint stays inside the verified containing bound",
  pool_theta$set_lower[1L] >= pool_theta$outer_lower[1L] &&
    pool_theta$set_lower[1L] - pool_theta$outer_lower[1L] < 1e-7
)
pool_evidence <- paper_profile_evidence(pool_quadratic, diag(3))
check(
  "retry point sharing preserves strict membership",
  all(vapply(attr(pool_tables, "profile_points"), pool_evidence$check_point, logical(1)))
)
check(
  "retry preserves the existing movement cap",
  all(attr(pool_tables, "profile_corrections")$movement <=
    PAPER_QUADRATIC_CONTROL$candidate_correction_rtol, na.rm = TRUE)
)
