# Independent rays and saved raw coefficients pin three false-finite draws.
raw_directions <- list(
  A2004 = c(-.042, .711, 1),
  B2736 = c(.125301, .984696, -.121132),
  B4691 = c(-.077759, .936779, .341172)
)
ray_starts <- c(A2004 = 40, B2736 = 2048, B4691 = 128)
for (name in names(raw_directions)) {
  qs <- profile_fixture_quadratic(name)
  ray <- raw_directions[[name]]
  check(
    sprintf("%s raw constraints admit both strict ray directions", name),
    profile_ray_check(qs, ray, ray_starts[[name]])
  )
  point <- if (name == "A2004") {
    matrix(c(
      .11448928398921554, -1.59721315379157325,
      -3.29090066679105053
    ), 1L)
  } else {
    NULL
  }
  automatic <- hetid::compute_quadratic_set_evidence(
    qs, diag(3),
    points = point
  )
  check(
    sprintf("%s default search finds both strict tails", name),
    all(automatic$summary$lower_state == "unbounded") &&
      all(automatic$summary$upper_state == "unbounded")
  )
  evidence <- paper_profile_evidence(qs, diag(3),
    points = point, directions = matrix(ray, 1L)
  )
  check(
    sprintf("%s evidence makes every theta side unbounded", name),
    all(evidence$summary$lower_state == "unbounded") &&
      all(evidence$summary$upper_state == "unbounded")
  )
  bounds <- solve_all_profile_bounds(qs, evidence = evidence)
  check(
    sprintf("%s profile endpoints are both infinite with valid tails", name),
    all(is.infinite(bounds$lower) & bounds$lower < 0) &&
      all(is.infinite(bounds$upper) & bounds$upper > 0) &&
      all(bounds$valid_lower & bounds$valid_upper) &&
      !any(bounds$bounded_lower | bounds$bounded_upper)
  )
}

# These design coefficients had false finite cached sides despite nonzero ray loading.
loading_rows <- read.csv(
  file.path(profile_fixture_root, "B-structural-loadings.csv"),
  stringsAsFactors = FALSE
)
for (draw in c(2736L, 4691L)) {
  qs <- profile_fixture_quadratic(paste0("B", draw))
  ray <- raw_directions[[paste0("B", draw)]]
  for (coef in c("(Intercept)", "lag_expected_sdf_pc2")) {
    rows <- loading_rows$draw == draw & loading_rows$coef == coef
    loading <- loading_rows$loading[rows]
    check(
      sprintf("B%d %s loading has a nonzero strict-ray projection", draw, coef),
      length(loading) == 3L && abs(sum(loading * ray)) > 1e-8
    )
    evidence <- paper_profile_evidence(qs, matrix(loading, ncol = 1L),
      directions = matrix(ray, 1L)
    )
    lower <- solve_linear_functional_bound(qs, loading, "min", evidence = evidence)
    upper <- solve_linear_functional_bound(qs, loading, "max", evidence = evidence)
    check(
      sprintf("B%d %s structural objective has two valid infinite sides", draw, coef),
      identical(lower$bound, -Inf) && identical(upper$bound, Inf) &&
        lower$valid && upper$valid && !lower$bounded && !upper$bounded
    )
  }
}
