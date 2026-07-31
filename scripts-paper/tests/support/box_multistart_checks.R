# News-box multistart widening and the engine's box-escape measure.

paper_source_once(paper_path(
  "mean_equation", "inference", "refine_bounds_by_tau.R"
))
paper_source_once(paper_path("log_variance", "engine", "polish_support.R"))

# Annulus 1 <= |theta| <= 2: bounded, non-convex, and the coordinate extremes
# (+/- 2) sit on the outer circle while the origin start is infeasible.
annulus <- list(
  A_i = list(diag(2L), -diag(2L)),
  b_i = list(c(0, 0), c(0, 0)),
  c_i = c(-4, 1)
)
annulus_tab <- data.frame(
  coef = c("t1", "t2"),
  set_lower = c(-0.5, -0.5),
  set_upper = c(0.5, 0.5),
  status = rep(PAPER_ENDPOINT_STATUS[["bounded"]], 2L),
  stringsAsFactors = FALSE
)
widened <- widen_theta_box(annulus, annulus_tab)
check(
  "box multistart never narrows an interval",
  all(widened$tab$set_lower <= annulus_tab$set_lower) &&
    all(widened$tab$set_upper >= annulus_tab$set_upper)
)
check(
  "every accepted argmax is feasible for the system it was solved on",
  length(widened$args) > 0L &&
    all(vapply(
      widened$args,
      function(theta) {
        max(quadratic_constraint_values(theta, annulus)) <=
          PAPER_QUADRATIC_CONTROL$feasibility_tolerance
      },
      logical(1)
    ))
)

uncertified <- annulus_tab
uncertified$status <- rep(PAPER_ENDPOINT_STATUS[["unreliable"]], 2L)
check(
  "box multistart leaves uncertified rows untouched",
  identical(
    widen_theta_box(annulus, uncertified)$tab[c("set_lower", "set_upper")],
    uncertified[c("set_lower", "set_upper")]
  )
)

# Widening reads the collapsed status and writes only the two endpoint columns,
# so the per-side statuses the endpoint bootstrap studentizes by must survive it
# untouched. A half-infinite row is skipped entirely, which leaves its live side
# un-widened -- recorded here because it is the tau*-safe behavior, not a defect.
sided_tab <- annulus_tab
sided_tab$lower_status <- c(
  PAPER_ENDPOINT_STATUS[["bounded"]], PAPER_ENDPOINT_STATUS[["bounded"]]
)
sided_tab$upper_status <- c(
  PAPER_ENDPOINT_STATUS[["bounded"]], PAPER_ENDPOINT_STATUS[["unbounded"]]
)
sided_tab$status[2L] <- PAPER_ENDPOINT_STATUS[["unbounded"]]
sided_widened <- widen_theta_box(annulus, sided_tab)$tab
check(
  "box multistart passes the per-side statuses through unchanged",
  identical(
    sided_widened[c("coef", "status", "lower_status", "upper_status")],
    sided_tab[c("coef", "status", "lower_status", "upper_status")]
  )
)
check(
  "a half-infinite row's live side is left un-widened by the collapsed gate",
  identical(sided_widened$set_lower[[2L]], sided_tab$set_lower[[2L]]) &&
    sided_widened$set_lower[[1L]] < sided_tab$set_lower[[1L]]
)

pool <- theta_box_start_pool(annulus, list(c(0.3, 0.4), c(0.3, 0.4)))
check(
  "start pool carries the origin, both axis directions, and one warm copy",
  length(pool) == 6L &&
    sum(vapply(pool, function(p) all(p == 0), logical(1))) == 1L &&
    sum(vapply(pool, function(p) isTRUE(all.equal(p, c(0.3, 0.4))), logical(1))) == 1L
)

# Regression pin on the system that exposed the defect: the news quadratic at
# the last bounds-by-tau grid tau (0.39727783203125). The origin start clips
# sdf_news_pc3 at 0.33 while the set reaches 1.36, and the axis round alone does
# not find it -- only the cross-seeding round does, so this check fails if that
# round is dropped or max_rounds falls to one.
fixture_qs <- list(
  A_i = list(
    matrix(c(
      1042.3401229991509, -78.63527437144954, -8.5789347183840299,
      -78.63527437144954, 4.3916380455361477, 0.62344784255152641,
      -8.5789347183840299, 0.62344784255152641, -0.017189848384816533
    ), 3L, 3L),
    matrix(c(
      5.1734579373981529, -1.1258976063897639, -0.066995733847777655,
      -1.1258976063897639, 0.2013477749496117, 0.012348066083466176,
      -0.066995733847777655, 0.012348066083466176, -0.00021953019843060735
    ), 3L, 3L),
    matrix(c(
      0.046153471569346953, -0.0057016685944292628, -0.0045953282920485955,
      -0.0057016685944292628, 0.00038150581466585558, 0.00043712821546027788,
      -0.0045953282920485955, 0.00043712821546027788, 0.00031963272286392321
    ), 3L, 3L)
  ),
  b_i = list(
    c(-37.71960005209715, 2.6303876435862406, 0.29010042756336285),
    c(-0.17081930717486318, 0.037652028570364648, 0.0020423814000701205),
    c(-0.0058656655034263119, 0.00058449131834501498, 0.00044757611019488554)
  ),
  c_i = c(
    0.18342000916914233, -3.0269715408043872e-05, 6.5994610610396141e-05
  )
)
origin_bounds <- solve_all_profile_bounds(fixture_qs)
fixture_tab <- data.frame(
  coef = paste0("sdf_news_pc", 1:3),
  set_lower = origin_bounds$lower,
  set_upper = origin_bounds$upper,
  status = rep(PAPER_ENDPOINT_STATUS[["bounded"]], 3L),
  stringsAsFactors = FALSE
)
check(
  "the origin start reproduces the clipped news box on the pinned system",
  all(origin_bounds$bounded_lower & origin_bounds$bounded_upper) &&
    isTRUE(all.equal(origin_bounds$upper[[3L]], 0.3298424232, tolerance = 1e-6))
)
fixture_widened <- widen_theta_box(fixture_qs, fixture_tab)
check(
  "box multistart recovers the branch the origin start misses",
  isTRUE(all.equal(
    fixture_widened$tab$set_upper[[3L]], 1.3573089621,
    tolerance = 1e-6
  )) &&
    isTRUE(all.equal(
      fixture_widened$tab$set_lower[[2L]], -0.83730822524,
      tolerance = 1e-6
    ))
)
check(
  "every widened endpoint on the pinned system is feasible there",
  all(vapply(
    fixture_widened$args,
    function(theta) {
      max(quadratic_constraint_values(theta, fixture_qs)) <=
        PAPER_QUADRATIC_CONTROL$feasibility_tolerance *
          max(1, max(abs(unlist(fixture_qs$c_i))))
    },
    logical(1)
  ))
)

# The PPML l.pc1 lower endpoint at that same tau was attained at this theta,
# which the box the scan was handed claimed to contain. The escape measure
# must see it.
escaped_arg <- c(0.1487659155, 0.9860762663, 1.3573089608)
reported_box <- data.frame(
  coef = paste0("sdf_news_pc", 1:3),
  set_lower = c(-0.099792537, -0.397818330, -1.465076324),
  set_upper = c(0.14876592, 0.98607627, 0.32984242),
  stringsAsFactors = FALSE
)
escape_oracle <- max(vapply(seq_len(3L), function(k) {
  lo <- reported_box$set_lower[k]
  hi <- reported_box$set_upper[k]
  max(lo - escaped_arg[k], escaped_arg[k] - hi) /
    max(hi - lo, abs(lo), abs(hi), 1)
}, numeric(1)))
check(
  "box escape matches an independent per-coordinate oracle",
  isTRUE(all.equal(
    logvar_box_escape(escaped_arg, reported_box),
    escape_oracle
  ))
)
check(
  "the clipped news box is caught as an escape",
  logvar_box_escape(escaped_arg, reported_box) >
    PAPER_QUADRATIC_CONTROL$box_escape_rtol
)
check(
  "a point inside the box does not escape",
  logvar_box_escape(c(0, 0, 0), reported_box) <= 0
)
check(
  "an unavailable attaining point yields no escape verdict",
  is.na(logvar_box_escape(c(NA_real_, 0, 0), reported_box))
)
