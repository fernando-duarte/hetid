# Checked points widen every eligible coordinate side, including half-infinite rows.
paper_source_once(paper_path(
  "mean_equation", "inference", "theta_box_multistart.R"
))
sided_tab <- data.frame(
  coef = c("x", "y"), set_lower = c(-1, -Inf), set_upper = c(1, 1),
  lower_status = c("bounded", "unbounded"),
  upper_status = c("bounded", "bounded"),
  status = c("bounded", "unbounded"), stringsAsFactors = FALSE
)
sided_widened <- profile_widen_theta_points(sided_tab, list(c(2, 4), c(-3, -5)))
check(
  "all checked points widen every eligible theta side",
  identical(sided_widened$set_lower, c(-3, -Inf)) &&
    identical(sided_widened$set_upper, c(2, 4))
)
check(
  "point sharing preserves theta side statuses",
  identical(
    sided_widened[c("lower_status", "upper_status", "status")],
    sided_tab[c("lower_status", "upper_status", "status")]
  )
)

beta_tab <- data.frame(
  coef = c("large", "tiny"), set_lower = c(0, 0), set_upper = c(0, 0),
  lower_status = "bounded", upper_status = "bounded", status = "bounded"
)
loading <- matrix(c(1, 1e-20), 1L,
  dimnames = list("x", c("large", "tiny"))
)
beta_widened <- widen_beta1_from_args(
  beta_tab, c(large = 0, tiny = 0), loading, list(1e20)
)
check(
  "tiny nonzero structural loadings retain the full checked point image",
  identical(beta_widened$set_lower, c(-1e20, -1)) &&
    identical(beta_widened$set_upper, c(0, 0))
)

# The origin is infeasible in this annulus. Starts and accepted points are
# checked against the joint system, while statuses come from geometry.
annulus <- list(
  A_i = list(diag(2), -diag(2)),
  b_i = list(c(0, 0), c(0, 0)), c_i = c(-4, 1)
)
annulus_tab <- data.frame(
  coef = c("t1", "t2"), set_lower = c(-.5, -.5),
  set_upper = c(.5, .5), lower_status = "bounded",
  upper_status = "bounded", status = "bounded"
)
pool <- theta_box_start_pool(annulus, list(c(.3, .4), c(.3, .4)))
check(
  "multistart pool retains origin, axis starts, and one warm copy",
  length(pool) == 6L &&
    sum(vapply(pool, function(p) all(p == 0), logical(1))) == 1L &&
    sum(vapply(pool, function(p) isTRUE(all.equal(p, c(.3, .4))), logical(1))) == 1L
)
annulus_widened <- widen_theta_box(annulus, annulus_tab)
check(
  "multistart never narrows an eligible finite side",
  all(annulus_widened$tab$set_lower <= annulus_tab$set_lower) &&
    all(annulus_widened$tab$set_upper >= annulus_tab$set_upper)
)
check(
  "every returned multistart point satisfies the full joint system",
  length(annulus_widened$args) > 0L &&
    all(vapply(annulus_widened$args, function(theta) {
      max(quadratic_constraint_values(theta, annulus)) < 0
    }, logical(1)))
)
check(
  "annulus geometry certifies both coordinate sides bounded",
  all(annulus_widened$tab$lower_status == "bounded") &&
    all(annulus_widened$tab$upper_status == "bounded")
)
