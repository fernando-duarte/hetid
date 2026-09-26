# Standalone worker loading and the affine-image contract of the real builder.
source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("inference", "bootstrap_stage_draw.R"))

local({
  beta1r <- c(first = 3, second = -2, zero = 5, tiny = 6, unreliable = 7, half = 8)
  beta2r <- rbind(c(1, -2, 0, 1e-16, 1, 1), c(2, 1, 0, -1e-16, 1, 1))
  dimnames(beta2r) <- list(c("theta1", "theta2"), names(beta1r))
  order <- c("second", "zero", "first", "half", "tiny", "unreliable")
  beta <- data.frame(
    coef = order, set_lower = beta1r[order] - 0.1, set_upper = beta1r[order] + 0.1,
    status = "bounded", lower_status = "bounded", upper_status = "bounded"
  )
  beta[beta$coef %in% c("zero", "tiny"), c("set_lower", "set_upper")] <-
    beta1r[c("zero", "tiny")]
  beta[beta$coef == "half", c("status", "upper_status")] <- "unbounded"
  beta$set_upper[beta$coef == "half"] <- Inf
  beta[beta$coef == "unreliable", c("status", "lower_status")] <- "unreliable"
  theta <- data.frame(
    coef = rownames(beta2r), set_lower = -0.1, set_upper = 0.1,
    outer_lower = -1, outer_upper = 1,
    lower_status = "bounded", upper_status = "bounded", status = "bounded"
  )
  points <- rbind(diag(2L), -diag(2L))
  args <- lapply(seq_len(nrow(points)), function(i) points[i, ])
  env <- new.env(parent = .GlobalEnv)
  env$coef_interval_tables_from_quadratic <- function(
    qs, beta1r, beta2r, points = NULL, evidence = NULL
  ) {
    list(beta1 = beta, theta = theta)
  }
  calls <- 0L
  env$widen_theta_box <- function(qs, theta_tab, warm = NULL, evidence = NULL) {
    calls <<- calls + 1L
    wide <- theta
    wide$set_lower <- -1
    wide$set_upper <- 1
    list(tab = wide, args = args, evidence = evidence, corrections = NULL)
  }
  build <- coef_interval_tables_widened
  environment(build) <- env
  qs <- list(A_i = list(diag(2L)), b_i = list(c(0, 0)), c_i = -1)
  out <- build(qs, beta1r, beta2r)
  # These points lie on the unit circle. Their affine images must be retained
  # even when the earlier functional search found only a smaller interval.
  images <- sweep(-points %*% beta2r, 2L, beta1r, "+")
  for (name in c("first", "second")) {
    row <- out$beta1[out$beta1$coef == name, ]
    stopifnot(row$set_lower <= min(images[, name]), row$set_upper >= max(images[, name]))
  }
  zero_row <- beta$coef == "zero"
  half_row <- beta$coef == "half"
  unreliable_row <- beta$coef == "unreliable"
  stopifnot(
    calls == 1L,
    identical(out$beta1[zero_row, ], beta[zero_row, ]),
    out$beta1$set_lower[half_row] <= min(images[, "half"]),
    identical(out$beta1$set_upper[half_row], Inf),
    identical(
      out$beta1$set_lower[unreliable_row],
      beta$set_lower[unreliable_row]
    ),
    out$beta1$set_upper[unreliable_row] >= max(images[, "unreliable"]),
    identical(
      out$beta1[c("coef", "status", "lower_status", "upper_status")],
      beta[c("coef", "status", "lower_status", "upper_status")]
    )
  )
  null <- build(qs, beta1r, beta2r * 0)
  stopifnot(identical(null$beta1, beta), identical(null$theta, out$theta))

  # Use the real optimizer on a nonconvex annulus; beta1 = -theta gives an
  # analytic independent target for both structural rows.
  env$widen_theta_box <- widen_theta_box
  beta1r <- c(theta1 = 0, theta2 = 0)
  beta2r <- diag(2L)
  dimnames(beta2r) <- list(names(beta1r), names(beta1r))
  theta$outer_lower <- -2
  theta$outer_upper <- 2
  beta <- theta
  qs$A_i <- list(diag(2L), -diag(2L))
  qs$b_i <- list(c(0, 0), c(0, 0))
  qs$c_i <- c(-4, 1)
  annulus <- build(qs, beta1r, beta2r)
  stopifnot(
    all(abs(annulus$beta1$set_lower + 2) < 1e-6),
    all(abs(annulus$beta1$set_upper - 2) < 1e-6)
  )
})
cat("bootstrap_structural_refinement_checks: PASS\n")
