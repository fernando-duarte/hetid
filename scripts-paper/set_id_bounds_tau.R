# Identified intervals for every coefficient of the structural consumption-
# growth equation as a function of the slack tau: on a grid over [0, tau*],
# the exact per-coefficient range of the joint identified set from
# set_id_mean_eq.R (profile bounds for the news coefficients, linear-
# functional bounds beta1(theta) = beta1R - beta2R' theta for the design
# coefficients), with the closed-form tau = 0 point as the left endpoint.
# A warm-started refinement re-solves each news bound from the previous grid
# point's argmax: the shared solver starts every solve at the origin and can
# drop into a lower local vertex mid-grid (a dip in the pc2 upper bound near
# tau = 0.3), while continuation along the grid tracks the true branch.
# Writes set_id_bounds_tau.pdf to scripts-paper/output/.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")

theta_coefs <- set_id_mean_eq$theta_table$coef
beta_coefs <- set_id_mean_eq$beta1_table$coef

# SLSQP extremization of theta_k from an arbitrary feasible start, in the
# shared solver's scaling (mirrors .solve_scaled, which pins the start at the
# origin); returns the theta-units bound and argmax, or NULL when the solve
# fails or the endpoint misses the feasible+active certificate
solve_theta_bound_from <- function(qs, k, direction, theta_start,
                                   box = 1e6, feas_tol = 1e-4) {
  if (is.null(theta_start)) {
    return(NULL)
  }
  delta <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, delta)
  sgn <- if (direction == "min") 1 else -1
  dim_theta <- ncol(qs$A_i[[1]])
  e_k <- numeric(dim_theta)
  e_k[k] <- 1
  res <- tryCatch(
    nloptr::slsqp(
      x0 = pmin(pmax(theta_start / delta, -box), box),
      fn = function(phi) sgn * sum(e_k * phi),
      gr = function(phi) sgn * e_k,
      lower = rep(-box, dim_theta), upper = rep(box, dim_theta),
      hin = function(phi) {
        theta <- delta * phi
        vapply(seq_along(qs$A_i), function(i) {
          (drop(t(theta) %*% qs$A_i[[i]] %*% theta) +
            sum(qs$b_i[[i]] * theta) + qs$c_i[i]) / omega[i]
        }, numeric(1))
      },
      hinjac = function(phi) {
        theta <- delta * phi
        t(vapply(seq_along(qs$A_i), function(i) {
          (delta * (2 * drop(qs$A_i[[i]] %*% theta) + qs$b_i[[i]])) / omega[i]
        }, numeric(dim_theta)))
      },
      control = list(xtol_rel = 1e-8, maxeval = 1000),
      deprecatedBehavior = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(res) || any(!is.finite(res$par))) {
    return(NULL)
  }
  theta <- delta * res$par
  resid <- .feasibility_residual(qs, theta, omega)
  if (!is.finite(resid) || abs(resid) > feas_tol) {
    return(NULL)
  }
  list(bound = theta[k], theta = theta)
}

# warm-start state, seeded with the tau = 0 closed-form point (the whole set
# at tau = 0): each successful solve hands its argmax to the next grid tau,
# where it is still feasible because the set grows with tau
seed_theta <- set_id_mean_eq$theta_table$point
if (anyNA(seed_theta)) seed_theta <- NULL
warm <- list(
  min = rep(list(seed_theta), length(theta_coefs)),
  max = rep(list(seed_theta), length(theta_coefs))
)
refined_n <- 0L

# warm-started refinement of the news intervals at one grid tau: re-solve
# each side from the previous grid point's argmax and keep a certified value
# only when it extends the origin-start solve
refine_theta_intervals <- function(tau, theta_tab) {
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  for (k in seq_along(theta_coefs)) {
    for (side in c("min", "max")) {
      cand <- solve_theta_bound_from(qs, k, side, warm[[side]][[k]])
      if (is.null(cand)) next
      warm[[side]][[k]] <<- cand$theta
      if (theta_tab$status[k] != "bounded") next
      if (side == "max" && cand$bound > theta_tab$set_upper[k]) {
        theta_tab$set_upper[k] <- cand$bound
        refined_n <<- refined_n + 1L
      } else if (side == "min" && cand$bound < theta_tab$set_lower[k]) {
        theta_tab$set_lower[k] <- cand$bound
        refined_n <<- refined_n + 1L
      }
    }
  }
  theta_tab
}

# per-coefficient interval of the joint identified set at one tau (the shared
# coef_interval_tables recipe); a row is certified only when both sides are
# finite and feasibility-valid, i.e. status "bounded"
bounds_at_tau <- function(tau) {
  it <- coef_interval_tables(
    set_id_mean_eq$gamma, tau, set_id_mean_eq$moments,
    set_id_mean_eq$beta1r, set_id_mean_eq$beta2r
  )
  it$theta <- refine_theta_intervals(tau, it$theta)
  tab <- rbind(it$beta1, it$theta)
  data.frame(
    tau = tau, coef = tab$coef, lower = tab$set_lower, upper = tab$set_upper,
    certified = tab$status == "bounded"
  )
}

# rows recovered from the stored tables rather than re-solved: the tau = 0
# closed-form point (lower = upper) and the baseline-slack intervals, which
# set_id_mean_eq.R already computed with the same solvers
tables <- rbind(set_id_mean_eq$beta1_table, set_id_mean_eq$theta_table)
stored_rows <- rbind(
  data.frame(
    tau = 0, coef = tables$coef, lower = tables$point, upper = tables$point,
    certified = !is.na(tables$point)
  ),
  data.frame(
    tau = set_id_mean_eq$tau_baseline, coef = tables$coef,
    lower = tables$set_lower, upper = tables$set_upper,
    certified = tables$status == "bounded"
  )
)

# solved tau grid, strictly inside (0, tau*): tau = 0 and the baseline come
# from the stored rows, and the tau* endpoint is excluded (the width diverges
# right at the transition, crushing every facet's scale)
tau_grid <- seq(0, set_id_mean_eq$tau_star, length.out = 25)
tau_grid <- tau_grid[tau_grid > 0 & tau_grid < set_id_mean_eq$tau_star]
bounds_df <- rbind(stored_rows, do.call(rbind, lapply(tau_grid, bounds_at_tau)))

# uncertified rows (unbounded or unreliable sides, expected near tau*) are
# dropped, truncating each coefficient's band where certification ends
plot_df <- bounds_df[bounds_df$certified, ]
plot_df$coef <- factor(plot_df$coef, levels = c(beta_coefs, theta_coefs))

ref_lines <- data.frame(
  tau = set_id_mean_eq$tau_baseline,
  line = sprintf("baseline tau = %.2g", set_id_mean_eq$tau_baseline)
)
bounds_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(tau)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    fill = "#2a78d6", alpha = 0.35
  ) +
  ggplot2::geom_line(ggplot2::aes(y = lower), color = "#2a78d6", linewidth = 0.4) +
  ggplot2::geom_line(ggplot2::aes(y = upper), color = "#2a78d6", linewidth = 0.4) +
  ggplot2::geom_vline(
    data = ref_lines, ggplot2::aes(xintercept = tau, linetype = line),
    color = "grey35", linewidth = 0.35
  ) +
  ggplot2::facet_wrap(~coef, scales = "free_y", ncol = length(beta_coefs)) +
  ggplot2::labs(x = expression(tau), y = NULL, linetype = NULL) +
  ggplot2::theme(legend.position = "bottom")

grDevices::pdf(file.path(out_dir, "set_id_bounds_tau.pdf"), width = 10, height = 5.5)
print(bounds_plot)
grDevices::dev.off()

cat(
  "set-id bounds-by-tau figure:", length(unique(bounds_df$tau)),
  "tau values in [0,", paste0(signif(max(bounds_df$tau), 3), "];"),
  sum(!bounds_df$certified), "uncertified coefficient-tau rows dropped;",
  refined_n, "sides extended by warm-start refinement\n"
)

rm(
  theta_coefs, beta_coefs, solve_theta_bound_from, seed_theta, warm,
  refined_n, refine_theta_intervals, bounds_at_tau, tables, stored_rows,
  tau_grid, bounds_df, plot_df, ref_lines, bounds_plot
)
