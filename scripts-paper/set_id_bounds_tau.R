# Identified intervals for every coefficient of the structural consumption-
# growth equation as a function of the slack tau: on a grid over [0, tau*],
# the exact per-coefficient range of the joint identified set from
# set_id_mean_eq.R (profile bounds for the news coefficients, linear-
# functional bounds beta1(theta) = beta1R - beta2R' theta for the design
# coefficients), with the closed-form tau = 0 point as the left endpoint.
# Writes set_id_bounds_tau.pdf to scripts-paper/output/.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")

theta_coefs <- set_id_mean_eq$theta_table$coef
beta_coefs <- set_id_mean_eq$beta1_table$coef

# per-coefficient interval of the joint identified set at one tau; a row is
# certified only when both sides are finite and feasibility-valid
bounds_at_tau <- function(tau) {
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  tb <- solve_all_profile_bounds(qs)
  beta_rows <- do.call(rbind, lapply(beta_coefs, function(p) {
    fmin <- solve_linear_functional_bound(qs, set_id_mean_eq$beta2r[, p], "min")
    fmax <- solve_linear_functional_bound(qs, set_id_mean_eq$beta2r[, p], "max")
    data.frame(
      tau = tau, coef = p,
      lower = set_id_mean_eq$beta1r[[p]] - fmax$bound,
      upper = set_id_mean_eq$beta1r[[p]] - fmin$bound,
      certified = fmin$bounded && fmax$bounded && fmin$valid && fmax$valid
    )
  }))
  theta_rows <- data.frame(
    tau = tau, coef = theta_coefs, lower = tb$lower, upper = tb$upper,
    certified = tb$bounded_lower & tb$bounded_upper &
      tb$valid_lower & tb$valid_upper
  )
  rbind(beta_rows, theta_rows)
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
# right at the transition, crushing every facet's scale; the vline marks it)
tau_grid <- seq(0, set_id_mean_eq$tau_star, length.out = 25)
tau_grid <- tau_grid[tau_grid > 0 & tau_grid < set_id_mean_eq$tau_star]
bounds_df <- rbind(stored_rows, do.call(rbind, lapply(tau_grid, bounds_at_tau)))

# uncertified rows (unbounded or unreliable sides, expected near tau*) are
# dropped, truncating each coefficient's band where certification ends
plot_df <- bounds_df[bounds_df$certified, ]
plot_df$coef <- factor(plot_df$coef, levels = c(beta_coefs, theta_coefs))

ref_lines <- data.frame(
  tau = c(set_id_mean_eq$tau_baseline, set_id_mean_eq$tau_star),
  line = c(
    sprintf("baseline tau = %.2g", set_id_mean_eq$tau_baseline),
    sprintf(
      "tau* = %.3g%s", set_id_mean_eq$tau_star,
      if (set_id_mean_eq$tau_star_capped) " (capped)" else ""
    )
  )
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
  sum(!bounds_df$certified), "uncertified coefficient-tau rows dropped\n"
)

rm(
  theta_coefs, beta_coefs, bounds_at_tau, tables, stored_rows, tau_grid,
  bounds_df, plot_df, ref_lines, bounds_plot
)
