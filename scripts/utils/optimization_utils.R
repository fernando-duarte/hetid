# Optimization Utilities -- inner profile bounds and outer Gamma optimization

symmetrize_quadratic_system <- function(quad_system) {
  q <- quad_system$quadratic
  for (idx in seq_along(q$A_i)) {
    ai <- q$A_i[[idx]]
    quad_system$quadratic$A_i[[idx]] <-
      (ai + t(ai)) / 2
  }
  quad_system
}

solve_profile_bound <- function(quadratic,
                                component_index,
                                direction = c("min", "max"),
                                theta_start = NULL) {
  direction <- match.arg(direction)
  n_con <- length(quadratic$A_i)
  dim_theta <- ncol(quadratic$A_i[[1]])

  e_k <- numeric(dim_theta)
  e_k[component_index] <- 1
  sign_mult <- if (direction == "min") 1 else -1

  obj_fn <- function(theta) sign_mult * sum(e_k * theta)
  grad_fn <- function(theta) sign_mult * e_k

  # Constraints: -(theta' A_i theta + b_i' theta + c_i) >= 0
  # nloptr requires hin(x) >= 0 for feasibility
  constraint_fn <- function(theta) {
    vapply(seq_len(n_con), function(i) {
      ai <- quadratic$A_i[[i]]
      bi <- quadratic$b_i[[i]]
      ci <- quadratic$c_i[i]
      -(drop(t(theta) %*% ai %*% theta) +
        sum(bi * theta) + ci)
    }, numeric(1))
  }

  constraint_jac <- function(theta) {
    jac <- matrix(0, nrow = n_con, ncol = dim_theta)
    for (i in seq_len(n_con)) {
      ai <- quadratic$A_i[[i]]
      bi <- quadratic$b_i[[i]]
      jac[i, ] <- -(2 * drop(ai %*% theta) + bi)
    }
    jac
  }

  if (is.null(theta_start)) {
    theta_start <- rep(0, dim_theta)
  }

  result <- tryCatch(nloptr::slsqp(
    x0 = theta_start, fn = obj_fn, gr = grad_fn,
    hin = constraint_fn, hinjac = constraint_jac,
    control = list(xtol_rel = 1e-8, maxeval = 1000)
  ), error = function(e) {
    list(
      par = theta_start, value = NA_real_,
      convergence = -99L, iter = 0L,
      message = e$message
    )
  })

  bound_val <- if (direction == "max") -result$value else result$value
  list(
    bound = bound_val, theta = result$par,
    convergence = result$convergence,
    iterations = result$iter
  )
}

solve_all_profile_bounds <- function(quadratic,
                                     theta_start = NULL) {
  n_comp <- ncol(quadratic$A_i[[1]])
  results <- lapply(seq_len(n_comp), function(k) {
    lo <- solve_profile_bound(
      quadratic, k, "min", theta_start
    )
    hi <- solve_profile_bound(
      quadratic, k, "max", theta_start
    )
    data.frame(
      component = k,
      lower = lo$bound, upper = hi$bound,
      width = hi$bound - lo$bound,
      converged_lower = lo$convergence >= 0,
      converged_upper = hi$convergence >= 0,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, results)
}

compute_total_width <- function(bounds_tbl) {
  sum(bounds_tbl$width)
}

pack_gamma <- function(gamma) as.vector(gamma)

unpack_gamma <- function(par, n_pcs, n_components) {
  matrix(par, nrow = n_pcs, ncol = n_components)
}

normalize_gamma_columns <- function(gamma) {
  col_norms <- sqrt(colSums(gamma^2))
  col_norms[col_norms == 0] <- 1
  sweep(gamma, 2, col_norms, FUN = "/")
}

objective_gamma_only <- function(par, moments, tau,
                                 n_pcs, n_components,
                                 maturities = 1:8) {
  gamma <- unpack_gamma(par, n_pcs, n_components)
  gamma <- normalize_gamma_columns(gamma)
  tryCatch(
    {
      quad_sys <- build_quadratic_system(
        gamma, tau, moments, maturities
      )
      quad_sys <- symmetrize_quadratic_system(quad_sys)
      bounds_tbl <- solve_all_profile_bounds(
        quad_sys$quadratic
      )
      if (any(!bounds_tbl$converged_lower) ||
        any(!bounds_tbl$converged_upper)) {
        return(1e6)
      }
      compute_total_width(bounds_tbl)
    },
    error = function(e) {
      1e6
    }
  )
}

run_gamma_optimization <- function(gamma_start,
                                   moments,
                                   tau,
                                   n_starts = 10,
                                   seed = SEED,
                                   maturities = 1:8) {
  set.seed(seed)
  n_pcs <- nrow(gamma_start)
  n_comp <- ncol(gamma_start)
  par_start <- pack_gamma(gamma_start)

  objective_start <- objective_gamma_only(
    par_start, moments, tau,
    n_pcs, n_comp, maturities
  )

  # Generate starting points: primary + perturbed
  starts <- vector("list", n_starts)
  starts[[1]] <- par_start
  for (s in seq_len(n_starts - 1) + 1) {
    perturbed <- gamma_start +
      rnorm(length(gamma_start))
    perturbed <- normalize_gamma_columns(perturbed)
    starts[[s]] <- pack_gamma(perturbed)
  }

  # Optimize from each start
  obj_fn <- function(par) {
    objective_gamma_only(
      par, moments, tau, n_pcs, n_comp, maturities
    )
  }
  all_results <- lapply(seq_len(n_starts), function(s) {
    tryCatch(nloptr::slsqp(
      x0 = starts[[s]], fn = obj_fn,
      control = list(xtol_rel = 1e-6, maxeval = 500)
    ), error = function(e) {
      list(
        par = starts[[s]], value = 1e6,
        convergence = -99L, iter = 0L,
        message = e$message
      )
    })
  })

  # Select best result
  objectives <- vapply(
    all_results, function(r) r$value, numeric(1)
  )
  best_idx <- which.min(objectives)
  best <- all_results[[best_idx]]
  gamma_opt <- normalize_gamma_columns(
    unpack_gamma(best$par, n_pcs, n_comp)
  )

  list(
    gamma_optimized = gamma_opt,
    objective_start = objective_start,
    objective_final = best$value,
    all_results = all_results,
    best_index = best_idx
  )
}
