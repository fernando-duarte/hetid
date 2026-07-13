# Tests for set_id_bootstrap_core.R: collection and diagnostics are exercised
# on hand-built draw lists (pure functions, no solver work); the one-draw
# evaluator is exercised on a small simulated heteroskedastic system.
# Run from the package root:
# Rscript scripts/utils/tests/test_set_id_bootstrap_core.R
source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")
source("scripts/utils/set_id_inference.R")
source("scripts/utils/set_id_bootstrap_core.R")

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

# hand-built raw draws: one good, one failed (error message string)
spec2 <- list(coefs = c("a", "b"), taus = c(0.05, 0.2))
good <- list(
  point = c(1, 2), point_ok = TRUE,
  bounds = list(
    list(lower = c(0.9, 1.8), upper = c(1.1, 2.2), status = c("bounded", "bounded")),
    list(lower = c(NA, 1.5), upper = c(NA, 2.5), status = c("unbounded", "bounded"))
  ),
  tau_star = 0.3, capped = FALSE
)
collected <- set_id_boot_collect(list(good, "solver exploded"), spec2)
check(
  "point draws keep coefficient columns",
  identical(dim(collected$point_draws), c(2L, 2L))
)
check(
  "failed draw becomes an all-NA endpoint row",
  all(is.na(collected$endpoint_draws[[1]]$lower[2, ]))
)
check(
  "failed draw is statused as failed",
  all(collected$endpoint_draws[[1]]$status[2, ] == "failed")
)
check(
  "per-tau statuses survive collection",
  identical(
    unname(collected$endpoint_draws[[2]]$status[1, ]),
    c("unbounded", "bounded")
  )
)
check("failure accounting", collected$n_failed == 1L &&
  collected$failure_causes[["solver exploded"]] == 1L)
check("tau* vector aligned", identical(collected$tau_star_draws, c(0.3, NA_real_)))

# diagnostics: one row per coefficient-tau with counts and a reason
tabs <- list(
  list(
    beta1 = data.frame(
      coef = "a", set_lower = 0.9, set_upper = 1.1,
      status = "bounded", stringsAsFactors = FALSE
    ),
    theta = data.frame(
      coef = "b", set_lower = 1.8, set_upper = 2.2,
      status = "bounded", stringsAsFactors = FALSE
    )
  ),
  list(
    beta1 = data.frame(
      coef = "a", set_lower = NA_real_, set_upper = NA_real_,
      status = "unbounded", stringsAsFactors = FALSE
    ),
    theta = data.frame(
      coef = "b", set_lower = 1.5, set_upper = 2.6,
      status = "bounded", stringsAsFactors = FALSE
    )
  )
)
inference <- lapply(seq_along(spec2$taus), function(j) {
  st <- tabs[[j]]
  endpoint_inference(
    collected$endpoint_draws[[j]]$lower, collected$endpoint_draws[[j]]$upper,
    rbind(st$beta1, st$theta),
    alpha = 0.10, min_reps = 1L
  )
})
diag_df <- set_id_boot_diagnostics(collected, inference, tabs, spec2$taus)
check("diagnostics has one row per coefficient-tau", nrow(diag_df) == 4L)
check(
  "diagnostics counts add up to the draw count",
  all(rowSums(diag_df[, c("n_bounded", "n_unbounded", "n_unreliable", "n_failed")]) == 2L)
)
check(
  "uncertified full-sample rows carry a reason",
  diag_df$reason[diag_df$coef == "a" & diag_df$tau == 0.2] != "reported"
)

# one-draw evaluator on a simulated heteroskedastic system (I = 2, null mode)
set.seed(7)
n <- 160
z <- exp(rnorm(n, 0, 0.4))
z <- z - mean(z)
e2 <- cbind(rnorm(n) * (1 + 0.9 * z), rnorm(n) * (1 + 0.7 * z))
x <- rnorm(n)
y1 <- 0.5 + 0.3 * x + 0.2 * e2[, 1] - 0.1 * e2[, 2] + rnorm(n, 0, 0.2)
dat <- data.frame(y1 = y1, x = x, w1 = e2[, 1], w2 = e2[, 2], z = z)
spec_sim <- list(
  coefs = c("(Intercept)", "x", "w1", "w2"),
  gamma = matrix(1, 1, 2), taus = c(0.05, 0.9),
  tau_grid = unique(c(seq(0, 0.99, by = 0.05), 0.99)),
  y1_col = "y1", x_cols = "x", y2_cols = c("w1", "w2"),
  z_col = "z", impose_null = TRUE
)
d <- set_id_boot_draw(dat, spec_sim)
check(
  "draw returns a finite point under full rank",
  d$point_ok && all(is.finite(d$point))
)
check(
  "every display tau is evaluated (no tau* skip)",
  length(d$bounds) == 2L &&
    all(vapply(d$bounds, function(b) length(b$status) == 4L, logical(1)))
)
check(
  "statuses come from the allowed set",
  all(unlist(lapply(d$bounds, `[[`, "status")) %in%
    c("bounded", "unbounded", "unreliable"))
)
check(
  "finite sides are ordered",
  all(mapply(
    function(b) all(b$lower <= b$upper, na.rm = TRUE), d$bounds
  ))
)
check(
  "tau* is inside the admissible range",
  is.finite(d$tau_star) && d$tau_star >= 0 && d$tau_star < 1
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_set_id_bootstrap_core failures")
