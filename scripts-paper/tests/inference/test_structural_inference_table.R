#!/usr/bin/env Rscript
# Structural-inference display rows, their publication gating, and the two
# panels the combined inference template renders, on a small synthetic result.
# Run from root:
#   Rscript scripts-paper/tests/inference/test_structural_inference_table.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis.R"))
paper_source_once(paper_path("support", "structural_inference", "rows.R"))
paper_source_once(paper_path("support", "structural_inference", "panels.R"))
paper_source_once(paper_path("support", "latex", "structural_var_inference.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# Synthetic result ----------------------------------------------------------
# one expected, one news and one return PC; the coefficient axis is every
# (panel, term) at tau = 0 and at each displayed tau
settings <- list(taus = c(0.05, 0.1, 0.2), x = "x1", y2 = "n1", x_var = "v1")
terms <- data.frame(
  panel = c("mean", "mean", "mean", "variance", "variance"),
  term = c("(Intercept)", "x1", "n1", "(Intercept)", "v1"),
  point = c(0.7957, -0.0123, 0.0456, -1.2502, 0.1858)
)
frame <- do.call(rbind, lapply(c(0, settings$taus), function(tau) {
  data.frame(
    coef = sprintf("%s|%s|tau=%.2f", terms$panel, terms$term, tau),
    panel = terms$panel, term = terms$term, tau = tau,
    lower = terms$point - tau, upper = terms$point + tau,
    lower_status = "bounded", upper_status = "bounded",
    lower_reason = "finite", upper_reason = "finite",
    lower_geometry = "box", upper_geometry = "box", approximation = "box",
    n_attempted = NA_integer_, n_failed = NA_integer_
  )
}))
zero <- frame$tau == 0
frame$upper[zero] <- frame$lower[zero]
counts <- list(n_valid_point = 90L, n_non_failed = 95L)
point_summary <- data.frame(
  coef = frame$coef[zero], point = frame$lower[zero],
  statistic = c(13.57, -0.13, 2.08, -5.60, 1.14),
  p_value_normal = c(1e-6, 0.9, 0.03, 1e-6, 0.25), p_value = 0.5,
  publication_allowed = TRUE, reason = "reported", failure_reason = NA_character_,
  counts
)
interval_summary <- data.frame(
  coef = frame$coef[!zero], ci_lower = frame$lower[!zero] - 0.1,
  ci_upper = frame$upper[!zero] + 0.1, publication_allowed = TRUE,
  reason = "reported", failure_reason = NA_character_, n_lower = 90L, n_upper = 91L,
  n_common = 89L, n_non_failed_lower = 95L, n_non_failed_upper = 96L
)
reference <- data.frame(
  panel = terms$panel, term = terms$term, estimate = terms$point * 1.01,
  statistic = c(14.52, 0.07, 4.11, -5.65, 1.78),
  p_value = c(1e-6, 0.94, 1e-4, 1e-6, 0.07), n_obs = c(256, 256, 256, 255, 255),
  r_squared = c(0.0985, 0.0985, 0.0985, NA, NA), available = TRUE, reason = "reported"
)
result <- list(
  prepared = list(n_obs = 256L, variance_n_obs = 255L, settings = settings),
  reference = list(frame = reference),
  bootstrap = list(
    full = list(frame = frame), point_summary = point_summary,
    intervals = list(summary = interval_summary),
    failure_gates = data.frame(
      coef = frame$coef, failed_share_lower = 0, failed_share_upper = 0.01
    )
  )
)
raises <- function(expr, pattern) {
  message <- tryCatch(
    {
      force(expr)
      ""
    },
    error = conditionMessage
  )
  grepl(pattern, message)
}

# Rows ----------------------------------------------------------------------
rows <- structural_inference_rows(result)
check("rows put the reference first, then one row per coefficient", {
  nrow(rows) == 25L && identical(rows$column[1:5], rep("reference", 5L)) &&
    setequal(rows$column, c("reference", "tau0", "tau0.05", "tau0.10", "tau0.20"))
})
check("tau = 0 rows carry the point, its statistic and both p-values", {
  r <- rows[rows$column == "tau0", ]
  identical(r$estimate, terms$point) && identical(r$statistic, point_summary$statistic) &&
    identical(r$p_value, point_summary$p_value_normal) && all(r$p_value_empirical == 0.5)
})
check("each panel reports its own sample size", {
  identical(unique(rows$n_obs[rows$panel == "mean"]), 256) &&
    identical(unique(rows$n_obs[rows$panel == "variance"]), 255)
})
gated <- result
gated$bootstrap$point_summary$publication_allowed[2L] <- FALSE
gated$bootstrap$intervals$summary$publication_allowed[1L] <- FALSE
gated_rows <- structural_inference_rows(gated)
check("a gated point keeps its estimate and loses its statistic", {
  r <- gated_rows[gated_rows$coef %in% frame$coef[2L], ]
  is.finite(r$estimate) && is.na(r$statistic) && is.na(r$p_value)
})
check("a gated interval loses its confidence bounds but keeps the set", {
  r <- gated_rows[gated_rows$coef %in% interval_summary$coef[1L], ]
  is.finite(r$lower) && is.na(r$ci_lower) && is.na(r$ci_upper)
})
unbounded <- result
unbounded$bootstrap$full$frame$upper_status[6L] <- "unbounded"
unbounded$bootstrap$full$frame$upper[6L] <- NA_real_
check("a proven unbounded side arrives as infinity", {
  r <- structural_inference_rows(unbounded)
  identical(r$upper[r$coef %in% frame$coef[6L]], Inf)
})
check("a bounded side without a finite value stops the rows", {
  broken <- result
  broken$bootstrap$full$frame$lower[7L] <- NA_real_
  raises(structural_inference_rows(broken), "upper|lower endpoint contradicts")
})
check("a summary off the coefficient axis stops the rows", {
  broken <- result
  broken$bootstrap$point_summary <- broken$bootstrap$point_summary[-1L, ]
  raises(structural_inference_rows(broken), "point summary does not cover")
})

# Panels and TeX ------------------------------------------------------------
panels <- structural_inference_panels(rows, settings)
check("panels carry the renderer's labels, headers and five columns", {
  identical(panels$mean$row_labels, c(
    "$b_0$", "", "$b_{1,E}$", "", "$b_{1,N}$", "", "$R^2$", "$N$"
  )) &&
    identical(panels$variance$rows, c("$\\theta_0$", "", "$\\theta_{1,R}$", "", "$R^2$", "$N$")) &&
    length(panels$mean$columns) == 5L && identical(panels$mean$headers, c(
    "OLS", "$\\tau{=}0$", "$\\tau{=}0.05$", "$\\tau{=}0.1$", "$\\tau{=}0.2$"
  ))
})
check("point cells round to three decimals with stars from their p-values", {
  identical(panels$mean$columns[[1L]][1:2], c("0.804***", "(14.52)")) &&
    identical(panels$mean$columns[[2L]][3:4], c("$-0.012$", "($-0.13$)")) &&
    identical(panels$variance$columns[[2L]][1L], "$-1.250$***")
})
check("set cells pair the identified set with its confidence interval", {
  identical(panels$mean$columns[[3L]][1:2], c(
    "$[0.746,\\,0.846]$", "$(0.646,\\,0.946)$"
  ))
})
check("summary rows keep the OLS R^2 and the per-panel sample sizes", {
  identical(vapply(panels$mean$columns, `[[`, "", 7L), c("0.10", rep("--", 4L))) &&
    identical(vapply(panels$variance$columns, `[[`, "", 5L), rep("--", 5L)) &&
    identical(vapply(panels$variance$columns, `[[`, "", 6L), rep("255", 5L))
})
tex <- paper_structural_var_inference_table(panels$mean, panels$variance)
check("the template renders both panels with x10 scaling", {
  any(tex == paste0(
    "$b_0$ & \\shiftedestimate{8}{04\\text{***}} & \\shiftedlastestimate{7}{96\\text{***}}",
    " & \\llap{$[$}{7}.46\\intervalcomma & {8}.46\\rlap{$]$}",
    " & \\llap{$[$}{6}.96\\intervalcomma & {8}.96\\rlap{$]$}",
    " & \\llap{$[$}{5}.96\\intervalcomma & {9}.96\\rlap{$]$} \\\\[-2pt]"
  )) && any(tex == paste0(
    "$R^2$ & \\shiftedestimate{0}{10} & \\estimatesummary{--} & \\intervalsummary{--}",
    " & \\intervalsummary{--} & \\intervalsummary{--} \\\\"
  )) && sum(grepl("^\\$N\\$ & ", tex)) == 2L
})
check("a withheld statistic or interval stops the table and names the cell", {
  raises(
    structural_inference_panels(gated_rows, settings),
    "mean\\|x1\\|tau0, mean\\|\\(Intercept\\)\\|tau0.05"
  )
})
check("an unbounded side stops the table", {
  raises(structural_inference_panels(structural_inference_rows(unbounded), settings), "tau0.05")
})

.test$finish()
