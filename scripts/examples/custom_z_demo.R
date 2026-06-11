# Demonstration: the full generalized-instrument workflow on custom Z.
# Not part of the default pipeline; nothing here writes into
# scripts/output. Run from the package root:
#   Rscript scripts/examples/custom_z_demo.R
suppressPackageStartupMessages(library(hetid))

data("variables", package = "hetid")
keep <- complete.cases(
  variables[, c(
    "gr1.pcecc96", paste0("pc", 1:4), "gspc_vol", "gs10", "tb3ms"
  )]
)
df <- variables[keep, ]

# Primitive Z: three PCs plus realized equity volatility, then a
# nonlinear transform of the first PC.
z_raw <- as.matrix(df[, c("pc1", "pc2", "pc3", "gspc_vol")])
instruments <- build_instrument_matrix(
  z_raw,
  transforms = list(pc1_sq = function(z) z[, "pc1"]^2)
)

# First stage on the same custom regressors via the exog escape hatch.
w1 <- compute_w1_residuals(data = df, exog = z_raw)$residuals

# Toy second stage: two synthetic endogenous columns built from the
# data frame so the demo runs offline end to end.
set.seed(123)
y2 <- cbind(
  df$gs10[-1] - df$gs10[-nrow(df)],
  df$tb3ms[-1] - df$tb3ms[-nrow(df)]
)
w2 <- apply(y2, 2, function(col) {
  residuals(lm(col ~ z_raw[-nrow(z_raw), ]))
})
t_obs <- min(length(w1), nrow(w2))
moments <- compute_identification_moments(
  w1[seq_len(t_obs)], w2[seq_len(t_obs), ],
  instruments[seq_len(t_obs), ]
)

# All instruments separately (the I x J scheme on custom Z)
qs_all <- build_general_quadratic_system(
  separate_instruments_lambda(moments), 0.2, moments
)
cat("separate-instrument constraints:", nrow(qs_all$labels), "\n")

# Two fixed combinations for the first component, one for the second
lambda_fixed <- list(
  cbind(c(1, 0, 0, 0, 0), c(0, 0, 0, 1, 1)),
  matrix(c(1, 1, 1, 0, 0), ncol = 1)
)
qs_fixed <- build_general_quadratic_system(lambda_fixed, 0.2, moments)
print(qs_fixed$labels)

# Optimized weights (sources the pipeline utils for the solver)
source(file.path("scripts", "utils", "common_settings.R"))
opt <- run_lambda_optimization(
  lambda_fixed, moments, 0.2,
  n_starts = 5, seed = 123, maxeval = 200L
)
cat("optimized total width:", opt$objective_final, "\n")

# Per-component instrument subsets: align the per-component sets by
# name, recompute the moments on the union matrix (the support
# indexes ITS columns), zero-pad compact weights onto it, and pass
# the same support to the optimizer's mask so the structural zeros
# survive optimization (off-support entries stay exactly 0.0).
aligned <- align_instrument_sets(
  list(
    instruments[, c("pc1", "pc2", "pc1_sq")],
    instruments[, c("pc3", "gspc_vol")]
  ),
  n_components = 2
)
moments_sets <- compute_identification_moments(
  w1[seq_len(t_obs)], w2[seq_len(t_obs), ],
  aligned$instruments[seq_len(t_obs), ]
)
lambda_subsets <- lambda_from_support(
  aligned$support,
  list(matrix(1, 3, 1), matrix(1, 2, 1)),
  j_total = ncol(aligned$instruments)
)
qs_sets <- build_general_quadratic_system(
  lambda_subsets, 0.2, moments_sets
)
cat("per-component-subset constraints:", nrow(qs_sets$labels), "\n")
opt_masked <- run_lambda_optimization(
  lambda_subsets, moments_sets, 0.2,
  n_starts = 3, seed = 123, maxeval = 100L,
  support = aligned$support
)
cat(
  "masked optimized width:", opt_masked$objective_final,
  "| off-support exact zeros:",
  all(opt_masked$lambda_optimized[[1]][-aligned$support[[1]], ] == 0) &&
    all(opt_masked$lambda_optimized[[2]][-aligned$support[[2]], ] == 0),
  "\n"
)
# Opt-in whitening: identical constraint set (weights enter only
# through direction), different search coordinates -- the optimizer
# walks mu = chol(Var(Z)) %*% lambda, the spec's variance-normalized
# weights, sub-blocked to each component's supported instruments.
# Reported weights stay in original coordinates, Euclidean-
# normalized; the applied transform is echoed under $whitening. A
# numerical reparameterization, not a statistical improvement.
opt_whitened <- run_lambda_optimization(
  lambda_subsets, moments_sets, 0.2,
  n_starts = 3, seed = 123, maxeval = 100L,
  support = aligned$support,
  whiten = list(z = aligned$instruments[seq_len(t_obs), ])
)
cat(
  "masked+whitened width:", opt_whitened$objective_final,
  "| transform recorded:", !is.null(opt_whitened$whitening),
  "| off-support exact zeros:",
  all(opt_whitened$lambda_optimized[[1]][
    -aligned$support[[1]],
  ] == 0),
  "\n"
)

# Membership probe across every constraint (hin <= 0 means inside);
# a grid where max(...) > 0 everywhere is an empty estimated set
check_all <- make_system_checker(qs_fixed$quadratic)
cat("origin inside fixed-combo set:", max(check_all(c(0, 0))) <= 0, "\n")
