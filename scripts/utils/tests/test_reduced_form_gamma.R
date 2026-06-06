# Tests for get_reduced_form_gamma. Run from package root:
#   Rscript scripts/utils/tests/test_reduced_form_gamma.R
# Trap-proof: checks the UN-normalized magnitude/direction (normalization would
# mask the residual-orthogonality trap by rescaling any noise to unit norm).
suppressMessages(library(nloptr))
source("scripts/utils/optimization_utils.R") # normalize_gamma_columns
# get_reduced_form_gamma lives in factor_utils.R; source it without the heavy
# common_settings (it only needs base R + normalize_gamma_columns).
source("scripts/utils/factor_utils.R")

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

n_mat <- 9L
# Known, well-conditioned levels coefficients (9 x 4) + intercept column.
set.seed(7)
b_slopes <- matrix(runif(n_mat * 4, 0.2, 1.0), n_mat, 4)
coefs <- cbind(`(Intercept)` = runif(n_mat), b_slopes)
colnames(coefs) <- c("(Intercept)", paste0("pc", 1:4))
loadings <- matrix(runif(n_mat * 3, -1, 1), n_mat, 3)
mats <- seq_len(n_mat)
facs <- 1:3

expected_unnorm <- t(b_slopes) %*% loadings[mats, facs, drop = FALSE]

g_unnorm <- get_reduced_form_gamma(coefs, loadings, mats, facs, normalize = FALSE)
check(
  "uses levels slopes, drops intercept (matches t(B) %*% loadings)",
  max(abs(g_unnorm - expected_unnorm)) < 1e-12
)
check("dims 4 x 3", all(dim(g_unnorm) == c(4, 3)))
check("rank 3", qr(g_unnorm)$rank == 3L)
check(
  "un-normalized magnitude is real signal (O(0.1)-O(10), not ~0)",
  max(abs(g_unnorm)) > 1e-2
)

g_norm <- get_reduced_form_gamma(coefs, loadings, mats, facs, normalize = TRUE)
check("normalized columns are unit-norm", max(abs(sqrt(colSums(g_norm^2)) - 1)) < 1e-10)
# direction preserved (cosine ~ 1 per column)
cos_col <- vapply(1:3, function(j) {
  sum(g_unnorm[, j] * g_norm[, j]) /
    (sqrt(sum(g_unnorm[, j]^2)) * sqrt(sum(g_norm[, j]^2)))
}, numeric(1))
check("normalize preserves column direction", all(abs(cos_col - 1) < 1e-10))

# TRAP: residual-orthogonality gives ~machine-zero slopes -> must ERROR
coefs_trap <- cbind(`(Intercept)` = runif(n_mat), matrix(1e-19, n_mat, 4))
colnames(coefs_trap) <- c("(Intercept)", paste0("pc", 1:4))
trapped <- tryCatch(
  {
    get_reduced_form_gamma(coefs_trap, loadings, mats, facs)
    FALSE
  },
  error = function(e) grepl("residual", conditionMessage(e), ignore.case = TRUE)
)
check("residual-trap (~0 slopes) is rejected with a clear error", trapped)

# row-mismatch guard
mismatch <- tryCatch(
  {
    get_reduced_form_gamma(coefs, loadings, 1:5, facs)
    FALSE
  },
  error = function(e) grepl("rows", conditionMessage(e))
)
check("row-count mismatch errors", mismatch)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
