# Post-selection block-moments integration tests (ACM-shaped data:
# per-block refit, window isolation, evaluation states). Run from
# package root:
#   Rscript scripts/utils/tests/test_postsel_block_moments.R
suppressMessages(source("scripts/utils/common_settings.R"))
source("scripts/utils/postsel_split_utils.R")

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

# Synthetic ACM-shaped data frame exercising the real-data block path
make_fake_acm <- function(n_rows, seed) {
  set.seed(seed)
  fake <- data.frame(
    date = as.Date("1990-03-31") + round(91.3125 * seq_len(n_rows))
  )
  for (m in HETID_CONSTANTS$DEFAULT_ACM_MATURITIES) {
    fake[[paste0(YIELD_PREFIX, m)]] <- rnorm(n_rows, mean = 3)
    fake[[paste0(TP_PREFIX, m)]] <- rnorm(n_rows, sd = 0.3)
  }
  fake[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]] <- rnorm(n_rows)
  for (p in seq_len(HETID_CONSTANTS$MAX_N_PCS)) {
    fake[[paste0(HETID_CONSTANTS$PC_PREFIX, p)]] <- rnorm(n_rows)
  }
  fake
}

fake <- make_fake_acm(48L, seed = 202)
fb <- split_block_rows(nrow(fake), prop = 0.5, gap = 2L)
m_s <- block_moments(fake, fb$s_rows)
m_e <- block_moments(fake, fb$e_rows)
check(
  "block moments lose exactly the lead observation per block",
  m_s$n_obs == length(fb$s_rows) - 1L &&
    m_e$n_obs == length(fb$e_rows) - 1L
)

# Window isolation: corrupting the other window must not move a
# block's moments (selection can never touch evaluation data and
# vice versa, including through the refit first stage)
num_cols <- setdiff(names(fake), "date")
fake_bad_e <- fake
fake_bad_e[fb$e_rows, num_cols] <-
  fake_bad_e[fb$e_rows, num_cols] * 100 + 7
check(
  "selection-window moments ignore evaluation-window data",
  identical(
    m_s$moments, block_moments(fake_bad_e, fb$s_rows)$moments
  )
)
fake_bad_s <- fake
fake_bad_s[fb$s_rows, num_cols] <-
  fake_bad_s[fb$s_rows, num_cols] * 100 + 7
check(
  "evaluation-window moments ignore selection-window data",
  identical(
    m_e$moments, block_moments(fake_bad_s, fb$e_rows)$moments
  )
)

# Refit proof for the first stage of W1: block residuals differ from
# slicing the full-sample fit (the coefficients move with the window)
w1_block <- compute_w1_residuals(
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, data = fake[fb$s_rows, ]
)$residuals
w1_full <- compute_w1_residuals(
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, data = fake
)$residuals
check(
  "per-block w1 first stage is refit, not sliced from the full sample",
  !isTRUE(all.equal(w1_block, w1_full[seq_along(w1_block)]))
)

# Refit proof for the W2 first stages: per-block residuals and
# coefficients must differ from the full-sample fits. The
# corruption-invariance checks above already rule out
# sliced-from-full wholesale; this is the direct witness for D2's
# claim that EVERY first stage is refit per block.
pc_cols <- paste0(HETID_CONSTANTS$PC_PREFIX, 1:6)
yield_cols <- paste0(
  YIELD_PREFIX, HETID_CONSTANTS$DEFAULT_ACM_MATURITIES
)
tp_cols <- paste0(TP_PREFIX, HETID_CONSTANTS$DEFAULT_ACM_MATURITIES)
w2_block <- compute_w2_residuals(
  yields = fake[fb$s_rows, yield_cols],
  term_premia = fake[fb$s_rows, tp_cols],
  maturities = DEFAULT_ID_MATURITIES,
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  pcs = as.matrix(fake[fb$s_rows, pc_cols])
)
w2_full <- compute_w2_residuals(
  yields = fake[, yield_cols],
  term_premia = fake[, tp_cols],
  maturities = DEFAULT_ID_MATURITIES,
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  pcs = as.matrix(fake[, pc_cols])
)
first_block <- w2_block$residuals[[1]]
check(
  "per-block w2 first stages are refit, not sliced from the full sample",
  !isTRUE(all.equal(
    first_block,
    w2_full$residuals[[1]][seq_along(first_block)]
  )) &&
    !isTRUE(all.equal(w2_block$coefficients, w2_full$coefficients))
)

# Fixed-weights evaluation carries the full three-state detail
gamma_test <- get_baseline_gamma("vfci", n_components = 3L)
ev <- evaluate_lambda_set(gamma_test, rep(0.2, 3), m_s$moments)
vocab <- c("bounded", "unbounded", "no-certified-bound")
check(
  "evaluated arm carries per-component three-state rows",
  nrow(ev$bounds) == 3L &&
    all(ev$bounds$state_lower %in% vocab) &&
    all(ev$bounds$state_upper %in% vocab) &&
    ev$system_state %in% vocab
)
check(
  "total width is a number exactly when the system is bounded",
  identical(
    identical(ev$system_state, "bounded"),
    is.finite(ev$total_width)
  )
)
ar <- arm_rows(
  "probe", "vfci_fixed", "none", "s", m_s$n_obs, ev,
  paste0("maturity_", seq_len(3))
)
check(
  "arm rows carry windows, sizes, and states without collapsing",
  nrow(ar) == 3L &&
    all(c(
      "arm", "weights", "sel_window", "eval_window", "t_eval",
      "state_lower", "state_upper", "state", "system_state"
    ) %in% names(ar)) &&
    all(ar$t_eval == m_s$n_obs)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
