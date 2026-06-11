# Post-selection split utility tests (unit level: block arithmetic,
# state vocabulary, solver classification). Run from package root:
#   Rscript scripts/utils/tests/test_postsel_split_utils.R
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

# Block arithmetic: deterministic, contiguous, disjoint, exact for
# the data-sized case (242 quarterly rows, the merged dataset size)
b <- split_block_rows(242L, prop = 0.5, gap = 4L)
check(
  "data-sized split yields the exact contiguous blocks",
  identical(b$s_rows, 1:119) &&
    identical(b$gap_rows, 120:123) &&
    identical(b$e_rows, 124:242)
)
b0 <- split_block_rows(10L, prop = 0.5, gap = 0L)
check(
  "zero-gap split yields adjacent blocks covering every row",
  identical(b0$s_rows, 1:5) && identical(b0$e_rows, 6:10) &&
    length(b0$gap_rows) == 0L
)
check(
  "too-small blocks are rejected loudly",
  inherits(
    try(split_block_rows(4L, gap = 2L), silent = TRUE), "try-error"
  )
)

# Three-state vocabulary mapping and fail-closed precedence
check(
  "side states map solver pairs to the three-word vocabulary",
  identical(
    side_state(
      c(TRUE, FALSE, TRUE, FALSE), c(TRUE, TRUE, FALSE, FALSE)
    ),
    c(
      "bounded", "unbounded", "no-certified-bound",
      "no-certified-bound"
    )
  )
)
check(
  "worst state applies fail-closed precedence",
  identical(worst_state(c("bounded", "unbounded")), "unbounded") &&
    identical(
      worst_state(c("bounded", "no-certified-bound", "unbounded")),
      "no-certified-bound"
    ) &&
    identical(worst_state("bounded"), "bounded")
)

# Hand-built single-constraint systems with known outcomes exercise
# the classification against the live solver deterministically
quad_disc <- list(
  d_i = 1, A_i = list(matrix(1)), b_i = list(0), c_i = -1
)
bb <- solve_all_profile_bounds(quad_disc)
check(
  "hand-built disc classifies bounded on both sides",
  identical(
    side_state(bb$bounded_lower, bb$valid_lower), "bounded"
  ) &&
    identical(
      side_state(bb$bounded_upper, bb$valid_upper), "bounded"
    ) &&
    abs(bb$lower + 1) < 1e-4 && abs(bb$upper - 1) < 1e-4
)
quad_halfline <- list(
  d_i = 1, A_i = list(matrix(0)), b_i = list(1), c_i = 0
)
hb <- solve_all_profile_bounds(quad_halfline)
check(
  "hand-built half-line is certified unbounded on the open side",
  identical(
    side_state(hb$bounded_lower, hb$valid_lower), "unbounded"
  ) &&
    identical(
      side_state(hb$bounded_upper, hb$valid_upper), "bounded"
    )
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
