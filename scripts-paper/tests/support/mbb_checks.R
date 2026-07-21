# Circular moving-block bootstrap: index semantics, the automatic block-length
# convention, and the pre-drawn/RNG-owning/chunked-parallel draw runner.

# Moving blocks retain their requested length, range, and circular adjacency.
set.seed(91)
idx <- mbb_index(11L, 4L)
check("moving-block draw has the requested length", length(idx) == 11L)
check("moving-block draw stays inside the sample", all(idx %in% seq_len(11L)))
check(
  "moving-block draw preserves adjacency inside each block",
  all(diff(idx[1:4]) %% 11L == 1L) && all(diff(idx[5:8]) %% 11L == 1L) &&
    all(diff(idx[9:11]) %% 11L == 1L)
)
set.seed(91)
check("moving-block draw is controlled by caller RNG", identical(idx, mbb_index(11L, 4L)))

# Seed 1 forces a start (9) inside the last bl-1 positions, so its block
# wraps modulo nn -- the defining behavior of the circular resampler.
set.seed(1)
idx_wrap <- mbb_index(11L, 4L)
wrap_blocks <- split(seq_along(idx_wrap), ceiling(seq_along(idx_wrap) / 4L))
check(
  "moving-block draw wraps modulo nn within a block",
  any(diff(idx_wrap[1:4]) < 0L) &&
    all(vapply(wrap_blocks, function(pos) {
      all(diff(idx_wrap[pos]) %% 11L == 1L)
    }, logical(1)))
)

# An oversized block covers the whole series in one block: any rotation of
# 1:nn is legitimate, including the identity rotation.
set.seed(203)
idx_full <- mbb_index(5L, 9L)
check(
  "oversized block returns some rotation of the full sample",
  length(idx_full) == 5L && setequal(idx_full, 1:5) &&
    all(diff(idx_full) %% 5L == 1L)
)

check(
  "automatic block length matches the T=256 convention",
  identical(paper_mbb_block_len(256L), 10L)
)
check(
  "automatic block length matches the T=208 convention",
  identical(paper_mbb_block_len(208L), 9L)
)
check(
  "automatic block length returns an integer",
  is.integer(paper_mbb_block_len(256L))
)

# The runner pins Mersenne-Twister for the duration of the call and restores
# whatever ambient generator kind was active, even when a callback stops.
default_kind <- RNGkind()
RNGkind("Wichmann-Hill")
ambient_kind <- RNGkind()
paper_run_mbb_draws(
  n_draws = 3L, sample_size = 8L, block_length = 3L, seed = 21L,
  draw = function(index, draw_id) sum(index)
)
check(
  "RNG kind is restored after a successful run",
  identical(RNGkind(), ambient_kind)
)
kind_run <- paper_run_mbb_draws(
  n_draws = 3L, sample_size = 8L, block_length = 3L, seed = 21L,
  draw = function(index, draw_id) sum(index)
)
check(
  "returned rng_kind reports Mersenne-Twister regardless of ambient kind",
  identical(ambient_kind[1], "Wichmann-Hill") &&
    identical(kind_run$rng_kind[1], "Mersenne-Twister")
)
run_error <- tryCatch(
  {
    paper_run_mbb_draws(
      n_draws = 3L, sample_size = 8L, block_length = 3L, seed = 21L,
      draw = function(index, draw_id) sum(index),
      progress = function(draw_id, n_draws, started_at) stop("boom mid-draw")
    )
    "no error"
  },
  error = function(error) conditionMessage(error)
)
check(
  "RNG kind is restored after a run that errors mid-draws",
  identical(run_error, "boom mid-draw") && identical(RNGkind(), ambient_kind)
)
do.call(RNGkind, as.list(default_kind))

# Chunked mclapply dispatch must reproduce the serial output exactly: indices
# are pre-drawn, so chunking or forking cannot perturb the deterministic draw.
if (.Platform$OS.type == "windows") {
  skip("chunked parallel draws match the serial draws", "no fork on windows")
  skip("progress fires under cores = 2 dispatch", "no fork on windows")
} else {
  parallel_draw <- function(index, draw_id) sum(index) + draw_id
  run_serial <- paper_run_mbb_draws(
    n_draws = 12L, sample_size = 20L, block_length = 5L,
    seed = 77L, cores = 1L, draw = parallel_draw
  )
  run_parallel <- paper_run_mbb_draws(
    n_draws = 12L, sample_size = 20L, block_length = 5L,
    seed = 77L, cores = 2L, draw = parallel_draw
  )
  check(
    "chunked parallel draws match the serial draws",
    identical(run_serial$draws, run_parallel$draws) &&
      identical(run_serial$indices, run_parallel$indices)
  )
  progress_hits <- integer()
  run_progress <- paper_run_mbb_draws(
    n_draws = 12L, sample_size = 20L, block_length = 5L,
    seed = 77L, cores = 2L, draw = parallel_draw,
    progress = function(n_done, n_draws, started_at) {
      progress_hits <<- c(progress_hits, n_done)
    }
  )
  check(
    "progress fires under cores = 2 dispatch and the run completes",
    identical(progress_hits, c(2L, 4L, 6L, 8L, 10L, 12L)) &&
      identical(run_progress$draws, run_serial$draws)
  )
}

mbb_run_a <- paper_run_mbb_draws(
  n_draws = 4L,
  sample_size = 11L,
  block_length = 4L,
  truncation = 8L,
  seed = 333L,
  draw = function(index, draw_id) {
    c(draw_id = draw_id, total = sum(index))
  }
)
mbb_run_b <- paper_run_mbb_draws(
  n_draws = 4L,
  sample_size = 11L,
  block_length = 4L,
  truncation = 8L,
  seed = 333L,
  draw = function(index, draw_id) {
    c(draw_id = draw_id, total = sum(index))
  }
)
check(
  "moving-block runner pre-draws a reproducible index stream",
  identical(mbb_run_a$indices, mbb_run_b$indices) &&
    identical(mbb_run_a$draws, mbb_run_b$draws) &&
    all(lengths(mbb_run_a$indices) == 8L)
)
mbb_progress <- integer()
mbb_failed <- paper_run_mbb_draws(
  n_draws = 3L,
  sample_size = 9L,
  block_length = 3L,
  seed = 444L,
  draw = function(index, draw_id) {
    if (draw_id == 2L) stop("fixture draw failed")
    sum(index)
  },
  progress = function(draw_id, n_draws, started_at) {
    mbb_progress <<- c(mbb_progress, draw_id)
  }
)
check(
  "moving-block runner captures failures and sequential progress",
  mbb_failed$n_failed == 1L &&
    identical(mbb_failed$draws[[2L]], "fixture draw failed") &&
    identical(mbb_progress, 1:3)
)
rm(mbb_run_a, mbb_run_b, mbb_progress, mbb_failed)
