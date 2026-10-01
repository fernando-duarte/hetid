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

# Execution consumes one stored schedule and reports callback failures/progress.
family <- paper_mbb_index_family(12L, 20L, 5L, 77L, "primary")
parallel_draw <- function(index, draw_id) sum(index) + draw_id
serial <- paper_run_indexed_draws(family, parallel_draw)
check(
  "indexed execution passes the exact stored index objects to callbacks",
  identical(serial$indices, family$indices) && identical(
    paper_run_indexed_draws(family, function(index, draw_id) index)$draws,
    family$indices
  )
)
check(
  "index construction is reproducible independently of execution",
  identical(family, paper_mbb_index_family(12L, 20L, 5L, 77L, "primary"))
)
progress_hits <- integer()
failed <- paper_run_indexed_draws(
  family,
  function(index, draw_id) {
    if (draw_id == 2L) stop("fixture draw failed")
    sum(index)
  },
  progress = function(draw_id, n_draws, started_at) {
    progress_hits <<- c(progress_hits, draw_id)
  }
)
check(
  "indexed execution captures failures and sequential progress",
  failed$n_failed == 1L &&
    identical(failed$draws[[2L]], "fixture draw failed") &&
    identical(failed$failed, seq_len(12L) == 2L) &&
    identical(progress_hits, seq_len(12L))
)
custom <- paper_run_indexed_draws(
  family, function(index, draw_id) list(ok = draw_id != 2L),
  is_failure = function(value) !value$ok
)
check("custom failure classification counts structured draw results", custom$n_failed == 1L)
invalid_callbacks <- 0L
invalid <- tryCatch(
  paper_run_indexed_draws(family, function(...) {
    invalid_callbacks <<- invalid_callbacks + 1L
  }, cores = 1.5),
  error = conditionMessage
)
check(
  "invalid executor controls fail before invoking callbacks",
  is.character(invalid) && identical(invalid_callbacks, 0L)
)
if (.Platform$OS.type == "windows") {
  skip("chunked parallel draws match serial draws and report progress", "no fork on windows")
} else {
  progress_hits <- integer()
  forked <- paper_run_indexed_draws(
    family, parallel_draw,
    cores = 2L,
    progress = function(n_done, n_draws, started_at) {
      progress_hits <<- c(progress_hits, n_done)
    }
  )
  check(
    "chunked parallel draws match serial draws and report progress",
    identical(forked$draws, serial$draws) &&
      identical(forked$indices, serial$indices) &&
      identical(progress_hits, c(2L, 4L, 6L, 8L, 10L, 12L))
  )
  forked_fail <- paper_run_indexed_draws(family, function(index, draw_id) {
    if (draw_id == 2L) stop("fixture draw failed")
    sum(index)
  }, cores = 2L)
  check(
    "forked callback failures retain their draw positions and counts",
    identical(forked_fail$draws, failed$draws) &&
      identical(forked_fail$failed, failed$failed) && forked_fail$n_failed == 1L
  )
}
rm(family, parallel_draw, serial, progress_hits, failed, custom, invalid, invalid_callbacks)
