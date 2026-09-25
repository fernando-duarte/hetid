#' Generate circular moving-block bootstrap indices
#'
#' @param sample_size Positive integer length of the aligned estimation sample.
#' @param block_length Positive integer block length, capped at `sample_size`.
#' @param n_draws Positive integer number of index vectors.
#' @details Starts are sampled uniformly with replacement. Each block wraps at
#'   the final row; concatenated blocks are truncated to `sample_size`. This
#'   function consumes the caller's random-number stream. The caller chooses
#'   the seed, RNG kind, block length, and already date-aligned input sample.
#' @return A list of `n_draws` numeric index vectors, each of length `sample_size`.
#' @export
#' @examples
#' set.seed(23)
#' circular_mbb_indices(10, 3, 2)
circular_mbb_indices <- function(sample_size, block_length, n_draws = 1L) {
  assert_scalar_integer_in_range(sample_size, "sample_size", 1, .Machine$integer.max)
  assert_scalar_integer_in_range(block_length, "block_length", 1, .Machine$integer.max)
  assert_scalar_integer_in_range(n_draws, "n_draws", 1, .Machine$integer.max)
  block_length <- min(block_length, sample_size)
  n_blocks <- ceiling(sample_size / block_length)
  lapply(seq_len(n_draws), function(draw_id) {
    starts <- sample.int(sample_size, n_blocks, replace = TRUE)
    unlist(lapply(starts, function(start) {
      (as.double(start) + 0:(block_length - 1) - 1) %% sample_size + 1
    }))[seq_len(sample_size)]
  })
}
