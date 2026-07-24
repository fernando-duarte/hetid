# Canonical stored index families for the paper moving-block bootstrap.

.paper_mbb_stop <- function(message) stop(simpleError(message, call = NULL))

.paper_mbb_design <- function(n_draws, sample_size, block_length) {
  stopifnot(
    length(n_draws) == 1L, is.finite(n_draws), n_draws >= 1L,
    n_draws == as.integer(n_draws),
    length(sample_size) == 1L, is.finite(sample_size), sample_size >= 1L,
    length(block_length) == 1L, is.finite(block_length), block_length >= 1L
  )
  list(
    n_draws = as.integer(n_draws),
    sample_size = as.integer(sample_size),
    block_length = as.integer(block_length)
  )
}

.paper_mbb_seed_record <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  suppressWarnings(as.integer(unclass(seed)[1L]))
}

.paper_mbb_family_value <- function(design, seed, family, indices, rng_state) {
  protocol <- paper_mbb_protocol()
  draft <- stats::setNames(
    list(
      family, design$n_draws, design$sample_size, design$block_length, seed,
      protocol$rng_kind, NA_character_, NA_character_, indices, rng_state
    ),
    protocol$family_fields
  )
  class(draft) <- protocol$family_class
  draft$index_sha256 <- paper_mbb_index_sha(draft)
  draft$post_index_rng_sha256 <- paper_sha256_object(rng_state)
  draft
}

.paper_mbb_index_family_validate <- function(index_family, authenticate = TRUE) {
  protocol <- paper_mbb_protocol()
  scalar_integer <- function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) && value >= 1L
  }
  seed_ok <- is.null(index_family$seed) || (
    is.integer(index_family$seed) && length(index_family$seed) == 1L &&
      !is.na(index_family$seed)
  )
  indices_ok <- is.list(index_family$indices) &&
    length(index_family$indices) == index_family$n_draws &&
    all(vapply(index_family$indices, function(index) {
      is.double(index) && length(index) == index_family$sample_size &&
        all(is.finite(index)) && all(index == as.integer(index)) &&
        all(index >= 1 & index <= index_family$sample_size)
    }, logical(1)))
  sha_ok <- function(value) {
    identical(value, NA_character_) || (
      is.character(value) && length(value) == 1L &&
        grepl("^[0-9a-f]{64}$", value)
    )
  }
  valid <- identical(class(index_family), protocol$family_class) &&
    identical(names(index_family), protocol$family_fields) &&
    index_family$family %in% unname(protocol$family_names) &&
    scalar_integer(index_family$n_draws) && scalar_integer(index_family$sample_size) &&
    scalar_integer(index_family$block_length) && seed_ok &&
    identical(index_family$rng_kind, protocol$rng_kind) &&
    sha_ok(index_family$index_sha256) && sha_ok(index_family$post_index_rng_sha256) &&
    indices_ok && is.integer(index_family$draw_rng_state) &&
    length(index_family$draw_rng_state) > 1L
  if (!isTRUE(valid)) .paper_mbb_stop("invalid paper MBB index family")
  if (authenticate && !identical(
    index_family$index_sha256, paper_mbb_index_sha(index_family)
  )) {
    .paper_mbb_stop("paper MBB index family hash does not authenticate indices")
  }
  if (authenticate && !identical(
    index_family$post_index_rng_sha256,
    paper_sha256_object(index_family$draw_rng_state)
  )) {
    .paper_mbb_stop("paper MBB post-index RNG state is not authenticated")
  }
  invisible(TRUE)
}

paper_mbb_index_sha <- function(index_family) {
  .paper_mbb_index_family_validate(index_family, authenticate = FALSE)
  paper_sha256_object(index_family$indices)
}

paper_mbb_index_family <- function(
  n_draws, sample_size, block_length, seed, family
) {
  design <- .paper_mbb_design(n_draws, sample_size, block_length)
  protocol <- paper_mbb_protocol()
  stopifnot(
    is.character(family), length(family) == 1L, !is.na(family),
    family %in% unname(protocol$family_names)
  )
  ambient <- .paper_mbb_rng_capture()
  on.exit(.paper_mbb_rng_restore(ambient), add = TRUE)
  do.call(RNGkind, as.list(protocol$rng_kind))
  set.seed(seed)
  indices <- lapply(seq_len(design$n_draws), function(draw_id) {
    mbb_index(design$sample_size, design$block_length)
  })
  family <- .paper_mbb_family_value(
    design, .paper_mbb_seed_record(seed), family, indices, .Random.seed
  )
  .paper_mbb_index_family_validate(family)
  family
}
