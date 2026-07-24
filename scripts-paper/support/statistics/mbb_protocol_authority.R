# One owner for the paper moving-block bootstrap protocol.

paper_mbb_protocol <- local({
  control <- list(scale = 1.5, exponent = 1 / 3)
  block_rule <- function() {
    sprintf("ceiling(%g*T^(1/%g))", control$scale, 1 / control$exponent)
  }
  block_length <- function(t_obs) {
    as.integer(ceiling(control$scale * t_obs^control$exponent))
  }
  protocol <- list(
    rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
    resampler = "circular_mbb",
    family_names = c(
      primary = "primary",
      sensitivity = "doubled_block_sensitivity",
      compatibility = "compatibility"
    ),
    family_class = "paper_mbb_index_family",
    family_fields = c(
      "family", "n_draws", "sample_size", "block_length", "seed",
      "rng_kind", "index_sha256", "post_index_rng_sha256",
      "family_sha256", "indices", "draw_rng_state"
    ),
    block_rule = block_rule,
    block_length = block_length
  )
  lockEnvironment(environment(), bindings = TRUE)
  function() protocol
})

paper_mbb_block_rule <- function() paper_mbb_protocol()$block_rule()

paper_mbb_block_len <- function(t_obs) paper_mbb_protocol()$block_length(t_obs)
