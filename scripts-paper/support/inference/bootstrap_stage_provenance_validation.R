bootstrap_stage_family_pair_validate <- function(
  families, family_names, designs, sample_size, seed, rng_kind,
  validator, sha_pattern
) {
  keys <- names(family_names)
  if (!bootstrap_stage_cache_exact_list(families, keys)) {
    return("index family names changed")
  }
  if (!bootstrap_stage_cache_exact_list(designs, keys)) {
    stop("family design contract is malformed", call. = FALSE)
  }
  for (key in keys) {
    validation <- tryCatch(validator(families[[key]]), error = identity)
    if (inherits(validation, "error")) {
      return(paste0(key, " family malformed: ", conditionMessage(validation)))
    }
    design <- designs[[key]]
    expected <- list(
      family = family_names[[key]],
      n_draws = design$n_draws,
      sample_size = sample_size,
      block_length = design$block_length,
      seed = seed,
      rng_kind = rng_kind
    )
    fields <- names(expected)
    same <- vapply(fields, function(field) {
      identical(families[[key]][[field]], expected[[field]])
    }, logical(1))
    if (!all(same)) {
      return(paste0(key, " family changed: ", fields[which(!same)[1L]]))
    }
    for (field in c("index_sha256", "post_index_rng_sha256")) {
      sha <- families[[key]][[field]]
      if (!is.character(sha) || length(sha) != 1L ||
        is.na(sha) || !grepl(sha_pattern, sha)) {
        return(paste0(key, " family hash is invalid: ", field))
      }
    }
  }
  TRUE
}

bootstrap_stage_provenance_record_validate <- function(
  value, fields, expected, sha_fields, sha_pattern
) {
  if (!bootstrap_stage_cache_exact_list(value, fields)) {
    return("provenance fields or class changed")
  }
  if (!bootstrap_stage_cache_exact_list(expected, fields)) {
    stop("provenance expectation contract is malformed", call. = FALSE)
  }
  for (field in sha_fields) {
    sha <- value[[field]]
    if (!is.character(sha) || length(sha) != 1L ||
      is.na(sha) || !grepl(sha_pattern, sha)) {
      return(paste0("provenance hash is invalid: ", field))
    }
  }
  same <- vapply(fields, function(field) {
    identical(value[[field]], expected[[field]])
  }, logical(1))
  if (!all(same)) {
    return(paste0("provenance changed: ", fields[which(!same)[1L]]))
  }
  TRUE
}
