# Stored family fields are immutable and raw attribute changes are quarantined.

protocol <- paper_mbb_protocol()
family <- paper_mbb_index_family(
  4L, 11L, 3L, 1L, protocol$family_names[["primary"]]
)
family_raw <- serialize(family, NULL, version = 3)
mutation_errors <- c(
  tryCatch(
    {
      family$family <- protocol$family_names[["compatibility"]]
      NA_character_
    },
    error = conditionMessage
  ),
  tryCatch(
    {
      family[["seed"]] <- 2L
      NA_character_
    },
    error = conditionMessage
  ),
  tryCatch(
    {
      family[1L] <- list(protocol$family_names[["compatibility"]])
      NA_character_
    },
    error = conditionMessage
  ),
  tryCatch(
    {
      names(family) <- rev(names(family))
      NA_character_
    },
    error = conditionMessage
  )
)
check(
  "all public family field replacement routes are rejected",
  identical(
    mutation_errors,
    rep("paper MBB index families are immutable", 4L)
  ) &&
    identical(serialize(family, NULL, version = 3), family_raw)
)

local({
  changed <- unclass(family)
  attr(changed, "comment") <- "untrusted"
  changed <- structure(
    changed,
    class = protocol$family_class
  )
  callbacks <- 0L
  hash_error <- tryCatch(
    paper_mbb_index_sha(changed),
    error = conditionMessage
  )
  execution_error <- tryCatch(
    paper_run_indexed_draws(changed, function(index, draw_id) {
      callbacks <<- callbacks + 1L
      sum(index)
    }),
    error = conditionMessage
  )
  check(
    "extra family attributes fail hashing and execution before callbacks",
    identical(hash_error, "invalid paper MBB index family") &&
      identical(execution_error, "invalid paper MBB index family") &&
      identical(callbacks, 0L)
  )
})

check(
  "bounded MBB protocol fields remain available to stage consumers",
  identical(
    protocol$rng_kind,
    c("Mersenne-Twister", "Inversion", "Rejection")
  ) &&
    identical(protocol$resampler, "circular_mbb") &&
    identical(
      protocol$family_names,
      c(
        primary = "primary",
        sensitivity = "doubled_block_sensitivity",
        compatibility = "compatibility"
      )
    ) &&
    identical(protocol$family_class, "paper_mbb_index_family") &&
    identical(names(family), protocol$family_fields)
)

metadata_changes <- list(
  family = protocol$family_names[["sensitivity"]],
  seed = 2L,
  block_length = 4L
)
metadata_errors <- vapply(names(metadata_changes), function(name) {
  changed <- unclass(family)
  changed[[name]] <- metadata_changes[[name]]
  changed <- structure(changed, class = protocol$family_class)
  tryCatch(
    paper_run_indexed_draws(changed, function(index, draw_id) sum(index)),
    error = conditionMessage
  )
}, character(1))
check(
  "family labels, seeds, and block lengths are authenticated",
  identical(
    unname(metadata_errors),
    rep(
      "paper MBB family metadata is not authenticated",
      length(metadata_changes)
    )
  )
)

rm(
  family, family_raw, metadata_changes, metadata_errors,
  mutation_errors, protocol
)
