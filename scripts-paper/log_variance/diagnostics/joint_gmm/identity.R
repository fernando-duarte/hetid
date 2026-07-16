# Canonical joint-input identity and qtr-keyed alignment for the joint moment-
# compatibility layer (joint-GMM, logvar-joint-gmm). logvar_joint_input_id hashes the
# ordered (sample_id, qtr, aligned z, gamma, display-tau, mean-system geometry) tuple
# with the house tempfile/tools::md5sum pattern (mirrors logvar_joint_decision_spec_id),
# validating finiteness first so a nonfinite input stops rather than silently poisoning
# the stamp. logvar_joint_align_inputs realigns z to the qtr order by key, never by row
# position, asserting a one-to-one qtr join first. Definitions only; sourced by
# the joint-GMM test entrypoint and the graph-replication driver.

# Stop with a classed joint-identity error mirroring the house structured conditions so
# callers can dispatch on the reason string.
logvar_joint_identity_stop <- function(reason, detail = "") {
  stop(structure(
    class = c(reason, "logvar_joint_identity_error", "error", "condition"),
    list(
      message = if (nzchar(detail)) paste0(reason, ": ", detail) else reason,
      call = NULL
    )
  ))
}

# Flatten one mean-set quadratic system to its canonical numeric content and dimensions,
# so the hash tracks the A_i/b_i/c_i geometry rather than list attributes or names.
logvar_joint_mean_system_payload <- function(system) {
  list(
    a = lapply(system$A_i, function(a) as.numeric(as.matrix(a))),
    a_dim = lapply(system$A_i, function(a) dim(as.matrix(a))),
    b = lapply(system$b_i, as.numeric),
    b_len = vapply(system$b_i, length, integer(1L)),
    c = as.numeric(system$c_i)
  )
}

# Canonical id over the ordered joint inputs. Every numeric element of qtr, z, gamma,
# tau, and each mean system must be finite before hashing; the payload is written with
# the pinned RDS version and hashed with base tools, so identical inputs yield the same
# id and a change in any substantive input changes it.
logvar_joint_input_id <- function(sample_id, qtr, z, gamma, tau, mean_systems) {
  z <- as.matrix(z)
  gamma <- as.matrix(gamma)
  ms_payload <- lapply(mean_systems, logvar_joint_mean_system_payload)
  numeric_parts <- c(
    list(as.numeric(qtr), as.numeric(z), as.numeric(gamma), as.numeric(tau)),
    lapply(ms_payload, function(p) unlist(c(p$a, p$b, p$c), use.names = FALSE))
  )
  for (part in numeric_parts) {
    if (length(part) && any(!is.finite(part))) {
      logvar_joint_identity_stop(
        "nonfinite_joint_input", "every numeric joint input must be finite"
      )
    }
  }
  payload <- list(
    sample_id = as.character(sample_id), qtr = as.numeric(qtr),
    z = as.numeric(z), z_dim = dim(z), gamma = as.numeric(gamma),
    gamma_dim = dim(gamma), tau = as.numeric(tau), mean_systems = ms_payload
  )
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(payload, tmp, version = 3)
  unname(tools::md5sum(tmp))
}

# Realign z (whose rows are keyed by other_qtr) to the qtr order: assert each qtr appears
# exactly once in other_qtr -- a one-to-one match that allows other_qtr to carry extra rows
# the qtr sample dropped (the variance sample is a qtr-subset of the mean sample, so the
# mean instrument is restricted to the variance quarters), then reindex the rows of z with
# match(). Never aligns by row position, so a shuffled input recovers the original order.
logvar_joint_align_inputs <- function(qtr, other_qtr, z) {
  z <- as.matrix(z)
  if (nrow(z) != length(other_qtr)) {
    logvar_joint_identity_stop("misaligned_join", "z rows do not match other_qtr")
  }
  if (anyDuplicated(qtr) || anyDuplicated(other_qtr) || !all(qtr %in% other_qtr)) {
    logvar_joint_identity_stop(
      "misaligned_join", "qtr is not a one-to-one subset of other_qtr"
    )
  }
  z[match(qtr, other_qtr), , drop = FALSE]
}
