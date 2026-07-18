# LAD sides and domain-failure hooks, split from engine_hooks.R.
# Probe every verified crossing path and fold its classified tail into the
# endpoint state. A coefficient with witnesses but no informative path fails
# closed rather than remaining silently bounded.
logvar_lad_sides_hook <- function(
  w1,
  w2,
  x_mat,
  e_scale_ref,
  control
) {
  function(qs, b_tab, scan, ctx) {
    labels <- colnames(x_mat)
    n_coef <- length(labels)
    st <- list(
      lower_unb = stats::setNames(rep(FALSE, n_coef), labels),
      upper_unb = stats::setNames(rep(FALSE, n_coef), labels),
      informative = stats::setNames(rep(FALSE, n_coef), labels),
      unresolved = character(0),
      closure = list()
    )
    witnesses <- ctx$precheck$info$witnesses
    ctx_g <- logvar_lad_augment_ctx(
      ctx,
      w1,
      w2,
      x_mat,
      e_scale_ref,
      control
    )
    for (wid in names(witnesses)) {
      witness <- witnesses[[wid]]
      for (path_id in seq_len(length(witness$anchors))) {
        classified <- tryCatch(
          logvar_lad_tail_classify(
            logvar_lad_crossing_probe(
              witness,
              path_id,
              ctx_g
            ),
            control
          ),
          error = function(error) {
            logvar_lad_pass_budget(error, NULL)
          }
        )
        st <- logvar_lad_side_update(
          st,
          classified,
          labels,
          wid,
          path_id
        )
      }
    }
    idle <- which(
      !st$informative &
        !st$lower_unb &
        !st$upper_unb
    )
    if (length(witnesses) > 0L && length(idle) > 0L) {
      st$unresolved <- c(
        st$unresolved,
        sprintf("%s:min", labels[idle]),
        sprintf("%s:max", labels[idle])
      )
    }
    list(
      lower_unbounded = st$lower_unb,
      upper_unbounded = st$upper_unb,
      unresolved_endpoints = unique(st$unresolved),
      closure_diagnostics = if (length(st$closure) == 0L) {
        NULL
      } else {
        st$closure
      },
      info = list(
        cross_all = ctx$precheck$info$cross,
        method = "lad_crossing_probe",
        n_witnesses = length(witnesses)
      )
    )
  }
}

# Fold one classified path into the side state.
logvar_lad_side_update <- function(
  st,
  classified,
  labels,
  witness_id,
  path_id
) {
  both <- function(index) {
    c(
      sprintf("%s:min", labels[index]),
      sprintf("%s:max", labels[index])
    )
  }
  if (is.null(classified) || is.null(classified$coef)) {
    st$unresolved <- c(
      st$unresolved,
      unlist(lapply(seq_along(labels), both))
    )
    return(st)
  }
  for (index in seq_along(labels)) {
    current <- classified$coef[[index]]
    if (identical(current$status, "uninformative")) {
      next
    }
    st$informative[index] <- TRUE
    if (identical(
      current$status,
      "persistent_divergent_evidence"
    )) {
      if (identical(current$endpoint, "lower")) {
        st$lower_unb[index] <- TRUE
      } else if (identical(current$endpoint, "upper")) {
        st$upper_unb[index] <- TRUE
      } else {
        st$unresolved <- c(st$unresolved, both(index))
      }
    } else if (identical(current$status, "stable_finite")) {
      st$closure[[length(st$closure) + 1L]] <- list(
        coef = labels[index],
        witness = witness_id,
        path_id = path_id,
        classification = current,
        provenance =
          "one-sided crossing-limit approximation"
      )
    } else {
      st$unresolved <- c(st$unresolved, both(index))
    }
  }
  st
}

# Classify an inner-fit domain failure only when every implicated row belongs to
# the verified crossing census.
logvar_lad_claim_hook <- function(
  w1,
  w2,
  e_scale_ref,
  control
) {
  function(b, fit, precheck, ctx) {
    if (!identical(fit$fit_status, "domain_failure")) {
      return(list(
        claimed = FALSE,
        domain_state = NA_character_,
        reason = "not a domain failure",
        probe_targets = NULL
      ))
    }
    residual <- drop(w1 - w2 %*% b)
    guard <- control$guard_ratio * e_scale_ref
    exact <- which(residual == 0)
    rows <- sort(c(
      exact,
      which(abs(residual) <= guard & residual != 0)
    ))
    census_rows <- if (is.null(precheck)) {
      integer(0)
    } else {
      precheck$info$cross
    }
    on_census <- length(rows) > 0L &&
      all(rows %in% census_rows)
    domain_state <- if (length(exact) > 0L) {
      "exact_domain_failure"
    } else {
      "numerically_unresolved_near_crossing"
    }
    reason <- if (!on_census) {
      "domain failure off the verified census: fail closed"
    } else if (identical(
      domain_state,
      "exact_domain_failure"
    )) {
      paste(
        "certified exclusion at an exact residual zero",
        "on a census crossing"
      )
    } else {
      paste(
        "guarded near-crossing on a census row:",
        "available but numerically unresolved"
      )
    }
    list(
      claimed = on_census,
      domain_state = domain_state,
      reason = reason,
      probe_targets = if (on_census) rows else NULL
    )
  }
}
