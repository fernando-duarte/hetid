# Zero denotes proved distinct forms; NA denotes an unproved relation.
lv_log_relation <- function(a, b) {
  if (any((a == 0) != (b == 0))) {
    return(0)
  }
  active <- which(a != 0)
  if (!length(active)) {
    return(1)
  }
  multiplier <- lv_log_power_relation(a[active], b[active])
  if (!is.na(multiplier)) {
    return(multiplier)
  }
  pivot <- active[which.max(abs(a[active]))]
  for (j in active) {
    difference <- lv_log_dot(c(a[pivot], -a[j]), c(b[j], b[pivot]))
    if (difference[1L] > 0 || difference[2L] < 0) {
      return(0)
    }
  }
  NA_real_
}

lv_log_power_relation <- function(a, b) {
  left <- vapply(a, lv_log_dyadic, numeric(2))
  right <- vapply(b, lv_log_dyadic, numeric(2))
  exponents <- right[2L, ] - left[2L, ]
  signs <- sign(right[1L, ]) * sign(left[1L, ])
  if (all(abs(left[1L, ]) == abs(right[1L, ])) &&
    length(unique(exponents)) == 1L && length(unique(signs)) == 1L) {
    multiplier <- signs[1L] * 2^exponents[1L]
    return(if (is.finite(multiplier) && multiplier != 0) multiplier else NA_real_)
  }
  NA_real_
}

lv_log_inverse_bound <- function(design, projection) {
  p <- ncol(design)
  gram <- matrix(vector("list", p * p), p, p)
  for (i in seq_len(p)) {
    for (j in seq_len(p)) gram[[i, j]] <- lv_log_dot(design[, i], design[, j])
  }
  inverse <- tcrossprod(projection)
  if (any(!is.finite(inverse))) {
    return(NULL)
  }
  errors <- numeric(p)
  for (i in seq_len(p)) {
    entries <- lapply(seq_len(p), function(j) {
      product <- lv_log_sum(lapply(seq_len(p), function(k) {
        lv_log_op(rep(inverse[i, k], 2L), gram[[k, j]], "*")
      }))
      lv_log_op(rep(as.numeric(i == j), 2L), product, "-")
    })
    errors[i] <- lv_log_norm(entries)
  }
  rho <- max(errors)
  if (!is.finite(rho) || rho >= 1) {
    return(NULL)
  }
  error_norm <- max(vapply(seq_len(p), function(i) {
    lv_log_dot(abs(inverse[i, ]), rep(1, p))[2L]
  }, numeric(1)))
  denominator <- lv_log_op(c(1, 1), rep(rho, 2L), "-")
  multiplier <- lv_log_op(rep(error_norm, 2L), denominator, "/")[2L]
  if (!is.finite(multiplier)) {
    return(NULL)
  }
  list(gram = gram, multiplier = multiplier)
}

lv_log_group_weights <- function(prep, rows) {
  design <- cbind(1, prep$x_centered)
  projection <- prep$projection
  coefs <- vapply(
    rows, function(group) rowSums(projection[, group, drop = FALSE]),
    numeric(nrow(projection))
  )
  coefs <- matrix(coefs, nrow(projection), length(rows))
  dimnames(coefs) <- list(rownames(projection), NULL)
  signs <- matrix(NA_real_, nrow(coefs), ncol(coefs), dimnames = dimnames(coefs))
  verified <- lv_log_inverse_bound(design, projection)
  if (is.null(verified)) {
    return(list(weights = coefs, signs = signs))
  }
  centered <- vapply(seq_len(ncol(prep$x_centered)), function(j) {
    identical(lv_log_exact(matrix(prep$x_centered[, j], ncol = 1L)), 0)
  }, logical(1))
  for (g in seq_along(rows)) {
    rhs <- lapply(seq_len(ncol(design)), function(j) {
      lv_log_dot(design[rows[[g]], j], rep(1, length(rows[[g]])))
    })
    normal_product <- lv_log_matvec(verified$gram, lapply(coefs[, g], rep, times = 2L))
    residual <- Map(function(a, b) lv_log_op(a, b, "-"), rhs, normal_product)
    error_norm <- max(vapply(residual, function(value) max(abs(value)), numeric(1)))
    eta <- lv_log_op(rep(error_norm, 2L), rep(verified$multiplier, 2L), "*")[2L]
    for (j in seq_len(nrow(coefs))) {
      enclosure <- lv_log_op(rep(coefs[j, g], 2L), c(-eta, eta), "+")
      if (enclosure[1L] > 0) signs[j, g] <- 1
      if (enclosure[2L] < 0) signs[j, g] <- -1
    }
    zero_slopes <- all(centered) && all(vapply(seq_len(ncol(prep$x_centered)), function(j) {
      identical(lv_log_exact(matrix(prep$x_centered[rows[[g]], j], ncol = 1L)), 0)
    }, logical(1)))
    if (zero_slopes) {
      coefs[-1L, g] <- 0
      signs[-1L, g] <- 0
    }
  }
  list(weights = coefs, signs = signs)
}

lv_set_logols_groups <- function(prep) {
  affine <- cbind(prep$w1, -prep$w2)
  rows <- list()
  factors <- rep(1, nrow(affine))
  ambiguous <- integer(0)
  for (i in seq_len(nrow(affine))) {
    matched <- FALSE
    for (g in seq_along(rows)) {
      representative <- rows[[g]][1L]
      relation <- lv_log_relation(affine[representative, ], affine[i, ])
      if (is.na(relation)) ambiguous <- union(ambiguous, c(representative, i))
      if (is.na(relation) || relation == 0) next
      rows[[g]] <- c(rows[[g]], i)
      factors[i] <- relation
      matched <- TRUE
      break
    }
    if (!matched) rows[[length(rows) + 1L]] <- i
  }
  coefs <- lv_log_group_weights(prep, rows)
  c(list(rows = rows, factors = factors, ambiguous = sort(ambiguous)), coefs)
}

lv_log_group_sides <- function(groups, crossing, labels) {
  active <- vapply(groups$rows, function(rows) any(rows %in% crossing), logical(1))
  signs <- groups$signs[, active, drop = FALSE]
  uncertain <- apply(is.na(signs), 1L, any)
  list(
    lower_unbounded = apply(signs > 0, 1L, any, na.rm = TRUE),
    upper_unbounded = apply(signs < 0, 1L, any, na.rm = TRUE),
    crossing = crossing,
    unresolved_endpoints = if (any(uncertain)) {
      c(paste0(labels[uncertain], ":min"), paste0(labels[uncertain], ":max"))
    } else {
      character(0)
    }
  )
}
