# Descriptive sensitivity set for the base-R log-variance dynamics gate.
# Definitions only; sourced by gate_record.R. The
# witnesses are the unique finite arg_lower/arg_upper b_N vectors from the
# bounded sides of log_var_eq$schema plus the tau = 0 seed b_seed, each
# feasibility- and sample-identity-rechecked. Evaluating the gate p-values at
# each witness is context only: a witness can never reverse the primary tau = 0
# verdict, and a crossing witness is logged as excluded rather than dropped
# silently to make the diagnostic run. Base R only.

# representation-stable 17-digit key so exact-duplicate witnesses collapse
.logvar_gate_key <- function(b) {
  paste(formatC(as.numeric(b), digits = 17, format = "fg", flag = "#"), collapse = "|")
}

# an empty rows frame carrying the source label, one p-value column per tested
# lag, and the b_N list column, so callers see a stable schema with no witnesses
.logvar_gate_empty_rows <- function(p_names) {
  df <- data.frame(source = character(0), stringsAsFactors = FALSE)
  for (pn in p_names) df[[pn]] <- numeric(0)
  df$b_n <- I(list())
  df
}

# gather the bounded-side witnesses from every per-tau schema, then the seed
.logvar_gate_witnesses <- function(schema, b_seed) {
  wit <- list()
  push <- function(b, src) wit[[length(wit) + 1L]] <<- list(source = src, b = as.numeric(b))
  if (length(schema)) {
    snm <- names(schema)
    if (is.null(snm)) snm <- sprintf("set%d", seq_along(schema))
    for (ti in seq_along(schema)) {
      sc <- schema[[ti]]
      if (is.null(sc) || !nrow(sc)) next
      for (j in seq_len(nrow(sc))) {
        if (isTRUE(sc$lower_status[j] == "bounded")) {
          push(sc$arg_lower[[j]], sprintf("%s:%s:lower", snm[ti], sc$coef[j]))
        }
        if (isTRUE(sc$upper_status[j] == "bounded")) {
          push(sc$arg_upper[[j]], sprintf("%s:%s:upper", snm[ti], sc$coef[j]))
        }
      }
    }
  }
  if (!is.null(b_seed)) push(b_seed, "b_seed")
  if (!length(wit)) {
    return(wit)
  }
  keys <- vapply(wit, function(w) .logvar_gate_key(w$b), character(1))
  wit[!duplicated(keys)]
}

# evaluate the gate p-values at each witness b_N: recheck the sample dimension
# and finiteness, exclude (with reason) any crossing or nonfinite witness, and
# record the tested-lag p-values otherwise
logvar_gate_sensitivity <- function(inputs, proj, schema, b_seed,
                                    tested_lags = c(1L, 4L, 8L), gate_lag = 4L,
                                    alpha = 0.05) {
  p_names <- paste0("p_", .logvar_gate_lag_names(tested_lags))
  empty <- list(
    rows = .logvar_gate_empty_rows(p_names),
    exclusions = data.frame(
      source = character(0), reason = character(0),
      stringsAsFactors = FALSE
    )
  )
  wit <- .logvar_gate_witnesses(schema, b_seed)
  if (!length(wit)) {
    return(empty)
  }
  ord <- .logvar_gate_order(inputs$w1, inputs$w2, inputs$pcr, inputs$qtr, proj)
  k <- ncol(ord$w2)
  rows <- list()
  excl <- list()
  drop_row <- function(src, reason) excl[[length(excl) + 1L]] <<- c(source = src, reason = reason)
  for (w in wit) {
    if (!all(is.finite(w$b)) || length(w$b) != k) {
      drop_row(w$source, "nonfinite_or_dim")
      next
    }
    e_star <- drop(ord$w1 - ord$w2 %*% w$b)
    if (any(e_star == 0)) {
      drop_row(w$source, "crossing")
      next
    }
    r <- .logvar_gate_resid(e_star, ord$pcr, ord$proj)
    if (!all(is.finite(r$xi_hat))) {
      drop_row(w$source, "nonfinite_xi")
      next
    }
    pv <- .logvar_gate_box(r$xi_hat, tested_lags)$p_values
    row <- as.list(stats::setNames(unname(pv), p_names))
    row$source <- w$source
    row$b_n <- list(w$b)
    rows[[length(rows) + 1L]] <- row
  }
  list(
    rows = if (length(rows)) .logvar_gate_bind_rows(rows, p_names) else empty$rows,
    exclusions = if (length(excl)) {
      data.frame(do.call(rbind, excl), row.names = NULL, stringsAsFactors = FALSE)
    } else {
      empty$exclusions
    }
  )
}

# assemble the per-witness p-value rows into one frame with the b_N list column
.logvar_gate_bind_rows <- function(rows, p_names) {
  out <- data.frame(
    source = vapply(rows, `[[`, character(1), "source"), stringsAsFactors = FALSE
  )
  for (pn in p_names) out[[pn]] <- vapply(rows, function(r) r[[pn]], numeric(1))
  out$b_n <- I(lapply(rows, function(r) r$b_n[[1L]]))
  out
}
