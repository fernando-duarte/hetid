# Formatting helpers shared by the tau* report text builder.

.fmt_tau <- function(x, digits = 4) {
  if (!is.finite(x)) "n/a" else formatC(x, format = "f", digits = digits)
}

.fmt_tau_star <- function(tau_star, capped) {
  if (!is.finite(tau_star)) {
    return("n/a")
  }
  base <- .fmt_tau(tau_star)
  if (isTRUE(capped)) paste0(">= ", base, " (search cap reached)") else base
}

.ts_of <- function(res, label) {
  i <- match(label, res$tau_stars$gamma)
  list(
    tau_star = if (is.na(i)) NA_real_ else res$tau_stars$tau_star[i],
    capped = if (is.na(i)) FALSE else res$tau_stars$capped[i]
  )
}

.results_block <- function(res, label) {
  rows <- vapply(seq_len(nrow(res$tau_stars)), function(i) {
    sprintf(
      "    %-24s tau* = %s",
      paste0(res$tau_stars$gamma[i], ":"),
      .fmt_tau_star(res$tau_stars$tau_star[i], res$tau_stars$capped[i])
    )
  }, character(1))
  c(sprintf("  %s", label), rows, .bracket_lines(res))
}

# Certified bracket around tau* for each swept (fixed) gamma: the largest tau
# certified bounded and the smallest tau certified unbounded. Taus in between
# came back "unreliable" -- the solver could not certify either way.
.bracket_lines <- function(res) {
  unlist(lapply(unique(res$sweep$gamma), function(g) {
    sw <- res$sweep[res$sweep$gamma == g, ]
    lb <- suppressWarnings(max(sw$tau[sw$status == "bounded"]))
    fu <- suppressWarnings(min(sw$tau[sw$status == "unbounded"]))
    if (!is.finite(lb) || !is.finite(fu)) {
      return(character(0))
    }
    sprintf(
      "      %s bracket: certified bounded thru %.6f, unbounded from %.6f",
      g, lb, fu
    )
  }))
}
