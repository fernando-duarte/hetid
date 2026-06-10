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

.mode_block <- function(res, label) {
  rows <- vapply(seq_len(nrow(res$tau_stars)), function(i) {
    sprintf(
      "    %-24s tau* = %s",
      paste0(res$tau_stars$gamma[i], ":"),
      .fmt_tau_star(res$tau_stars$tau_star[i], res$tau_stars$capped[i])
    )
  }, character(1))
  c(sprintf("  %s", label), rows)
}
