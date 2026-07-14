# Cell formatters shared by the log-variance tables (the benchmark
# log_var_eq_table.R and the combined estimator panels), moved verbatim from
# the benchmark table script so both render identical cells: the "--"
# non-finite formatter, the status-aware identified-set cell, and the
# coefficient/t-statistic row interleaver. Definitions only.

# NA and non-finite render "--" (a non-finite tau = 0 point means the Lewbel
# point sits on a residual-zero hyperplane; the driver's min_abs_eps_point
# diagnostic surfaces it)
fmt <- function(x) ifelse(!is.finite(x), "--", sprintf("%.3f", x))
# an unreliable or upstream-propagated (NA-endpoint) cell renders its status
# word; certified one-sided divergence renders a half-infinite range; a
# degenerate interval (point-identified) is left blank as in the structural
# table
set_cell <- function(lo, hi, status) {
  ifelse(
    status == "unreliable" | is.na(lo) | is.na(hi), status,
    ifelse(
      is.infinite(lo) & is.infinite(hi), "unbounded",
      ifelse(
        is.infinite(lo), sprintf("$(-\\infty,\\,%.3f]$", hi),
        ifelse(
          is.infinite(hi), sprintf("$[%.3f,\\,\\infty)$", lo),
          ifelse(lo == hi, "", sprintf("$[%.3f,\\,%.3f]$", lo, hi))
        )
      )
    )
  )
}

# coefficient rows interleaved with the OLS t-statistic rows (blank in the
# identification columns)
interleave <- function(a, b) as.vector(rbind(a, b))
