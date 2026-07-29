# Status-strip geometry for the log-variance bounds-by-tau figure
# (bounds_by_tau_frame.R). The strip discloses per-tau endpoint status, so a
# tile must never be wide enough to paint over its neighbour: on the refined
# grid a single fixed width spans several steps, and because the strip is built
# in row order the higher tau draws last and hides the downgrade.

paper_source_once(paper_path("config", "tau_grid.R"))
paper_source_once(paper_path(
  "log_variance", "figures", "bounds_by_tau_frame.R"
))

st_tau_star <- 0.41455078125
st_taus <- c(
  0, PAPER_ANALYSIS_CONTRACT$tau$display,
  paper_bounds_tau_grid(st_tau_star)
)
st_coefs <- c("(Intercept)", "l.pc1")
st_rows <- do.call(rbind, lapply(st_taus, function(tt) {
  data.frame(
    tau = tt, coef = st_coefs, lower = -0.1 - tt, upper = 0.1 + tt,
    lower_status = "bounded", upper_status = "bounded",
    stringsAsFactors = FALSE
  )
}))
st_strip <- logvar_bounds_tau_frame(st_rows, st_tau_star)$strip
st_sub <- st_strip[st_strip$coef == "l.pc1", ]
st_sub <- st_sub[order(st_sub$tau), ]
st_nn <- vapply(
  seq_len(nrow(st_sub)),
  function(i) min(abs(st_sub$tau[i] - st_sub$tau[-i])),
  numeric(1)
)

check(
  "the strip keeps one tile per sampled tau",
  nrow(st_strip) == nrow(st_rows)
)
check(
  "no tile is wider than the distance to its nearest neighbour",
  all(st_sub$w <= st_nn)
)
check(
  "the retired fixed width would have overlapped a neighbour",
  st_tau_star / 30 > min(st_nn)
)

# a facet collapsed onto one tau has no local spacing to read, so it falls back
# to the tuned default (carrying the same clearance factor the spaced case
# uses) rather than to a zero-width tile
st_one <- st_rows[st_rows$tau == st_rows$tau[[1L]], ]
st_one_strip <- logvar_bounds_tau_frame(st_one, st_tau_star)$strip
check(
  "a single-tau facet falls back to a positive width within the default",
  all(st_one_strip$w > 0) && all(st_one_strip$w <= st_tau_star / 30)
)

rm(
  st_tau_star, st_taus, st_coefs, st_rows, st_strip, st_sub, st_nn,
  st_one, st_one_strip
)
