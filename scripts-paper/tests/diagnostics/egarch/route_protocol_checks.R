# Protocol-derived EGARCH route prose.

d_nr_alt_lag <- d_nr
d_nr_alt_lag$gate_lag <- 7L
r_nr_alt_lag <- logvar_egarch_route(
  d_nr_alt_lag,
  FALSE,
  NA_character_
)
check(
  "non_reject reason derives its lag label from the decision protocol",
  grepl("lag-7", r_nr_alt_lag$skip_reason, fixed = TRUE)
)
rm(d_nr_alt_lag, r_nr_alt_lag)
