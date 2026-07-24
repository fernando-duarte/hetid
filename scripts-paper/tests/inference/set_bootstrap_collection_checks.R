check(
  "box seed is the midpoint of a finite box",
  identical(
    logvar_box_seed(list(set_lower = c(-1, 2), set_upper = c(1, 4))),
    c(0, 3)
  )
)
check(
  "box seed is 0 on an infinite bound",
  identical(
    logvar_box_seed(list(set_lower = c(-Inf, 2), set_upper = c(1, Inf))),
    c(0, 0)
  )
)
lsr_sch <- data.frame(
  coef = c("a", "b"), lower = c(-1, -2), upper = c(1, 2),
  lower_status = c("bounded", "unbounded"), upper_status = c("bounded", "bounded"),
  stringsAsFactors = FALSE
)
check(
  "side record reads the four vectors off a valid schema",
  identical(logvar_side_record(lsr_sch, c("a", "b")), list(
    lower = c(-1, -2), upper = c(1, 2),
    lower_status = c("bounded", "unbounded"), upper_status = c("bounded", "bounded")
  ))
)
lsr_na <- logvar_side_record(NULL, c("a", "b"))
check(
  "a NULL schema becomes an all-failed record of the right length",
  identical(lsr_na$lower_status, c("failed", "failed")) &&
    identical(lsr_na$upper_status, c("failed", "failed")) &&
    all(is.na(lsr_na$lower)) && all(is.na(lsr_na$upper))
)
lsr_mismatch <- logvar_side_record(lsr_sch, c("a", "c"))
check(
  "a coef-label mismatch also becomes an all-failed record",
  identical(lsr_mismatch$lower_status, c("failed", "failed")) &&
    length(lsr_mismatch$lower) == 2L
)
lsc_spec <- list(
  coefs = c("a", "b"),
  taus = c(0.05, 0.2),
  estimator_ids = c("ppml", "harvey")
)
lsc_rec <- function(lo, up, lst, ust) {
  list(lower = lo, upper = up, lower_status = lst, upper_status = ust)
}
lsc_good <- list(
  ppml = list(
    lsc_rec(c(-1, -2), c(1, 2), c("bounded", "bounded"), c("bounded", "bounded")),
    lsc_rec(c(-1.5, NA), c(1.5, NA), c("bounded", "unbounded"), c("bounded", "unbounded"))
  ),
  harvey = list(
    lsc_rec(c(-3, -4), c(3, 4), c("bounded", "bounded"), c("bounded", "bounded")),
    lsc_rec(c(-3.5, -4.5), c(3.5, 4.5), c("bounded", "bounded"), c("bounded", "bounded"))
  )
)
lsc_collected <- logvar_set_boot_collect(list(lsc_good, "solver exploded"), lsc_spec)
check(
  "collect stacks draws into B x p matrices with the spec's coefficient names",
  identical(dim(lsc_collected$ppml[[1]]$lower), c(2L, 2L)) &&
    identical(colnames(lsc_collected$ppml[[1]]$lower), c("a", "b"))
)
check(
  "collect preserves per-estimator per-tau values on the good draw",
  identical(unname(lsc_collected$harvey[[2]]$upper[1, ]), c(3.5, 4.5))
)
check(
  "an errored (character) draw becomes an all-failed row, never dropped",
  nrow(lsc_collected$ppml[[1]]$lower) == 2L &&
    all(is.na(lsc_collected$ppml[[1]]$lower[2, ])) &&
    all(lsc_collected$ppml[[1]]$lower_status[2, ] == "failed") &&
    all(lsc_collected$harvey[[2]]$upper_status[2, ] == "failed")
)
