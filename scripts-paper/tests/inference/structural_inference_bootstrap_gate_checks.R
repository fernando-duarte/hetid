# Publication gates of the structural-inference bootstrap, sourced by
# test_structural_inference_bootstrap.R after its stand-ins are defined.

# Failed-draw cap ---------------------------------------------------------
# four draws against a cap of one quarter. tau .05 loses one PPML candidate in
# one draw, tau .10 loses every candidate in two, tau .20 fails its sample in
# one, the tau-zero variance point fails in one, and the tau .05 mean range is
# unresolved geometry in every draw
stopifnot(settings$maximum_failed_share == 0.25)
alter <- function(frame, draw_id) {
  variance <- frame$panel == "variance"
  rows <- variance & frame$tau == 0.05
  if (draw_id == 1L) {
    frame$lower_status[rows] <- frame$upper_status[rows] <- "unreliable"
    frame$n_attempted[rows] <- 3L
    frame$n_failed[rows] <- 1L
  }
  rows <- variance & frame$tau == 0.10
  if (draw_id %in% 1:2) {
    frame$lower[rows] <- frame$upper[rows] <- NA_real_
    frame$lower_status[rows] <- frame$upper_status[rows] <- "unreliable"
    frame$n_attempted[rows] <- frame$n_failed[rows] <- 3L
  }
  for (rows in list(variance & frame$tau == 0.20, variance & frame$tau == 0)) {
    if (draw_id == 1L) {
      frame$lower[rows] <- frame$upper[rows] <- NA_real_
      frame$lower_status[rows] <- frame$upper_status[rows] <- "failed"
    }
  }
  rows <- frame$panel == "mean" & frame$tau == 0.05
  if (draw_id > 0L) frame$lower_status[rows] <- frame$upper_status[rows] <- "unreliable"
  frame
}
capped <- boot(prepared, settings)
capped_forked <- boot(prepared, settings, workers = 2L)
alter <- function(frame, draw_id) frame
gates <- capped$failure_gates
share <- function(panel, tau) {
  unique(gates$failed_share_lower[axis$panel == panel & axis$tau == tau])
}
stopifnot(
  identical(capped, capped_forked), identical(gates$coef, axis$coef),
  identical(gates$failed_share_lower, gates$failed_share_upper),
  share("variance", 0.05) == 0.25, share("variance", 0.10) == 0.5,
  share("variance", 0.20) == 0.25, share("variance", 0) == 0.25,
  share("mean", 0.05) == 0, share("mean", 0) == 0,
  identical(gates$passed, !(axis$panel == "variance" & axis$tau == 0.10)),
  !capped$publication_ok
)
intervals <- capped$intervals$summary
row <- match(intervals$coef, axis$coef)
over <- axis$panel[row] == "variance" & axis$tau[row] == 0.10
unresolved <- axis$panel[row] == "mean" & axis$tau[row] == 0.05
stopifnot(
  !any(intervals$publication_allowed[over]),
  all(intervals$failure_reason[over] == "failed-replication share exceeds application limit"),
  all(is.na(intervals$failure_reason[!over])),
  all(intervals$reason[unresolved] != "reported"),
  !any(intervals$publication_allowed[unresolved]),
  all(intervals$publication_allowed[!over & !unresolved] ==
    (intervals$reason[!over & !unresolved] == "reported")),
  all(capped$point_summary$failure_reason %in% NA)
)
cat("ok   partial and total PPML failures count toward the cap, unresolved geometry does not\n")

# Calibration search stop -------------------------------------------------
# spread lower and upper endpoints independently over 45 draws, so the pointwise
# search has room between its endpoint quantiles and the containment cap. a two
# evaluation budget stops it at max_evals, which must block publication, while
# the paper's unlimited budget converges on tolerance and publishes
alter <- function(frame, draw_id) {
  range <- frame$tau > 0
  frame$lower[range] <- 0.01 * ((draw_id * 0.618034) %% 1 - 0.5) * (draw_id > 0)
  frame$upper[range] <- 0.02 + 0.01 * ((draw_id * 0.414214) %% 1 - 0.5) * (draw_id > 0)
  frame
}
search <- function(max_evals) {
  budget <- stub_settings(45L)
  budget$interval_control$max_evals <- max_evals
  budget_prepared <- stub_prepared(budget)
  budget_full <- with_fit(fake_fit, structural_inference_fit(budget_prepared, budget))
  warned <- NULL
  result <- withCallingHandlers(
    boot(budget_prepared, budget, full = budget_full),
    warning = function(w) {
      warned <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  c(result, list(warned = warned))
}
stopped <- search(2L)
converged <- search(.Machine$integer.max)
alter <- function(frame, draw_id) frame
cells <- stopped$intervals$summary
stopifnot(
  all(cells$search_stop == "max_evals"), !any(cells$publication_allowed),
  all(cells$failure_reason == "calibration search stopped at max_evals"),
  !stopped$publication_ok, grepl("publication is blocked", stopped$warned),
  all(stopped$failure_gates$passed),
  all(converged$intervals$summary$search_stop == "tolerance"),
  converged$publication_ok, is.null(converged$warned)
)
cat("ok   a calibration search that stops at its budget blocks publication\n")
