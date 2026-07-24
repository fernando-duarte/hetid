# Legacy failures are intentionally asymmetric. Shared estimation errors reach
# both wrappers; mean tau-star errors stay mean-only; predecessor and terminal
# volatility-builder failures use different external representations.

blf_reference_est <- estimate_set_id_system(lbd_dat, lbd_spec)
blf_mean_spec <- c(
  list(
    coefs = c(
      names(blf_reference_est$beta1r),
      rownames(blf_reference_est$beta2r)
    ),
    taus = lbd_spec$taus[-1L],
    tau_grid = seq(0, max(lbd_spec$taus), by = 0.05)
  ),
  lbd_spec[c(
    "gamma", "y1_col", "x_cols", "y2_cols", "z_col", "impose_null"
  )]
)
blf_capture <- function(code) {
  tryCatch(force(code), error = function(error) conditionMessage(error))
}
blf_capture_condition <- function(code) {
  tryCatch(
    list(value = force(code)),
    error = function(error) list(message = conditionMessage(error), class = class(error))
  )
}
blf_capture_full <- function(code) {
  tryCatch(force(code), error = function(error) {
    list(
      message = conditionMessage(error), class = class(error),
      call = error$call, token = error$token
    )
  })
}
blf_raw_failure <- function(draw, spec) {
  paper_run_mbb_draws(
    1L, nrow(lbd_dat), 1L,
    function(index, draw_id) draw(lbd_dat[index, , drop = FALSE], spec), 1L
  )$draws[[1L]]
}
blf_mean_control <- paper_legacy_mean_from_est(
  blf_reference_est,
  blf_mean_spec
)
blf_shared <- paper_with_legacy_binding(
  "estimate_set_id_system",
  function(dat, spec) {
    stop(structure(
      list(
        message = "shared estimation fixture",
        call = quote(shared_estimation_fixture()),
        token = list(owner = "shared", code = 23L)
      ),
      class = c("shared_estimation_fixture", "error", "condition")
    ))
  },
  function() {
    list(
      mean = blf_capture_full(set_id_boot_draw(lbd_dat, blf_mean_spec)),
      volatility = blf_capture_full(logvar_set_boot_draw(lbd_dat, lbd_spec)),
      mean_raw = blf_raw_failure(set_id_boot_draw, blf_mean_spec),
      volatility_raw = blf_raw_failure(logvar_set_boot_draw, lbd_spec)
    )
  }
)
blf_shared_projection <- list(
  message = "shared estimation fixture",
  class = c("shared_estimation_fixture", "error", "condition"),
  call = quote(shared_estimation_fixture()),
  token = list(owner = "shared", code = 23L)
)
check(
  "shared estimation condition reaches both wrappers before raw flattening",
  identical(blf_shared$mean, blf_shared_projection) &&
    identical(blf_shared$volatility, blf_shared_projection) &&
    identical(blf_shared$mean_raw, "shared estimation fixture") &&
    identical(blf_shared$volatility_raw, "shared estimation fixture")
)

blf_mean_only <- paper_with_legacy_binding(
  "sweep_fixed_gamma",
  function(...) stop("mean tau-star fixture"),
  function() {
    list(
      mean = blf_capture(set_id_boot_draw(lbd_dat, blf_mean_spec)),
      volatility = logvar_set_boot_draw(lbd_dat, lbd_spec)
    )
  }
)
check(
  "mean tau-star failure leaves the legacy volatility branch unchanged",
  identical(blf_mean_only$mean, "mean tau-star fixture") &&
    identical(blf_mean_only$volatility, lbd_draw)
)

blf_ppml_spec <- lbd_spec
blf_ppml_spec$builders$ppml <- function(...) {
  stop("ppml builder fixture")
}
blf_ppml_actual <- blf_capture_condition(
  logvar_set_boot_draw(lbd_dat, blf_ppml_spec)
)
blf_ppml_expected <- blf_capture_condition(
  paper_legacy_logvar_from_est(
    lbd_dat,
    blf_reference_est,
    blf_ppml_spec
  )
)
check(
  "failed predecessor builder remains an outer volatility error",
  identical(blf_ppml_actual, blf_ppml_expected) &&
    identical(blf_ppml_actual$message, "all(dependencies %in% names(built)) is not TRUE") &&
    identical(blf_ppml_actual$class, c("simpleError", "error", "condition"))
)

blf_harvey_spec <- lbd_spec
blf_harvey_spec$builders$harvey <- function(...) {
  stop("harvey builder fixture")
}
blf_harvey_actual <- logvar_set_boot_draw(lbd_dat, blf_harvey_spec)
blf_harvey_expected <- paper_legacy_logvar_from_est(
  lbd_dat,
  blf_reference_est,
  blf_harvey_spec
)
blf_harvey_status <- unlist(lapply(blf_harvey_actual$harvey, function(cell) {
  c(cell$lower_status, cell$upper_status)
}))
check(
  "failed terminal builder remains structured failed volatility cells",
  identical(blf_harvey_actual, blf_harvey_expected) &&
    identical(blf_harvey_actual$ppml, lbd_draw$ppml) &&
    length(blf_harvey_status) > 0L &&
    all(blf_harvey_status == PAPER_ENDPOINT_STATUS[["failed"]])
)
check(
  "volatility builder failures leave the legacy mean branch unchanged",
  identical(
    set_id_boot_draw(lbd_dat, blf_mean_spec),
    blf_mean_control
  )
)
rm(
  blf_reference_est,
  blf_mean_spec,
  blf_capture,
  blf_capture_condition,
  blf_capture_full,
  blf_raw_failure,
  blf_mean_control,
  blf_shared,
  blf_shared_projection,
  blf_mean_only,
  blf_ppml_spec,
  blf_ppml_actual,
  blf_ppml_expected,
  blf_harvey_spec,
  blf_harvey_actual,
  blf_harvey_expected,
  blf_harvey_status
)
