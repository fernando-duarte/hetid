set_id_boot_collect <- function(boot_raw, spec) {
  n_coef <- length(spec$coefs)
  failed <- vapply(boot_raw, is.character, logical(1))
  causes <- if (any(failed)) table(unlist(boot_raw[failed])) else NULL
  # a wholesale draw failure is "failed" on every status the draw would have
  # produced -- both endpoint sides and the tau = 0 point alike
  failed_status <- rep(PAPER_ENDPOINT_STATUS[["failed"]], n_coef)
  failed_draw <- list(
    point = rep(NA_real_, n_coef),
    point_status = failed_status,
    point_ok = FALSE,
    bounds = rep(list(list(
      lower = rep(NA_real_, n_coef),
      upper = rep(NA_real_, n_coef),
      lower_status = failed_status,
      upper_status = failed_status
    )), length(spec$taus))
  )
  boot_raw[failed] <- list(failed_draw)
  stack <- function(field) {
    out <- do.call(rbind, lapply(boot_raw, `[[`, field))
    colnames(out) <- spec$coefs
    out
  }
  endpoints <- lapply(seq_along(spec$taus), function(index) {
    extract <- function(field) {
      out <- do.call(rbind, lapply(boot_raw, function(draw) {
        draw$bounds[[index]][[field]]
      }))
      colnames(out) <- spec$coefs
      out
    }
    list(
      lower = extract("lower"), upper = extract("upper"),
      lower_status = extract("lower_status"),
      upper_status = extract("upper_status")
    )
  })
  list(
    point_draws = stack("point"),
    point_status = stack("point_status"),
    n_point_deficient = sum(!vapply(boot_raw, `[[`, logical(1), "point_ok")) - sum(failed),
    endpoint_draws = endpoints,
    n_failed = sum(failed),
    failure_causes = causes
  )
}
