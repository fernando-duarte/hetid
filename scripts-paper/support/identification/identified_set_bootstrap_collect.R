set_id_boot_collect <- function(boot_raw, spec) {
  n_coef <- length(spec$coefs)
  failed <- vapply(boot_raw, is.character, logical(1))
  causes <- if (any(failed)) table(unlist(boot_raw[failed])) else NULL
  failed_draw <- list(
    point = rep(NA_real_, n_coef),
    point_ok = FALSE,
    bounds = rep(list(list(
      lower = rep(NA_real_, n_coef),
      upper = rep(NA_real_, n_coef),
      status = rep(PAPER_ENDPOINT_STATUS[["failed"]], n_coef)
    )), length(spec$taus)),
    tau_star = NA_real_,
    capped = FALSE
  )
  boot_raw[failed] <- list(failed_draw)
  point_draws <- do.call(rbind, lapply(boot_raw, `[[`, "point"))
  colnames(point_draws) <- spec$coefs
  endpoints <- lapply(seq_along(spec$taus), function(index) {
    extract <- function(field) {
      out <- do.call(rbind, lapply(boot_raw, function(draw) {
        draw$bounds[[index]][[field]]
      }))
      colnames(out) <- spec$coefs
      out
    }
    list(lower = extract("lower"), upper = extract("upper"), status = extract("status"))
  })
  list(
    point_draws = point_draws,
    n_point_deficient = sum(!vapply(boot_raw, `[[`, logical(1), "point_ok")) - sum(failed),
    endpoint_draws = endpoints,
    tau_star_draws = vapply(boot_raw, `[[`, numeric(1), "tau_star"),
    n_capped = sum(vapply(boot_raw, `[[`, logical(1), "capped")),
    n_failed = sum(failed),
    failure_causes = causes
  )
}
