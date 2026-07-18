# Coverage-selector and bounds-plot checks using the peng_* fixtures.

# An as_selected selector handing back an 8000-row grid reaches the scan
# without the nearest-neighbor cap and scans in callback order.
peng_fine <- function(...) {
  g <- seq(-0.99, 0.99, length.out = 130L)
  m <- as.matrix(expand.grid(g, g))
  dimnames(m) <- NULL
  m[rowSums(m^2) <= 1, , drop = FALSE][seq_len(8000L), , drop = FALSE]
}
peng_asel <- peng_sel("sel-asel", "as_selected", function(g) g)
local({
  old <- logvar_feasible_grid
  on.exit(assign("logvar_feasible_grid", old, envir = globalenv()), add = TRUE)
  ok <- peng_try({
    assign("logvar_feasible_grid", peng_fine, envir = globalenv())
    d <- peng_dummy()
    res <- peng_run(d$est, grid_selector = peng_asel)
    gg <- peng_fine()
    ord <- all(vapply(seq_len(5L), function(i) {
      isTRUE(all.equal(d$cc$order[[i]], unname(gg[i, ])))
    }, logical(1)))
    is.data.frame(res$table) && ord
  })
  check("peng as_selected 8000-row grid bypasses the NN cap in callback order", ok)
})

# The Morton selector has exact size, is permutation-invariant and input-bound,
# responds to a new spatial cell, and works as an engine callback.
peng_mpts <- local({
  m <- matrix(runif(500L * 2L, -1, 1), 500L, 2L)
  m[rowSums(m^2) <= 1, , drop = FALSE]
})
check("peng Morton selector is exact-count, permutation-invariant, input-bound", peng_try({
  if (!exists("logvar_ppml_morton_select")) stop("absent")
  s1 <- logvar_ppml_morton_select(peng_mpts, 40L)
  g1 <- s1$grid
  g2 <- logvar_ppml_morton_select(peng_mpts[sample.int(nrow(peng_mpts)), ], 40L)$grid
  g3 <- logvar_ppml_morton_select(rbind(peng_mpts, c(0.999, -0.001)), 40L)$grid
  nrow(g1) == 40L && setequal(peng_bkey(g1), peng_bkey(g2)) &&
    all(peng_bkey(g1) %in% peng_bkey(peng_mpts)) &&
    !setequal(peng_bkey(g1), peng_bkey(g3)) &&
    identical(s1$selector_id, LOGVAR_PPML_COVERAGE_PROTOCOL$selector_id) &&
    identical(s1$traversal, LOGVAR_PPML_COVERAGE_PROTOCOL$traversal)
}))
check("peng Morton selector works as the engine grid_selector callback", peng_try({
  if (!exists("logvar_ppml_morton_select")) stop("absent")
  res <- peng_run(
    peng_dummy()$est,
    grid_selector = logvar_ppml_morton_select,
    max_grid_points = 10L
  )
  is.data.frame(res$table) && nrow(res$table) == 2L
}))

# The plot-data canonicalizer fixes column types and orders rows by numeric
# tau, declared coefficient order, then side.
peng_row <- function(tau) {
  s <- data.frame(
    tau = tau, coef = c("t0", "t1"), lower = c(-1, -2), upper = c(1, 2),
    lower_status = "bounded", upper_status = "bounded", lower_provenance = "grid",
    upper_provenance = "polish", stringsAsFactors = FALSE
  )
  s$arg_lower <- I(list(c(0, 0), c(0.1, 0.1)))
  s$arg_upper <- I(list(c(0.2, 0.2), c(0.3, 0.3)))
  s
}
check("peng plot-data canonicalizer orders rows and fixes column types", peng_try({
  d <- logvar_bounds_plot_data(
    list(peng_row(0.1), peng_row(0.05)),
    sets = list(), sample_id = "s", spec_id = "p", output_path = "x"
  )$data
  identical(d$tau, c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1)) &&
    identical(d$coef, c("t0", "t0", "t1", "t1", "t0", "t0", "t1", "t1")) &&
    identical(d$side, rep(c("lower", "upper"), 4L)) && is.double(d$tau) &&
    is.character(d$coef) && is.double(d$value) && is.character(d$status) &&
    is.character(d$side) && is.character(d$endpoint_b)
}))
