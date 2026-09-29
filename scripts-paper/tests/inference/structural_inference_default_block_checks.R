# The driver's default block length follows the realised mean sample, and the
# production default keeps the identity it had when the block length followed
# the window. No draws run: the cache read returns the identity it is handed.
# Sourced by test_structural_inference_cache.R after the small real fit, which
# loads the frozen inputs.

block_check <- .test$optional_check(frozen, "frozen paper inputs unavailable")
if (frozen) {
  real_identity <- structural_inference_identity
  real_cache_read <- structural_inference_cache_read
  real_growth <- gr1_pcecc96
  seen <- new.env()
  structural_inference_identity <- function(prepared, settings) {
    seen$prepared <- prepared
    real_identity(prepared, settings)
  }
  structural_inference_cache_read <- function(path, identity) identity
  driver <- function(...) {
    paper_structural_inference_run(..., workers = 1L, path = real_path, mode = "reuse")
  }
  # the pre-change default: window-derived settings, then the sample
  old_settings <- structural_inference_settings()
  old_prepared <- paper_structural_inference_prepare(old_settings)
  old_identity <- real_identity(old_prepared, old_settings)
  actual <- driver()
  actual_prepared <- seen$prepared
  seam <- driver(prepared = old_prepared)
  # sixty fewer quarters of growth leave 196 mean origins, block length 9
  growth <- hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
  dropped <- head(which(gr1_pcecc96$qtr >= tsibble::yearquarter(date_begin)), 60L)
  gr1_pcecc96[[growth]][dropped] <- NA_real_
  short <- driver()
  short_prepared <- seen$prepared
  custom <- driver(settings = structural_inference_settings(block_length = 7L))
  explicit <- driver(settings = structural_inference_settings())
  gr1_pcecc96 <- real_growth
  structural_inference_identity <- real_identity
  structural_inference_cache_read <- real_cache_read
}
block_check("the default run keeps the pre-change identity and prepared inputs", {
  identical(actual, old_identity) && identical(actual_prepared, old_prepared) &&
    actual_prepared$n_obs == 256L && actual$settings$block_length == 10L
})
block_check("prepared inputs without settings run under their own settings", {
  identical(seam, old_identity)
})
block_check("a shorter mean sample sets the default block from its size", {
  short_prepared$n_obs == 196L && short$settings$block_length == 9L &&
    identical(short_prepared$settings, short$settings) &&
    identical(
      short$settings[names(short$settings) != "block_length"],
      old_settings[names(old_settings) != "block_length"]
    )
})
block_check("caller settings keep their block length on a shorter sample", {
  custom$settings$block_length == 7L && explicit$settings$block_length == 10L &&
    identical(custom$input_sha, short$input_sha)
})
