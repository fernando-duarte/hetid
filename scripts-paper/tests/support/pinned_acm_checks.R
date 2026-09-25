# Exercise the preflight adapter and the actual extraction call without a network.
local({
  env <- new.env(parent = globalenv())
  sys.source(paper_path("support", "data", "frozen_inputs.R"), env)
  env$acm_daily_release <- "paper-release"
  env$acm_daily_sha256 <- strrep("a", 64)
  preflight <- env$paper_ensure_pinned_acm_daily
  # Replace only the package call with an argument-capturing function.
  body(preflight)[[2]][[1]] <- quote(list)
  args <- preflight()
  check("frozen preflight forwards the paper pin", identical(args, list(
    frequency = "daily", release = env$acm_daily_release,
    expected_sha256 = env$acm_daily_sha256, quiet = TRUE
  )))
  expressions <- parse(paper_path("data_preparation", "build_yield_volatility.R"))
  assignment <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("acm_daily"))
  }, as.list(expressions))[[1]]
  call <- assignment[[3]]
  call[[1]] <- quote(list)
  env$yield_volatility_input <- list(
    data_types = "yields", frequency = "daily", auto_download = TRUE, source = "github"
  )
  env$all_mats <- hetid::HETID_CONSTANTS$ALL_ACM_MATURITIES
  env$acm_daily_source <- "frozen"
  args <- eval(call, env)
  check("daily extraction selects the preflight pin", identical(
    args[c("release", "expected_sha256")],
    list(release = env$acm_daily_release, expected_sha256 = env$acm_daily_sha256)
  ))
  env$acm_daily_source <- "live"
  args <- eval(call, env)
  check("live daily extraction remains unpinned", is.null(args$release) &&
    is.null(args$expected_sha256))
  runner <- readLines(paper_path("run_pipeline.R"))
  verify <- grep("^paper_verify_frozen_inputs\\(", runner)
  cleanup <- grep("cleanup_conditional_artifacts", runner, fixed = TRUE)
  check("frozen inputs are verified before cleanup", length(verify) == 1L &&
    length(cleanup) > 0L && verify < min(cleanup))
})
