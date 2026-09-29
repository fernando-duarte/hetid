# Identity of the structural-inference draws. Whole control objects are hashed
# conservatively, so a presentation-only control edit can invalidate the cache.
# The prepared tuple stands
# in for the upstream data and the code that built it, so an edit that never
# reaches the estimator keeps the cache. The renderer, the table template and
# this cache's own IO stay out, and so does the repository's R/ tree: the draws
# run the installed hetid, which is hashed directly. Bump the schema by hand
# whenever the cached fields change.

STRUCTURAL_INFERENCE_CACHE_SCHEMA <- 1L

STRUCTURAL_INFERENCE_CODE_FILES <- c(
  paste0("support/structural_inference/", c(
    "settings.R", "inputs.R", "arrays.R", "fit.R", "positive.R", "failures.R",
    "reference.R", "bootstrap.R", "calibration.R"
  )),
  "support/statistics/mbb_protocol_authority.R",
  "support/statistics/mbb_rng_state.R",
  "support/identification/quadratic_system.R",
  "log_variance/engine/contracts.R"
)

# functions that share a file with presentation code, hashed as functions so a
# table-formatting edit in the same file keeps the draws
STRUCTURAL_INFERENCE_CODE_FUNCTIONS <- "paper_newey_west_statistics"

STRUCTURAL_INFERENCE_PACKAGES <- c("hetid", "sandwich", "tsibble")

# Helper function: hash the prepared numeric, date and variance-mask tuple
structural_inference_input_sha <- function(prepared) {
  prepared$settings <- NULL
  paper_sha256_object(prepared)
}

# Helper function: hash functions by their deparsed code
# serialize() writes the flag bits R sets on a call once its function has run,
# so hashing the language objects changes after the first hetid call of a
# session. the deparsed text does not, and hexNumeric keeps constants exact
structural_inference_code_sha <- function(functions) {
  paper_sha256_object(lapply(functions, deparse, control = c(
    "keepInteger", "showAttributes", "keepNA", "niceNames", "hexNumeric", "quoteExpressions"
  )))
}

# Helper function: the installed hetid's functions and constants, by name
structural_inference_namespace <- function(package = "hetid") {
  namespace <- asNamespace(package)
  symbols <- sort(ls(namespace, all.names = TRUE), method = "radix")
  values <- mget(symbols, envir = namespace)
  list(
    functions = Filter(is.function, values),
    constants = Filter(function(value) !is.function(value) && !is.environment(value), values)
  )
}

# Helper function: the identity a cached result must carry to be reused
structural_inference_identity <- function(prepared, settings) {
  stopifnot(identical(settings, prepared$settings))
  functions <- mget(STRUCTURAL_INFERENCE_CODE_FUNCTIONS, envir = globalenv(), mode = "function")
  hetid <- structural_inference_namespace()
  list(
    schema = STRUCTURAL_INFERENCE_CACHE_SCHEMA,
    input_sha = structural_inference_input_sha(prepared),
    settings = settings,
    code_sha = paper_boot_code_sha(STRUCTURAL_INFERENCE_CODE_FILES),
    function_sha = structural_inference_code_sha(functions),
    hetid_code_sha = structural_inference_code_sha(hetid$functions),
    hetid_constants_sha = paper_sha256_object(hetid$constants),
    # the DESCRIPTION strings; installed hetid records no RemoteSha, so the
    # namespace hashes above carry its code identity
    packages = vapply(STRUCTURAL_INFERENCE_PACKAGES, function(package) {
      utils::packageDescription(package)$Version
    }, character(1)),
    r_version = R.version.string, platform = R.version$platform,
    blas = unname(extSoftVersion()[["BLAS"]]), lapack = La_library(),
    lapack_version = La_version(),
    controls_sha = paper_sha256_object(list(
      analysis = PAPER_ANALYSIS_CONTRACT,
      inference = PAPER_INFERENCE_SEARCH_CONTROL,
      reporting = PAPER_REPORTING_CONTROL
    ))
  )
}
