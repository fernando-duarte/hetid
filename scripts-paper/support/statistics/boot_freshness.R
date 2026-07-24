# Shared runtime and source hashes used by the unified bootstrap provenance.

paper_boot_runtime_sha <- function() {
  pkgs <- c("hetid", "nloptr", "sandwich")
  versions <- vapply(
    pkgs,
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  )
  external <- extSoftVersion()
  paper_sha256_object(list(
    r_version = as.character(getRversion()),
    platform = R.version$platform,
    packages = stats::setNames(versions, pkgs),
    hetid_source_sha = paper_repo_code_sha(
      file.path(
        "R",
        list.files(repo_path("R"), pattern = "[.]R$")
      )
    ),
    hetid_namespace_sha = paper_namespace_code_sha("hetid"),
    blas = unname(external["BLAS"]),
    lapack = unname(external["LAPACK"]),
    controls = list(
      analysis = get0("PAPER_ANALYSIS_CONTRACT", inherits = TRUE),
      inference = get0("PAPER_INFERENCE_SEARCH_CONTROL", inherits = TRUE),
      reporting = get0("PAPER_REPORTING_CONTROL", inherits = TRUE),
      serialization = get0("PAPER_SERIALIZATION_CONTROL", inherits = TRUE),
      cache_schema = get0("BOOTSTRAP_STAGE_CACHE_SCHEMA", inherits = TRUE)
    )
  ))
}

paper_namespace_code_sha <- function(package) {
  namespace <- if (is.environment(package)) {
    package
  } else {
    asNamespace(package)
  }
  symbols <- sort(ls(namespace, all.names = TRUE), method = "radix")
  functions <- lapply(symbols, function(symbol) {
    value <- get(symbol, envir = namespace, inherits = FALSE)
    if (!is.function(value)) {
      return(NULL)
    }
    list(
      name = symbol,
      formals = formals(value),
      body = body(value)
    )
  })
  paper_sha256_object(Filter(Negate(is.null), functions))
}

paper_source_code_sha <- function(paths, path_fn) {
  paths <- sort(unique(paths), method = "radix")
  stopifnot(
    is.character(paths), length(paths) > 0L, !anyNA(paths),
    all(nzchar(paths)), !any(grepl("^/|(^|/)\\.\\.(/|$)", paths))
  )
  contents <- lapply(paths, function(relative_path) {
    parts <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
    path <- do.call(path_fn, as.list(parts))
    size <- file.info(path)$size
    list(
      path = relative_path,
      bytes = readBin(path, what = "raw", n = size)
    )
  })
  paper_sha256_object(contents)
}

paper_boot_code_sha <- function(paths) {
  paper_source_code_sha(paths, paper_path)
}

paper_repo_code_sha <- function(paths) {
  paper_source_code_sha(paths, repo_path)
}
