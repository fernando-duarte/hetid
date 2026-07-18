# Project-root and source-path conventions for the paper analysis pipeline.
# Source paths are absolute so nested source() calls do not depend on a caller's
# file location. Generated-artifact paths remain portable and repository-relative.

is_package_root <- file.exists("DESCRIPTION") && dir.exists("scripts-paper")
package_name <- if (is_package_root) {
  unname(read.dcf("DESCRIPTION", fields = "Package")[1L, 1L])
} else {
  NA_character_
}
if (!isTRUE(identical(package_name, "hetid"))) {
  stop(
    "The scripts-paper pipeline must be run from the hetid package root.",
    call. = FALSE
  )
}
rm(is_package_root, package_name)

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

.source_path_parts <- function(parts) {
  if (!length(parts)) {
    return(character(0))
  }
  if (anyNA(parts) || any(!nzchar(parts)) ||
    any(grepl("^([/\\\\]|[A-Za-z]:)", parts))) {
    stop("Source-path components must be nonempty relative paths.", call. = FALSE)
  }
  parts
}

repo_path <- function(...) {
  parts <- .source_path_parts(as.character(list(...)))
  path <- if (length(parts)) {
    do.call(file.path, c(list(repo_root), as.list(parts)))
  } else {
    repo_root
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

paper_path <- function(...) {
  parts <- .source_path_parts(as.character(list(...)))
  do.call(repo_path, c(list("scripts-paper"), as.list(parts)))
}

out_dir <- file.path("scripts-paper", "output")

.paper_source_registry <- new.env(parent = emptyenv())

paper_source_once <- function(path, envir = .GlobalEnv) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  state <- get0(
    path,
    envir = .paper_source_registry,
    inherits = FALSE
  )
  if (isTRUE(state)) {
    return(invisible(FALSE))
  }
  if (identical(state, FALSE)) {
    stop(
      sprintf("Circular paper source dependency: %s", path),
      call. = FALSE
    )
  }
  assign(path, FALSE, envir = .paper_source_registry)
  loaded <- FALSE
  on.exit(
    {
      if (!loaded) {
        rm(list = path, envir = .paper_source_registry)
      }
    },
    add = TRUE
  )
  base::sys.source(path, envir = envir)
  assign(path, TRUE, envir = .paper_source_registry)
  loaded <- TRUE
  invisible(TRUE)
}

paper_source_once(paper_path("support", "runtime", "core.R"))
paper_source_once(paper_path("config", "analysis_contract.R"))
