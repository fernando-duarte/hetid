# Literal source parsing and direct-source ownership checks.

literal_path <- function(expression) {
  if (is.character(expression) && length(expression) == 1L) {
    if (grepl("^/", expression)) {
      return(expression)
    }
    return(file.path(repo_root, expression))
  }
  if (!is.call(expression)) {
    return(NA_character_)
  }
  if (!is.symbol(expression[[1L]])) {
    return(NA_character_)
  }
  name <- as.character(expression[[1L]])
  if (name == "normalizePath") {
    return(literal_path(expression[[2L]]))
  }
  if (!name %in% c("paper_path", "repo_path", "file.path")) {
    return(NA_character_)
  }
  parts <- lapply(as.list(expression[-1L]), function(part) {
    if (is.character(part) && length(part) == 1L) {
      part
    } else {
      NA_character_
    }
  })
  if (anyNA(parts)) {
    return(NA_character_)
  }
  path <- do.call(file.path, parts)
  if (name == "paper_path") {
    return(do.call(paper_path, parts))
  }
  if (name == "repo_path") {
    return(do.call(repo_path, parts))
  }
  if (grepl("^/", path)) path else file.path(repo_root, path)
}

source_targets <- function(file) {
  targets <- character()
  walk <- function(node) {
    call_name <- if (is.call(node) && is.symbol(node[[1L]])) {
      as.character(node[[1L]])
    } else {
      ""
    }
    if (call_name %in% c("source", "paper_source_once")) {
      target <- literal_path(node[[2L]])
      if (!is.na(target)) {
        targets <<- c(targets, target)
      }
    }
    if (is.call(node) || is.expression(node) || is.pairlist(node)) {
      children <- as.list(node)
      for (index in seq_along(children)) {
        if (!identical(children[[index]], quote(expr = ))) {
          walk(children[[index]])
        }
      }
    }
  }
  walk(parse(file))
  unique(targets)
}

for (file in r_files) {
  expressions <- parse(file)
  direct <- FALSE
  walk_direct <- function(node) {
    if (is.call(node) &&
      identical(as.character(node[[1L]]), "source") &&
      is.call(node[[2L]]) &&
      identical(
        as.character(node[[2L]][[1L]]),
        "paper_path"
      )) {
      direct <<- TRUE
    }
    if (is.call(node) || is.expression(node) || is.pairlist(node)) {
      children <- as.list(node)
      for (index in seq_along(children)) {
        if (!identical(children[[index]], quote(expr = ))) {
          walk_direct(children[[index]])
        }
      }
    }
  }
  walk_direct(expressions)
  if (direct) {
    record_problem(
      "Direct paper_source_once(paper_path()) remains in %s",
      relative_paper(file)
    )
  }
}

source_map <- stats::setNames(
  lapply(r_files, source_targets),
  r_files
)
paper_prefix <- paste0(paper_root, "/")
record_paths <- function(label, paths) {
  if (length(paths)) {
    record_problem(
      "%s: %s",
      label,
      paste(paths, collapse = ", ")
    )
  }
}
for (file in names(source_map)) {
  missing <- source_map[[file]][!file.exists(source_map[[file]])]
  external <- source_map[[file]][
    !startsWith(source_map[[file]], paper_prefix)
  ]
  record_paths(
    sprintf(
      "Missing source targets from %s",
      relative_paper(file)
    ),
    missing
  )
  record_paths(
    sprintf(
      "External source targets from %s",
      relative_paper(file)
    ),
    external
  )
}
