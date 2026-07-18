# Subprocess check that tracked routing decisions need no ignored docs state.

local({
  checkout_root <- tempfile("paper-decision-checkout-")
  dir.create(checkout_root, recursive = TRUE)
  on.exit(unlink(checkout_root, recursive = TRUE), add = TRUE)

  copied <- file.copy(
    repo_path("DESCRIPTION"),
    file.path(checkout_root, "DESCRIPTION")
  )
  source_inventory <- system2(
    "git",
    c(
      "ls-files",
      "--cached",
      "--others",
      "--exclude-standard",
      "--",
      "scripts-paper"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  source_inventory <- source_inventory[
    !startsWith(source_inventory, "scripts-paper/output/")
  ]
  source_inventory <- source_inventory[
    file.exists(file.path(repo_root, source_inventory))
  ]
  decision_sources <- c(
    "scripts-paper/config/decisions/egarch.R",
    "scripts-paper/config/decisions/lad.dcf"
  )
  destinations <- file.path(checkout_root, source_inventory)
  invisible(vapply(
    unique(dirname(destinations)),
    dir.create,
    logical(1),
    recursive = TRUE,
    showWarnings = FALSE
  ))
  copied_sources <- file.copy(
    file.path(repo_root, source_inventory),
    destinations
  )
  dir.create(file.path(checkout_root, "docs"))
  check(
    "clean-checkout fixture uses version-control-visible sources and empty docs",
    copied &&
      all(decision_sources %in% source_inventory) &&
      all(copied_sources) &&
      !length(list.files(file.path(checkout_root, "docs")))
  )

  script <- file.path(checkout_root, "check-decisions.R")
  writeLines(c(
    sprintf(
      "setwd(%s)",
      encodeString(checkout_root, quote = "\"")
    ),
    "source(file.path(\"scripts-paper\", \"config\", \"paths.R\"))",
    paste0(
      "paper_source_once(paper_path(",
      "\"config\", \"artifacts.R\"))"
    ),
    paste0(
      "paper_source_once(paper_path(",
      "\"config\", \"decisions\", \"egarch.R\"))"
    ),
    paste0(
      "paper_source_once(paper_path(",
      "\"log_variance\", \"estimators\", \"lad\", ",
      "\"dependency_gate.R\"))"
    ),
    paste0(
      "lad_path <- paper_path(",
      "\"config\", \"decisions\", \"lad.dcf\")"
    ),
    paste0(
      "lad_version <- read.dcf(",
      "lad_path, fields = \"quantreg_version\")[1L, 1L]"
    ),
    paste0(
      "lad <- logvar_lad_gate_read(",
      "lad_path, TRUE, lad_version)"
    ),
    "stopifnot(is.list(logvar_egarch_decision))",
    "stopifnot(isTRUE(lad$source_lad))"
  ), script)

  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    script,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  check(
    "tracked decisions source with an empty docs directory",
    identical(as.integer(status), 0L)
  )
})
