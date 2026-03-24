# Quality Check Script for hetid
# Runs all code quality tools in the correct order and produces reports.
# Usage: Rscript scripts/quality-check.R
#
# Tools are grouped by execution constraints:
#   Group 1: pkgcheck, rcmdcheck (create .Rcheck/ artifacts)
#   Group 2: codetools, cyclocomp (require load_all)
#   Group 3: dupree, CodeDepends, lintr, checkglobals, spelling, urlchecker
#   Group 4: covr (slowest)

library(cli)

start_time <- Sys.time()
cli_h1("hetid Quality Check")
cli_text("Started at: {.timestamp {start_time}}")
cli_rule()

# --- Preflight -----------------------------------------------------------

report_dir <- "docs/quality-reports"
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

required <- c(
  "dupree", "CodeDepends", "codetools", "covr", "cyclocomp",
  "lintr", "pkgcheck", "pkgload", "devtools", "checkglobals",
  "spelling", "urlchecker", "rcmdcheck"
)
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing)) {
  cli_alert_danger("Missing packages: {.pkg {missing}}")
  stop("Install missing packages before running quality checks.")
}

writeLines(
  capture.output(sessionInfo()),
  file.path(report_dir, "session-info.txt")
)
cli_alert_success("Session info captured")

# --- Helper ---------------------------------------------------------------

# Track pass/fail per tool (from Codex consensus contribution)
tool_results <- data.frame(
  tool = character(), status = character(),
  elapsed_s = numeric(), stringsAsFactors = FALSE
)

run_check <- function(name, expr) {
  cli_h2(name)
  check_start <- Sys.time()
  result <- tryCatch(
    expr,
    error = function(e) {
      cli_alert_danger("{name} failed: {e$message}")
      NULL
    }
  )
  elapsed <- round(
    as.numeric(difftime(Sys.time(), check_start, units = "secs")), 1
  )
  status <- if (!is.null(result)) "pass" else "fail"
  tool_results <<- rbind(tool_results, data.frame(
    tool = name, status = status,
    elapsed_s = elapsed, stringsAsFactors = FALSE
  ))
  if (!is.null(result)) {
    cli_alert_success("{name}: done ({elapsed}s)")
  }
  invisible(result)
}

# --- Group 1: Heavy aggregators ------------------------------------------

cli_h1("Group 1: pkgcheck + rcmdcheck")

run_check("pkgcheck", {
  library(pkgcheck)
  checks <- pkgcheck(".", goodpractice = FALSE)
  txt <- capture.output(print(summary(checks)))
  writeLines(txt, file.path(report_dir, "pkgcheck.txt"))
  checks
})

unlink("hetid.Rcheck", recursive = TRUE)
unlink(list.files(pattern = "hetid_.*[.]tar[.]gz$"), force = TRUE)

run_check("rcmdcheck (strict)", {
  library(rcmdcheck)
  Sys.setenv(
    `_R_CHECK_LENGTH_1_CONDITION_` = "true",
    `_R_CHECK_LENGTH_1_LOGIC2_` = "true",
    `_R_CHECK_MATRIX_DATA_` = "true",
    `_R_CHECK_CONNECTIONS_LEFT_OPEN_` = "true",
    `_R_CHECK_THINGS_IN_TEMP_DIR_` = "true",
    `_R_CHECK_BROWSER_NONINTERACTIVE_` = "true",
    `_R_CHECK_RD_VALIDATE_RD2HTML_` = "true"
  )
  res <- rcmdcheck(".", args = "--as-cran", quiet = TRUE)
  capture.output(res, file = file.path(
    report_dir,
    "rcmdcheck-strict.txt"
  ))
  cli_text(
    "  {length(res$errors)} errors, {length(res$warnings)} warnings, ",
    "{length(res$notes)} notes"
  )
  res
})

unlink("hetid.Rcheck", recursive = TRUE)
unlink(list.files(pattern = "hetid_.*[.]tar[.]gz$"), force = TRUE)

# --- Group 2: Package-load-dependent tools --------------------------------

cli_h1("Group 2: codetools + cyclocomp")

pkgload::load_all(".")

run_check("codetools", {
  library(codetools)
  out <- capture.output(
    checkUsagePackage("hetid", all = TRUE, suppressLocal = FALSE)
  )
  writeLines(out, file.path(report_dir, "codetools.txt"))
  cli_text("  {length(out)} warning(s)")
  out
})

run_check("cyclocomp", {
  library(cyclocomp)
  ns <- getNamespace("hetid")
  fns <- ls(ns, all.names = FALSE)
  cc <- data.frame(
    func = fns,
    complexity = vapply(fns, function(f) {
      fn <- get(f, envir = ns)
      if (is.function(fn)) cyclocomp(fn) else NA_integer_
    }, integer(1)),
    stringsAsFactors = FALSE
  )
  cc <- cc[order(-cc$complexity), ]
  write.csv(cc, file.path(report_dir, "cyclocomp.csv"),
    row.names = FALSE
  )
  high <- cc[cc$complexity > 15 & !is.na(cc$complexity), ]
  cli_text("  {nrow(high)} function(s) above complexity 15")
  cc
})

# --- Group 3: File-only tools --------------------------------------------

cli_h1("Group 3: dupree, CodeDepends, lintr, checkglobals, spelling, urlchecker")

run_check("dupree", {
  library(dupree)
  dup_result <- dupree_package(".", min_block_size = 20)
  dup <- dup_result$dups_df
  dup <- dup[order(-dup$score), , drop = FALSE]
  write.csv(dup, file.path(report_dir, "dupree.csv"),
    row.names = FALSE
  )
  high <- dup[dup$score > 0.5, ]
  cli_text("  {nrow(high)} pair(s) above score 0.5")
  dup
})

run_check("CodeDepends", {
  library(CodeDepends)
  scripts <- list.files("R", pattern = "[.]R$", full.names = TRUE)
  names(scripts) <- basename(scripts)
  dep_info <- lapply(scripts, function(f) {
    tryCatch(getInputs(readScript(f)), error = function(e) {
      warning("Failed on ", basename(f), ": ", e$message)
      NULL
    })
  })
  dep_summary <- lapply(names(dep_info), function(nm) {
    info_list <- dep_info[[nm]]
    if (is.null(info_list)) {
      return(NULL)
    }
    data.frame(
      file = nm,
      inputs = paste(unique(unlist(
        lapply(info_list, slot, "inputs")
      )), collapse = ", "),
      outputs = paste(unique(unlist(
        lapply(info_list, slot, "outputs")
      )), collapse = ", "),
      called_symbols = paste(unique(unlist(
        lapply(info_list, function(node) {
          names(slot(node, "functions"))
        })
      )), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  dep_df <- do.call(rbind, Filter(Negate(is.null), dep_summary))
  write.csv(dep_df, file.path(report_dir, "codedepends.csv"),
    row.names = FALSE
  )
  cli_text("  {nrow(dep_df)} file(s) analyzed")
  dep_df
})

run_check("lintr", {
  library(lintr)
  lints <- lint_package(".")
  capture.output(lints, file = file.path(
    report_dir,
    "lintr-strict.txt"
  ))
  cli_text("  {length(lints)} finding(s)")
  lints
})

run_check("checkglobals", {
  library(checkglobals)
  res <- check_pkg(".")
  capture.output(res, file = file.path(
    report_dir,
    "checkglobals.txt"
  ))
  res
})

run_check("spelling", {
  library(spelling)
  sp <- spell_check_package(".")
  if (nrow(sp) > 0) {
    write.csv(sp, file.path(report_dir, "spelling.csv"),
      row.names = FALSE
    )
    cli_text("  {nrow(sp)} potential misspelling(s)")
  } else {
    cli_text("  0 misspellings")
  }
  sp
})

run_check("urlchecker", {
  library(urlchecker)
  res <- url_check(".")
  writeLines(
    capture.output(print(res)),
    file.path(report_dir, "urlchecker.txt")
  )
  if (nrow(res) > 0) {
    cli_text("  {nrow(res)} URL issue(s)")
  } else {
    cli_text("  All URLs valid")
  }
  res
})

# --- Group 4: Coverage ---------------------------------------------------

cli_h1("Group 4: covr")

run_check("covr", {
  library(covr)
  cov <- package_coverage(".", type = "tests", quiet = TRUE)
  report(cov,
    file = file.path(report_dir, "covr-report.html"),
    browse = FALSE
  )

  line_cov <- tally_coverage(cov, by = "line")
  write.csv(as.data.frame(line_cov),
    file.path(report_dir, "covr-lines.csv"),
    row.names = FALSE
  )

  line_df <- as.data.frame(line_cov)
  func_summary <- aggregate(
    value ~ filename + functions,
    data = line_df,
    FUN = function(x) round(100 * mean(x > 0), 1)
  )
  names(func_summary)[3] <- "coverage_pct"
  func_summary <- func_summary[order(func_summary$coverage_pct), ]
  write.csv(func_summary,
    file.path(report_dir, "covr-summary.csv"),
    row.names = FALSE
  )

  zero <- zero_coverage(cov)
  write.csv(as.data.frame(zero),
    file.path(report_dir, "covr-zero.csv"),
    row.names = FALSE
  )

  pct <- percent_coverage(cov)
  cli_text("  Package coverage: {round(pct, 1)}%")

  low <- func_summary[func_summary$coverage_pct < 80, ]
  if (nrow(low) > 0) {
    cli_text("  {nrow(low)} function(s) under 80%")
  }
  cov
})

# --- Summary --------------------------------------------------------------

cli_rule()
elapsed <- round(
  as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1
)
cli_h1("Quality Check Complete ({elapsed} min)")

# Write tool summary CSV (from Codex consensus contribution)
write.csv(tool_results,
  file.path(report_dir, "tool-summary.csv"),
  row.names = FALSE
)
cli_text("Tool summary: {sum(tool_results$status == 'pass')}/{nrow(tool_results)} passed")
cli_text("Reports saved to: {.path {report_dir}}")
cli_text("Run Phase B triage on the reports to identify actionable findings.")
