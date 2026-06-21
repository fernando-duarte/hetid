#!/usr/bin/env Rscript
# Value-level numeric-invariance comparator for the D4 gate.
# Usage: Rscript pipeline_value_diff.R <dirA> <dirB>
# Compares parsed VALUES, not bytes: numeric columns of CSVs, numeric leaves of
# RDS (skipping volatile metadata), and ordered numeric tokens of .tex/.txt/.html.
# Skips binary/render/build-cruft (.pdf/.png/.svg/.log/.fls/.fdb_latexmk/.aux/.synctex.gz).
args <- commandArgs(trailingOnly = TRUE)
A <- args[[1]]
B <- args[[2]]
TOL <- 0 # value-EXACT for the deterministic reached stages

skip_ext <- "\\.(pdf|png|svg|log|fls|fdb_latexmk|aux|synctex\\.gz|out|toc|nav|snm|vrb|html|htm)$"
volatile <- c(
  "created", "built_at", "git_sha", "date_created", "timestamp",
  "generated", "date_generated", "sysname", "user"
)

list_rel <- function(d) {
  f <- list.files(d, recursive = TRUE, full.names = FALSE)
  f[!grepl(skip_ext, f, ignore.case = TRUE)]
}
# Recursively collect numeric leaves from an R object, dropping Date/volatile.
num_leaves <- function(x, nm = "") {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    return(numeric(0))
  }
  if (nm %in% volatile) {
    return(numeric(0))
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (is.list(x)) {
    ns <- names(x)
    if (is.null(ns)) ns <- rep("", length(x))
    return(unlist(lapply(seq_along(x), function(i) num_leaves(x[[i]], ns[[i]])),
      use.names = FALSE
    ))
  }
  numeric(0)
}
nums_from_text <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = " ")
  # Strip ISO date/time stamps so their digits never enter the number stream
  # (the only run-to-run nondeterminism is embedded wall-clock timestamps).
  txt <- gsub(
    "[0-9]{4}-[0-9]{2}-[0-9]{2}([ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?)?",
    " ", txt
  )
  m <- regmatches(txt, gregexpr("-?[0-9]*\\.?[0-9]+(e[-+]?[0-9]+)?", txt,
    ignore.case = TRUE
  ))[[1]]
  suppressWarnings(as.numeric(m))
}
cmp_num <- function(a, b) {
  if (length(a) != length(b)) {
    return(sprintf("length %d vs %d", length(a), length(b)))
  }
  if (length(a) == 0) {
    return(NA_character_)
  }
  # Equal if: identical value (Inf==Inf, -Inf==-Inf via ==), both real NA,
  # both NaN, or finite within TOL.
  eq <- (a == b) # Inf==Inf TRUE; NA/NaN -> NA
  both_na <- is.na(a) & is.na(b) & !is.nan(a) & !is.nan(b)
  both_nan <- is.nan(a) & is.nan(b)
  fin <- is.finite(a) & is.finite(b)
  ok <- (!is.na(eq) & eq) | both_na | both_nan |
    (fin & suppressWarnings(abs(a - b)) <= TOL)
  bad <- which(!ok)
  if (length(bad)) {
    i <- bad[[1]]
    return(sprintf(
      "%d/%d differ; first @%d: %.17g vs %.17g",
      length(bad), length(a), i, a[[i]], b[[i]]
    ))
  }
  NA_character_
}

fa <- list_rel(A)
fb <- list_rel(B)
only_a <- setdiff(fa, fb)
only_b <- setdiff(fb, fa)
common <- intersect(fa, fb)

diffs <- list()
for (f in common) {
  pa <- file.path(A, f)
  pb <- file.path(B, f)
  ext <- tolower(tools::file_ext(f))
  res <- tryCatch(
    {
      if (ext == "csv") {
        da <- read.csv(pa, stringsAsFactors = FALSE)
        db <- read.csv(pb, stringsAsFactors = FALSE)
        na <- intersect(names(da), names(db))
        msg <- NA_character_
        for (cn in na) {
          if (is.numeric(da[[cn]]) || is.numeric(db[[cn]])) {
            m <- cmp_num(
              suppressWarnings(as.numeric(da[[cn]])),
              suppressWarnings(as.numeric(db[[cn]]))
            )
            if (!is.na(m)) {
              msg <- paste0("col ", cn, ": ", m)
              break
            }
          }
        }
        msg
      } else if (ext == "rds") {
        cmp_num(num_leaves(readRDS(pa)), num_leaves(readRDS(pb)))
      } else if (ext %in% c("tex", "txt", "html", "htm", "md")) {
        cmp_num(nums_from_text(pa), nums_from_text(pb))
      } else {
        NA_character_
      }
    },
    error = function(e) paste0("PARSE-ERROR: ", conditionMessage(e))
  )
  if (!is.na(res)) diffs[[f]] <- res
}

cat("=== VALUE DIFF:", A, "vs", B, "===\n")
cat(
  "common files compared:", length(common),
  " | only in A:", length(only_a), " | only in B:", length(only_b), "\n\n"
)
if (length(only_a)) {
  cat("ONLY IN A (", length(only_a), "):\n")
  cat(" ", only_a, sep = "\n  ")
  cat("\n")
}
if (length(only_b)) {
  cat("ONLY IN B (", length(only_b), "):\n")
  cat(" ", only_b, sep = "\n  ")
  cat("\n")
}
if (length(diffs)) {
  cat("\nNUMERIC DIFFERENCES (", length(diffs), " files):\n")
  for (f in names(diffs)) cat(sprintf("  %-70s %s\n", f, diffs[[f]]))
} else {
  cat("\nNUMERIC: all", length(common), "common files VALUE-IDENTICAL (tol=0).\n")
}
