local({
  case <- cache_case()
  old <- list(version = "old")
  saveRDS(old, case$path, version = 3L)
  installed_read <- FALSE
  reader <- function(path) {
    if (identical(path, case$path) && installed_read) {
      installed_read <<- FALSE
      stop("post-promotion read failed")
    }
    readRDS(path)
  }
  promoter <- function(from, to) {
    promoted <- file.rename(from, to)
    installed_read <<- isTRUE(promoted)
    promoted
  }
  failure <- tryCatch(
    paper_boot_transactional_replace(
      list(version = "new"), case$path, case$validator,
      reader, case$writer, promoter
    ),
    error = conditionMessage
  )
  check(
    "post-promotion failure restores the prior valid cache",
    identical(failure, "post-promotion read failed") &&
      identical(readRDS(case$path), old)
  )
})

local({
  case <- cache_case()
  old <- list(version = "old")
  saveRDS(old, case$path, version = 3L)
  installed_read <- FALSE
  copy_calls <- 0L
  reader <- function(path) {
    if (identical(path, case$path) && installed_read) {
      installed_read <<- FALSE
      stop("post-promotion read failed")
    }
    readRDS(path)
  }
  promoter <- function(from, to) {
    promoted <- file.rename(from, to)
    installed_read <<- isTRUE(promoted)
    promoted
  }
  copier <- function(from, to, overwrite) {
    copy_calls <<- copy_calls + 1L
    if (copy_calls == 1L) {
      return(file.copy(from, to, overwrite = overwrite))
    }
    FALSE
  }
  failure <- tryCatch(
    paper_boot_transactional_replace(
      list(version = "new"), case$path, case$validator,
      reader, case$writer, promoter,
      copier = copier
    ),
    error = conditionMessage
  )
  backups <- cache_backups(case$path)
  check(
    "failed recovery retains the prior valid backup",
    grepl("prior cache recovery failed", failure, fixed = TRUE) &&
      length(backups) == 1L &&
      identical(readRDS(backups[[1L]]), old)
  )
})

local({
  case <- cache_case()
  old <- list(version = "old")
  saveRDS(old, case$path, version = 3L)
  installed_read <- FALSE
  copy_calls <- 0L
  reader <- function(path) {
    if (identical(path, case$path) && installed_read) {
      installed_read <<- FALSE
      stop("post-promotion read failed")
    }
    readRDS(path)
  }
  promoter <- function(from, to) {
    promoted <- file.rename(from, to)
    installed_read <<- isTRUE(promoted)
    promoted
  }
  copier <- function(from, to, overwrite) {
    copy_calls <<- copy_calls + 1L
    if (copy_calls == 1L) {
      return(file.copy(from, to, overwrite = overwrite))
    }
    writeBin(as.raw(1:4), to)
    stop("recovery copy crashed")
  }
  failure <- tryCatch(
    paper_boot_transactional_replace(
      list(version = "new"), case$path, case$validator,
      reader, case$writer, promoter,
      copier = copier
    ),
    error = conditionMessage
  )
  backups <- cache_backups(case$path)
  check(
    "recovery exceptions retain the prior valid backup",
    grepl("prior cache recovery failed", failure, fixed = TRUE) &&
      length(backups) == 1L &&
      identical(readRDS(backups[[1L]]), old)
  )
})
