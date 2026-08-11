# Tell the macOS File Provider (Dropbox) to stop syncing a generated file.
#
# The repository lives under synced Dropbox, which races LaTeX over its own
# build artifacts: it can hold a file open mid-build, and it can put one back
# after the build deletes it. That is how sidecars reappear beside the published
# tables after a run that cleaned up and reported success. Flagging a file makes
# Dropbox let go of it, so there is nothing left to race.
#
# This is the mechanism the paper repository's latexmkrc already uses on every
# file it generates; see that folder's own notes for the history, including why
# the legacy com.dropbox.ignored attribute is not enough on the current client
# and why the flag has to be re-applied each run -- a newly created file starts
# unflagged, so the root rules.dropboxignore cannot be relied on alone.
#
# Best-effort by construction. A path can vanish between being listed and being
# flagged, which is the outcome wanted anyway, and the attribute means nothing
# off macOS. Deleting the sidecars and stopping when any survive stays the hard
# guarantee; flagging only removes the reason they come back.

PAPER_FILEPROVIDER_IGNORE_ATTR <- "com.apple.fileprovider.ignore#P"

#' Flag paths so the macOS File Provider stops syncing them
#'
#' @param paths character vector of paths; missing ones are skipped
#' @return invisibly, the paths that were flagged (empty off macOS or when the
#'   xattr binary is absent)
paper_flag_fileprovider_ignored <- function(paths) {
  paths <- paths[file.exists(paths)]
  darwin <- identical(unname(Sys.info()[["sysname"]]), "Darwin")
  if (!length(paths) || !darwin || !nzchar(Sys.which("xattr"))) {
    return(invisible(character(0)))
  }
  for (path in paths) {
    # the attribute name carries a '#', so every argument is quoted
    system2(
      "xattr",
      c(
        "-w",
        shQuote(PAPER_FILEPROVIDER_IGNORE_ATTR),
        "1",
        shQuote(path)
      ),
      stdout = FALSE,
      stderr = FALSE
    )
  }
  invisible(paths)
}
