.onAttach <- function(libname, pkgname) {
  # Resolve once: availability and the mtime lookup share the path
  csv_path <- get_acm_data_path()
  if (file.exists(csv_path)) {
    file_date <- file.info(csv_path)$mtime
    message_text <- paste0(
      "  * ACM term premia: Available (updated ",
      format(file_date, HETID_CONSTANTS$ISO_DATE_FORMAT), ")"
    )
  } else {
    message_text <- "  * ACM term premia: Not found. Run download_term_premia()"
  }

  packageStartupMessage(
    "Data availability:\n",
    message_text,
    "\n\nUse load_term_premia() to access the data."
  )
}
