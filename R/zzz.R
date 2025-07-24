# Package startup functions

.onAttach <- function(libname, pkgname) {
  # Check ACM data availability using standardized path management
  if (check_data_file_exists("ACMTermPremium.csv")) {
    # Get file modification time
    csv_path <- get_acm_data_path()
    file_date <- file.info(csv_path)$mtime
    message_text <- paste0(
      "  * ACM term premia: Available (updated ",
      format(file_date, HETID_CONSTANTS$ISO_DATE_FORMAT), ")"
    )
  } else {
    message_text <- "  * ACM term premia: Not found. Run download_term_premia()"
  }

  # Display startup message
  packageStartupMessage(
    "Data availability:\n",
    message_text,
    "\n\nUse load_term_premia() to access the data."
  )
}
