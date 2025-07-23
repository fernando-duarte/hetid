# Package startup functions

.onAttach <- function(libname, pkgname) {
  # Check ACM data availability
  csv_path <- system.file("extdata", "ACMTermPremium.csv", package = pkgname)

  if (csv_path == "" || !file.exists(csv_path)) {
    message_text <- "  * ACM term premia: Not found. Run download_term_premia()"
  } else {
    # Get file modification time
    file_date <- file.info(csv_path)$mtime
    message_text <- paste0(
      "  * ACM term premia: Available (updated ",
      format(file_date, "%Y-%m-%d"), ")"
    )
  }

  # Display startup message
  packageStartupMessage(
    "Data availability:\n",
    message_text,
    "\n\nUse load_term_premia() to access the data."
  )
}
