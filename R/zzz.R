# Package startup functions

.onAttach <- function(libname, pkgname) {
  # Check if term premia data exists
  csv_path <- system.file("extdata", "ACMTermPremium.csv", package = pkgname)

  if (csv_path == "" || !file.exists(csv_path)) {
    packageStartupMessage(
      "Note: ACM term premia data not found. ",
      "Run download_term_premia() to download the latest data from NY Fed."
    )
  } else {
    # Get file modification time
    file_date <- file.info(csv_path)$mtime
    packageStartupMessage(
      "ACM term premia data available (last updated: ",
      format(file_date, "%Y-%m-%d"), "). ",
      "Use load_term_premia() to load the data."
    )
  }
}
