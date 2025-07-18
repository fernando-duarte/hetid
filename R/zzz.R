# Package startup functions

.onAttach <- function(libname, pkgname) {
  # Data sources to check
  data_sources <- list(
    list(
      name = "ACM term premia",
      file = "ACMTermPremium.csv",
      download_fn = "download_term_premia()",
      load_fn = "load_term_premia()"
    ),
    list(
      name = "Fed yield curve (Svensson)",
      file = "feds200628.csv",
      download_fn = "download_yield_curve('feds200628')",
      load_fn = "load_yield_curve('feds200628')"
    ),
    list(
      name = "Fed yield curve (smoothed)",
      file = "feds200533.csv",
      download_fn = "download_yield_curve('feds200533')",
      load_fn = "load_yield_curve('feds200533')"
    )
  )

  # Check each data source
  messages <- list()
  for (source in data_sources) {
    csv_path <- system.file("extdata", source$file, package = pkgname)

    if (csv_path == "" || !file.exists(csv_path)) {
      messages[[length(messages) + 1]] <- paste0(
        "  * ", source$name, ": Not found. Run ", source$download_fn
      )
    } else {
      # Get file modification time
      file_date <- file.info(csv_path)$mtime
      messages[[length(messages) + 1]] <- paste0(
        "  * ", source$name, ": Available (updated ",
        format(file_date, "%Y-%m-%d"), ")"
      )
    }
  }

  # Display startup message
  packageStartupMessage(
    "Data availability:\n",
    paste(messages, collapse = "\n"),
    "\n\nUse the corresponding load_*() functions to access the data."
  )
}
