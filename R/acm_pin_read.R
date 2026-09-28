# Validate bytes before parsing; pinned inputs require complete, usable dates
read_acm_pin_data <- function(path, pin) {
  if (!acm_pin_digest_matches(path, pin)) {
    acm_pin_error(pin, "sha256 mismatch or missing asset")
  }
  result <- tryCatch(
    {
      value <- read.csv(path, stringsAsFactors = FALSE)
      validate_acm_schema(value, path)
      date_col <- intersect(c("DATE", "date"), names(value))[1]
      value[[date_col]] <- parse_and_warn_dates(value[[date_col]], date_col)
      names(value)[names(value) == date_col] <- "date"
      if (nrow(value) == 0L || anyNA(value$date)) {
        stop_hetid("Pinned ACM data must have nonempty, fully parseable dates")
      }
      value
    },
    error = function(e) acm_pin_error(pin, conditionMessage(e)),
    warning = function(w) acm_pin_error(pin, conditionMessage(w))
  )
  result
}

acm_pin_metadata <- function(pin, acquisition) {
  c(
    release = pin$release, sha256 = pin$sha256, frequency = pin$frequency,
    source_url = pin$source_url,
    retrieved = format(Sys.time(), HETID_CONSTANTS$ISO_TIMESTAMP_FORMAT, tz = "UTC"),
    acquisition = acquisition
  )
}

validate_acm_pin_metadata <- function(pin, path = pin$path) {
  meta <- tryCatch(
    read.dcf(paste0(path, ".meta")),
    error = function(e) acm_pin_error(pin, "missing or unreadable provenance"),
    warning = function(w) acm_pin_error(pin, "unreadable provenance")
  )
  required <- c("release", "sha256", "frequency", "source_url", "retrieved", "acquisition")
  if (nrow(meta) != 1L || !all(required %in% colnames(meta))) {
    acm_pin_error(pin, "incomplete provenance")
  }
  if (anyNA(meta[1, required]) || any(!nzchar(meta[1, required]))) {
    acm_pin_error(pin, "missing provenance fields")
  }
  expected <- acm_pin_metadata(pin, "download")
  identity_fields <- c("release", "sha256", "frequency")
  if (!identical(unname(meta[1, identity_fields]), unname(expected[identity_fields])) ||
    !meta[1, "acquisition"] %in% c("download", "verified-existing-asset")) {
    acm_pin_error(pin, "provenance does not match the requested pin")
  }
  invisible(TRUE)
}

read_acm_pin <- function(pin) {
  validate_acm_pin_metadata(pin)
  read_acm_pin_data(pin$path, pin)
}

load_acm_pin <- function(pin, auto_download) {
  if (!dir.exists(dirname(pin$path))) {
    if (!auto_download) {
      stop_insufficient_data(paste0(
        "Pinned ACM snapshot not found. Use auto_download = TRUE or ",
        "download_term_premia() with the same release and expected_sha256."
      ))
    }
    download_acm_pin(pin, force = FALSE, quiet = FALSE)
  }
  read_acm_pin(pin)
}
