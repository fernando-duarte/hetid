# Validate an explicit release and caller-supplied digest before touching the cache.
acm_pin <- function(release, expected_sha256, source, frequency) {
  if (is.null(release) && is.null(expected_sha256)) {
    return(NULL)
  }
  assert_acm_pin_string(release, "release")
  assert_acm_pin_string(expected_sha256, "expected_sha256")
  assert_bad_argument_ok(
    nzchar(release) && !grepl("[[:space:][:cntrl:]]", release),
    "release must be a nonempty tag without whitespace or control characters",
    arg = "release"
  )
  assert_bad_argument_ok(
    grepl("^[0-9a-fA-F]{64}$", expected_sha256),
    "expected_sha256 must be a 64-character hexadecimal digest",
    arg = "expected_sha256"
  )
  assert_bad_argument_ok(
    source != "nyfed", "Pinned releases require the GitHub source",
    arg = "source"
  )
  release <- unname(enc2utf8(release))
  sha <- unname(tolower(expected_sha256))
  # One full digest keeps paths short without truncating the identity hash.
  key <- acm_pin_key(release, sha, frequency)
  filename <- acm_asset_filename("github", frequency)
  list(
    release = release, sha256 = sha, frequency = frequency,
    source_url = paste0(
      ACM_RELEASE_URL, "download/", utils::URLencode(release, reserved = TRUE, repeated = TRUE),
      "/", filename
    ),
    path = file.path(get_user_data_dir(), "releases", key, filename)
  )
}

# Pin failures name the immutable snapshot, which is never silently overwritten.
acm_pin_error <- function(pin, detail) {
  recovery <- if (dir.exists(dirname(pin$path))) {
    " Remove that snapshot directory before retrying if it is corrupt."
  } else {
    " The snapshot was not published."
  }
  stop_hetid(paste0("Pinned ACM snapshot at ", dirname(pin$path), ": ", detail, ".", recovery))
}

acm_pin_digest_matches <- function(path, pin) {
  file.exists(path) && !dir.exists(path) &&
    identical(tolower(unname(tools::sha256sum(path))), pin$sha256)
}

assert_acm_pin_string <- function(value, arg) {
  assert_bad_argument_ok(
    is.character(value) && length(value) == 1L && is.null(dim(value)) && !is.na(value),
    paste0(arg, " must be a single nonmissing character string"),
    arg = arg
  )
}

acm_pin_key <- function(release, sha, frequency) {
  key_file <- tempfile("acm_key_")
  on.exit(unlink(key_file), add = TRUE)
  tryCatch(
    {
      writeBin(charToRaw(paste(release, sha, frequency, sep = "\n")), key_file)
      key <- unname(tools::sha256sum(key_file))
      if (is.na(key)) {
        stop_hetid("Could not hash the ACM snapshot identity")
      }
      key
    },
    error = function(e) stop_hetid(conditionMessage(e)),
    warning = function(w) stop_hetid(conditionMessage(w))
  )
}
