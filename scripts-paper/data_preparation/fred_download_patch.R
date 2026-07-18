# Work around quantmod issues #452 and #449 (both open as of 2026-06):
#   getSymbols.FRED() downloads via read.csv(curl::curl(URL)) from the public
#   graph-CSV endpoint (fred.stlouisfed.org/graph/fredgraph.csv, served via
#   Akamai). With current libraries that endpoint exhibits two failure modes:
#     #452  the curl package's libcurl multi/streaming interface negotiates
#           HTTP/2 and fails with "HTTP/2 stream 1 was not closed cleanly:
#           INTERNAL_ERROR" -> "Unable to import <series>: cannot open the
#           connection".
#     #449  larger series stall mid-transfer (connection open, 0 bytes/sec) with
#           no transfer timeout, so the download hangs indefinitely.
#
# This patches ONLY the data-acquisition step of getSymbols.FRED; all date
# parsing / xts construction is byte-for-byte the original. tidyquant's
# tq_get(get = "economic.data") dispatches through this function, so the patch
# fixes the whole FRED pipeline.
#
# Two transports, picked at call time:
#   * If FRED_API_KEY is set (e.g. in ~/.Renviron), fetch JSON from the official
#     FRED API (api.stlouisfed.org) -- a different, reliable host that serves the
#     large daily series in a few seconds where the CSV endpoint times out.
#   * Otherwise fall back to the curl command line tool against the CSV endpoint,
#     forcing HTTP/1.1 (sidesteps #452) with retries + stall timeouts (#449).
# The API key is read from the environment and never written to logs.
# Remove this file's source() once quantmod ships a fix upstream.

local({
  fred_control <- PAPER_FRED_DOWNLOAD_CONTROL
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    return(invisible())
  }

  # Fetch one series from the FRED API as a 2-column data.frame (date, value),
  # matching the shape read.csv() produced from the CSV endpoint. Retries a few
  # times; errors carry the series id but never the URL (which holds the key).
  fred_fetch_api <- function(
    symbol,
    key,
    control = fred_control$api
  ) {
    url <- paste0(
      fred_control$endpoints$api,
      "?series_id=", utils::URLencode(symbol, reserved = TRUE),
      "&api_key=", key,
      "&file_type=json"
    )
    last_err <- NULL
    for (k in seq_len(control$attempts)) {
      tmp <- tempfile(fileext = ".json")
      on.exit(unlink(tmp), add = TRUE)
      old_to <- options(timeout = control$timeout_seconds)
      on.exit(options(old_to), add = TRUE)
      st <- tryCatch(
        suppressWarnings(utils::download.file(url, tmp, method = "libcurl", quiet = TRUE)),
        error = function(e) {
          last_err <<- "download error"
          -1L
        }
      )
      if (identical(st, 0L) && file.exists(tmp) && file.size(tmp) > 0) {
        # A truncated or non-JSON body is the transient failure this loop retries,
        # so it is caught -- but the parser's message is the only thing that says
        # which, and the stop() at exhaustion reports last_err, so keep it.
        parse_err <- NULL
        js <- tryCatch(jsonlite::fromJSON(tmp), error = function(e) {
          parse_err <<- conditionMessage(e)
          NULL
        })
        if (is.null(js)) {
          last_err <- paste(c("unparseable JSON response", parse_err), collapse = ": ")
        } else if (!is.null(js$error_code)) {
          stop("FRED API error for ", symbol, ": ", js$error_message, call. = FALSE)
        } else if (!is.null(js$observations) && NROW(js$observations) > 0) {
          obs <- js$observations
          val <- obs$value
          val[val == "."] <- NA_character_
          return(data.frame(
            date = obs$date, value = suppressWarnings(as.numeric(val)),
            stringsAsFactors = FALSE
          ))
        } else {
          last_err <- "no observations returned"
        }
      } else {
        last_err <- paste0("download status ", st)
      }
      Sys.sleep(min(
        control$backoff_seconds * k,
        control$backoff_cap_seconds
      ))
    }
    stop("FRED API fetch failed for ", symbol, " (", last_err, ")", call. = FALSE)
  }

  # Fallback: curl CLI against the CSV endpoint, HTTP/1.1 + retries + stall timeout.
  fred_curl_extra <- function(control) {
    paste(
      control$transfer_flags,
      sprintf(
        "--retry %d --retry-delay %d %s",
        control$retries,
        control$retry_delay_seconds,
        control$retry_flags
      ),
      sprintf(
        "--connect-timeout %d --max-time %d",
        control$connect_timeout_seconds,
        control$max_time_seconds
      ),
      sprintf(
        "--speed-limit %d --speed-time %d",
        control$speed_limit,
        control$speed_time_seconds
      ),
      control$output_flags
    )
  }

  fred_fetch_csv <- function(
    symbol,
    control = fred_control$csv
  ) {
    URL <- paste0(fred_control$endpoints$csv, symbol)
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    curl_extra <- fred_curl_extra(control)
    status <- utils::download.file(URL, tmp, method = "curl", extra = curl_extra, quiet = TRUE)
    if (!identical(status, 0L) || file.size(tmp) == 0) {
      stop("FRED CSV download failed for ", symbol, " (curl status ", status, ")", call. = FALSE)
    }
    read.csv(tmp, na.strings = ".")
  }

  patched_getSymbols.FRED <- function(Symbols, env, return.class = "xts", ...) {
    importDefaults("getSymbols.FRED")
    this.env <- environment()
    for (var in names(list(...))) assign(var, list(...)[[var]], this.env)
    if (!hasArg("verbose")) verbose <- FALSE
    if (!hasArg("auto.assign")) auto.assign <- TRUE
    if (!hasArg("warnings")) warnings <- TRUE
    if (!hasArg("from")) from <- ""
    if (!hasArg("to")) to <- ""
    key <- Sys.getenv(fred_control$api_key_environment)
    returnSym <- Symbols
    noDataSym <- NULL
    for (i in seq_along(Symbols)) {
      if (verbose) cat("downloading ", Symbols[[i]], ".....\n\n")
      test <- try(
        {
          # --- begin patch: replace read.csv(curl::curl(URL)) -------------------
          fr <- if (nzchar(key)) {
            fred_fetch_api(Symbols[[i]], key)
          } else {
            fred_fetch_csv(Symbols[[i]])
          }
          # --- end patch --------------------------------------------------------
          if (verbose) cat("done.\n")
          fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1], origin = "1970-01-01"),
            src = "FRED", updated = Sys.time()
          )
          dim(fr) <- c(NROW(fr), 1)
          colnames(fr) <- as.character(toupper(Symbols[[i]]))
          fr <- fr[paste(from, to, sep = "/")]
          fr <- convert.time.series(fr = fr, return.class = return.class)
          Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
          if (auto.assign) assign(Symbols[[i]], fr, env)
        },
        silent = TRUE
      )
      if (inherits(test, "try-error")) {
        msg <- paste0(
          "Unable to import ", dQuote(returnSym[[i]]), ".\n",
          attr(test, "condition")$message
        )
        if (hasArg(".has1sym.") && match.call(expand.dots = TRUE)$.has1sym.) stop(msg)
        if (isTRUE(warnings)) warning(msg, call. = FALSE, immediate. = TRUE)
        noDataSym <- c(noDataSym, returnSym[[i]])
      }
    }
    if (auto.assign) {
      return(setdiff(returnSym, noDataSym))
    }
    return(fr)
  }

  # Give the patch an environment that exposes the helpers above while still
  # resolving quantmod internals (importDefaults, convert.time.series, ...) via
  # the namespace parent.
  patch_env <- new.env(parent = asNamespace("quantmod"))
  patch_env$fred_fetch_api <- fred_fetch_api
  patch_env$fred_fetch_csv <- fred_fetch_csv
  patch_env$fred_control <- fred_control
  environment(patched_getSymbols.FRED) <- patch_env
  utils::assignInNamespace("getSymbols.FRED", patched_getSymbols.FRED, ns = "quantmod")
})
