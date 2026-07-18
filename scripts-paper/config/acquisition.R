# Operational FRED transport policy.

PAPER_FRED_DOWNLOAD_CONTROL <- list(
  api_key_environment = "FRED_API_KEY",
  endpoints = list(
    api = paste0(
      "https://api.stlouisfed.org/",
      "fred/series/observations"
    ),
    csv = paste0(
      "https://fred.stlouisfed.org/",
      "graph/fredgraph.csv?id="
    )
  ),
  api = list(
    attempts = 4L,
    timeout_seconds = 60L,
    backoff_seconds = 2L,
    backoff_cap_seconds = 8L
  ),
  csv = list(
    transfer_flags = "--location --http1.1",
    retries = 5L,
    retry_delay_seconds = 2L,
    retry_flags = "--retry-connrefused",
    connect_timeout_seconds = 20L,
    max_time_seconds = 180L,
    speed_limit = 1L,
    speed_time_seconds = 30L,
    output_flags = "--silent --show-error"
  )
)

stopifnot(
  PAPER_FRED_DOWNLOAD_CONTROL$api$attempts >= 1L,
  PAPER_FRED_DOWNLOAD_CONTROL$api$timeout_seconds > 0L,
  PAPER_FRED_DOWNLOAD_CONTROL$csv$retries >= 0L,
  PAPER_FRED_DOWNLOAD_CONTROL$csv$max_time_seconds > 0L
)
