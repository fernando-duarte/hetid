# Redirect the per-user R data directory to a temporary location for the
# entire test run so tests never read or write the real user cache and
# always resolve the bundled ACM data unless they populate a cache of
# their own. Individual tests may still override R_USER_DATA_DIR locally.
withr::local_envvar(
  R_USER_DATA_DIR = withr::local_tempdir(.local_envir = teardown_env()),
  .local_envir = teardown_env()
)
