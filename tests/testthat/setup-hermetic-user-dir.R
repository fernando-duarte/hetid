# Point R_USER_DATA_DIR at a temp dir for the whole run so tests never
# touch the real user cache; individual tests may still override it
withr::local_envvar(
  R_USER_DATA_DIR = withr::local_tempdir(.local_envir = teardown_env()),
  .local_envir = teardown_env()
)
