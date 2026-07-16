# Fixtures for the digest-verified GitHub download flow: gz asset,
# release-API JSON, and a download.file mock; no real network

# Build a gz fixture plus matching release-API JSON; the digest can be
# corrupted, the JSON malformed, or the asset renamed per scenario
make_github_fixture <- function(dir, digest = NULL, json = NULL,
                                asset_name = HETID_CONSTANTS$ACM_DATA_FILENAME) {
  gz_path <- file.path(dir, "fixture.csv.gz")
  con <- gzfile(gz_path, "wb")
  writeLines(c("DATE,ACMY01", "2020-01-31,1.5"), con)
  close(con)
  true_sha <- unname(tools::sha256sum(gz_path))
  if (is.null(digest)) {
    digest <- true_sha
  }
  if (is.null(json)) {
    json <- sprintf(
      paste0(
        '{"tag_name":"test-tag","name":"Test Release","assets":[',
        '{"name":"other_asset.xls","digest":"sha256:%s"},',
        '{"name":"%s","digest":"sha256:%s"}]}'
      ),
      strrep("0", 64), asset_name, digest
    )
  }
  list(gz = gz_path, sha = true_sha, json = json)
}

mock_github_download <- function(fixture) {
  function(url, destfile, ...) {
    if (grepl("api.github.com", url, fixed = TRUE)) {
      writeLines(fixture$json, destfile)
    } else {
      file.copy(fixture$gz, destfile, overwrite = TRUE)
    }
    invisible(0L)
  }
}
