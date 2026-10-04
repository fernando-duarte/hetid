test_that("fixture parts reproduce the original compressed and serialized bytes", {
  paths <- test_path("fixtures", paste0("fitted-volatility-endpoint-oracle.part-", 1:2, ".rds"))
  expect_true(all(file.info(paths)$size < 1000 * 1024))
  bytes <- do.call(c, lapply(paths, function(path) readBin(path, "raw", n = file.info(path)$size)))
  files <- c(tempfile(), tempfile())
  on.exit(unlink(files))
  writeBin(bytes, files[1L])
  writeBin(memDecompress(bytes, "gzip"), files[2L])
  expect_identical(unname(tools::sha256sum(files)), c(
    "87bfe9bc07d7e996f037d6b4207f5aedf7906d2a9e865f57f40f5f5ab80b316a",
    "022c44cc778a047587af684e81efa5bba6758dc680caf92af63148a64970401b"
  ))
  oracle <- read_fitted_volatility_endpoint_oracle()
  expect_identical(oracle$ppml$complete, TRUE)
  expect_identical(oracle$harvey$complete, TRUE)
})

test_that("PPML and Harvey envelopes preserve exact independent donor histories", {
  oracle <- read_fitted_volatility_endpoint_oracle()
  for (method in c("ppml", "harvey")) {
    expected <- oracle[[method]]
    actual <- fv_test_replay(expected)
    expect_identical(actual$sets$sample$prep$w1_mean, expected$input$sample$w1)
    expect_identical(actual$sets$sample$prep$w2_mean, expected$input$sample$w2)
    expect_oracle_equal(
      actual$sets$sample$x_mat, expected$sample$x_mat, ORACLE_TOLERANCE[["direct"]]
    )
    expect_identical(actual$sets$sample$w1, expected$sample$w1)
    expect_identical(as.vector(actual$sets$sample$w2), as.vector(expected$sample$w2))
    expect_identical(dim(actual$sets$sample$w2), dim(expected$sample$w2))
    expect_identical(dimnames(actual$sets$sample$w2), dimnames(expected$sample$w2))
    expect_identical(actual$sets$sample$response_date, expected$sample$response_qtr)
    expect_identical(names(actual$envelopes), names(expected$records))
    for (key in names(expected$records)) {
      envelope <- actual$envelopes[[key]]
      record <- expected$records[[key]]
      data_fields <- setdiff(names(record$envelope$data), "qtr")
      expect_oracle_equal(
        lv_drop_route(envelope$data[data_fields]),
        lv_drop_route(record$envelope$data[data_fields]), ORACLE_TOLERANCE[["solver"]]
      )
      schema_fields <- setdiff(names(record$schema), "sample_id")
      expect_oracle_equal(
        lv_drop_route(envelope$schema[schema_fields]),
        lv_drop_route(record$schema[schema_fields]), ORACLE_TOLERANCE[["solver"]]
      )
      expect_identical(
        envelope$schema$sample_id,
        rep(actual$sets$sample$sample_id, nrow(envelope$schema))
      )
      expect_identical(envelope$point_status, record$envelope$point_status)
      expect_identical(lv_drop_route(envelope$diagnostics$source), lv_drop_route(
        record$source_budget[names(envelope$diagnostics$source)]
      ))
      expect_effort_consistent(envelope$diagnostics$source)
      engine_fields <- names(record$target$diagnostics)
      expect_identical(
        lv_drop_route(envelope$diagnostics$engine[engine_fields]),
        lv_drop_route(record$target$diagnostics)
      )
      expect_effort_consistent(envelope$diagnostics$engine)
      for (i in 1:2) {
        scale <- c("variance", "volatility")[i]
        for (side in c("lower", "upper", "point")) {
          expect_oracle_equal(
            envelope$data[[paste0(scale, "_", side)]],
            expected$levels[[key]][[i]][[paste0("log_variance_", side)]],
            ORACLE_TOLERANCE[["solver"]]
          )
        }
      }
    }
    fields <- names(expected$stages$grid$value)
    expect_oracle_equal(
      lv_drop_route(lv_test_core(actual$path[fields])),
      lv_drop_route(lv_test_core(expected$stages$grid$value)), ORACLE_TOLERANCE[["solver"]]
    )
    expect_effort_consistent(actual$path)
  }
})
