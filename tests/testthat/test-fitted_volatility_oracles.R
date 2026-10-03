test_that("fixture parts reproduce the original compressed and serialized bytes", {
  paths <- test_path("fixtures", paste0("fitted-volatility-endpoint-oracle.part-", 1:2, ".rds"))
  expect_true(all(file.info(paths)$size < 1000 * 1024))
  bytes <- do.call(c, lapply(paths, function(path) readBin(path, "raw", n = file.info(path)$size)))
  files <- c(tempfile(), tempfile())
  on.exit(unlink(files))
  writeBin(bytes, files[1L])
  writeBin(memDecompress(bytes, "gzip"), files[2L])
  expect_identical(unname(tools::sha256sum(files)), c(
    "c1046c3145e6c3c5d489fcfdcccc7aac756a50b73a69886d17a6905439997ea2",
    "e30f7331c92ec342ae31324f9e5020d4a43ce1258bfe7ec4742ac25d492faf3e"
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
    expect_identical(actual$sets$sample$x_mat, expected$sample$x_mat)
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
      expect_identical(envelope$data[data_fields], record$envelope$data[data_fields])
      schema_fields <- setdiff(names(record$schema), "sample_id")
      expect_identical(envelope$schema[schema_fields], record$schema[schema_fields])
      expect_identical(
        envelope$schema$sample_id,
        rep(actual$sets$sample$sample_id, nrow(envelope$schema))
      )
      expect_identical(envelope$point_status, record$envelope$point_status)
      expect_identical(envelope$diagnostics$source, record$source_budget[
        names(envelope$diagnostics$source)
      ])
      engine_fields <- names(record$target$diagnostics)
      expect_identical(envelope$diagnostics$engine[engine_fields], record$target$diagnostics)
      for (i in 1:2) {
        scale <- c("variance", "volatility")[i]
        for (side in c("lower", "upper", "point")) {
          expect_identical(
            envelope$data[[paste0(scale, "_", side)]],
            expected$levels[[key]][[i]][[paste0("log_variance_", side)]]
          )
        }
      }
    }
    cache <- actual$sets$cache$store
    expect_identical(
      sort(ls(cache, all.names = TRUE), method = "radix"),
      sort(expected$final_sets$cache$keys, method = "radix")
    )
    expect_identical(
      as.list(cache)[expected$final_sets$cache$keys],
      expected$final_sets$cache$fits
    )
    fields <- names(expected$stages$grid$value)
    expect_identical(
      lv_test_core(actual$path[fields]),
      lv_test_core(expected$stages$grid$value)
    )
  }
})
