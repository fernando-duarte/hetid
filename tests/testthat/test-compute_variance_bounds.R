test_that("the bundled maturity composition preserves the donor summary fixture", {
  withr::local_envvar(c(R_USER_DATA_DIR = withr::local_tempdir()))
  expect_warning(acm <- extract_acm_data(
    maturities = HETID_CONSTANTS$ALL_ACM_MATURITIES, frequency = "quarterly",
    use_incomplete_quarters = TRUE, auto_download = FALSE
  ), class = "hetid_warning_incomplete_quarter")
  yields <- as.matrix(acm[acm_column_name("yields", HETID_CONSTANTS$ALL_ACM_MATURITIES)])
  tp <- as.matrix(acm[acm_column_name("term_premia", HETID_CONSTANTS$ALL_ACM_MATURITIES)])
  result <- compute_variance_bounds(yields, tp)
  frame <- result$per_maturity
  expect_identical(frame$Maturity, seq.int(3L, 117L, 3L))
  expect_identical(names(frame), c(
    "Maturity", "Variance_Bound", "Expected_SDF_Bound",
    "News_Envelope_Bound", "News_Q_Bound"
  ))
  expect_identical(frame$Variance_Bound, pmin(frame$News_Envelope_Bound, frame$News_Q_Bound))
  expect_identical(frame$Maturity[frame$News_Q_Bound < frame$News_Envelope_Bound], 3L)
  reference <- matrix(c(
    2.3065730332906108e-9, 2.5670706300547337e-9, 1.0646231692999857e-10,
    4.1878247953933092e-9, 1.2107073730974770e-9,
    1.8136123519860909e-9, 1.9728416718313524e-9, 1.0551259629957454e-10,
    3.1936706474892579e-9, 9.1376372602717717e-10
  ), nrow = 5L, dimnames = list(
    c("Mean", "Median", "Minimum", "Maximum", "Standard Deviation"),
    c("SDF news", "Expected SDF")
  ))
  expect_equal(result$summary, reference, tolerance = 1e-13)
})

test_that("synthetic bound arms agree with independently reduced primitive formulas", {
  y12 <- c(1, 4, 9, 2, 7, 5, 3, 8)
  yields <- data.frame(y12 = y12, y24 = numeric(8), y36 = numeric(8))
  tp <- data.frame(tp12 = numeric(8), tp24 = numeric(8), tp36 = numeric(8))
  result <- compute_variance_bounds(yields, tp, step = 12L, maturities = 24L)
  frame <- result$per_maturity
  x <- -y12[3:8] / 100
  n1 <- y12[2:7] / 100
  q0 <- expm1(x) - x
  q1 <- exp(n1) * (expm1(x - n1) - (x - n1))
  g <- expm1(n1) - n1 - n1^2 / 2
  sd_population <- function(z) sqrt(sum((z - mean(z))^2) / length(z))
  news_q <- (sd_population(q1) + sd_population(q0) + sd_population(g))^2
  envelope <- 0.25 * (mean((x - n1)^4) + mean(n1^4))
  expected <- min(0.25 * mean(x^4), sd_population(q0)^2)
  expect_equal(frame$News_Envelope_Bound, envelope, tolerance = 1e-14)
  expect_equal(frame$News_Q_Bound, news_q, tolerance = 1e-14)
  expect_equal(frame$Expected_SDF_Bound, expected, tolerance = 1e-14)
  expect_equal(frame$Variance_Bound, min(envelope, news_q), tolerance = 1e-14)
  expect_identical(
    result$summary["Standard Deviation", ],
    c("SDF news" = NA_real_, "Expected SDF" = NA_real_)
  )
})

test_that("component call order and all five summary statistics remain explicit", {
  calls <- character()
  local_mocked_bindings(
    compute_variance_bound = function(yields, term_premia, i, step) {
      calls <<- c(calls, paste0("envelope", i))
      c(4, 8, 12)[i]
    },
    compute_news_q_bound = function(yields, term_premia, i, step) {
      calls <<- c(calls, paste0("q", i))
      c(1, 20, 6)[i]
    },
    compute_expected_sdf_variance_bound = function(yields, term_premia, i, step) {
      calls <<- c(calls, paste0("expected", i))
      c(2, 4, 6)[i]
    }
  )
  result <- compute_variance_bounds(matrix(0, 4, 1), matrix(0, 4, 1), 1L, 1:3)
  expect_identical(calls, c(paste0("envelope", 1:3), paste0("q", 1:3), paste0("expected", 1:3)))
  expected <- cbind(
    "SDF news" = c(
      Mean = 5, Median = 6, Minimum = 1, Maximum = 8,
      "Standard Deviation" = sqrt(13)
    ),
    "Expected SDF" = c(
      Mean = 4, Median = 4, Minimum = 2, Maximum = 6,
      "Standard Deviation" = 2
    )
  )
  expect_identical(result$summary, expected)
  reordered <- compute_variance_bounds(matrix(0, 4, 1), matrix(0, 4, 1), 1L, c(3L, 1L))
  expect_identical(reordered$per_maturity$Maturity, c(3L, 1L))
  expect_identical(reordered$per_maturity$Variance_Bound, c(6, 1))
})
