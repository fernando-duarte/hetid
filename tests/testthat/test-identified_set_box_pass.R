test_that("node_on_boundary classifies just-inside, on, and just-outside nodes", {
  tol <- HETID_CONSTANTS$BOX_BOUNDARY_TOLERANCE
  for (half in c(1e-3, 1, 1e3)) {
    expect_true(hetid:::node_on_boundary(half, half))
    expect_true(hetid:::node_on_boundary(-half, half))
    expect_true(hetid:::node_on_boundary(half * (1 + tol / 2), half))
    expect_false(hetid:::node_on_boundary(half * (1 - 10 * tol), half))
  }
  # the production call shape: elementwise over coordinates, per-coordinate half-width
  expect_identical(
    hetid:::node_on_boundary(c(1, 2 * (1 - 10 * tol)), c(1, 2)),
    c(TRUE, FALSE)
  )
  expect_identical(hetid:::node_on_boundary(numeric(0), numeric(0)), logical(0))
})
