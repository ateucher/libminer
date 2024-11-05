test_that("lib_summary returns expected results with defaults", {
  result <- lib_summary()

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_gte(nrow(result), 1)
  expect_equal(names(result), c("Library", "n_packages"))
  expect_type(result$n_packages, "integer")
  expect_type(result$Library, "character")
})

test_that("sizes works", {
  result <- lib_summary(sizes = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_gte(nrow(result), 1)
  expect_equal(names(result), c("Library", "n_packages", "lib_size"))
  expect_type(result$n_packages, "integer")
  expect_type(result$Library, "character")
  expect_type(result$lib_size, "double")
})

test_that("lib_summary fails appropriately", {
  expect_error(lib_summary("hello"), "must be logical")
})
