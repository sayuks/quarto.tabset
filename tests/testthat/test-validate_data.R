test_that("validate_data", {
  # Test 1: data must be a data frame
  expect_error(validate_data(1, "a", "b"), "must be a data frame")
  expect_error(validate_data(list(a = 1), "a", "b"), "must be a data frame")
  expect_no_error(validate_data(data.frame(a = 1, b = 1), "a", "b"))

  # Test 2: data must have one or more rows
  expect_error(validate_data(data.frame(a = numeric(0), b = character(0)), "a", "b"), "must have one or more rows")

  # Test 3: data must have two or more columns
  expect_error(validate_data(data.frame(a = 1), "a", "b"), "must have two or more columns")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b"))

  # Test 4: layout must be length 1 and character
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", layout = 1), "must be character")
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", layout = c("a", "b")), "must be length 1")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b", layout = "::::a"))

  # Test 5: layout must begin with at least three or more repetitions of ":"
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", layout = "a"), "must begin with at least three or more repetitions of \":\"")

  # Test 6: heading_levels must be numeric
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = "a"), "must be numeric")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = 1))

  # Test 7: heading_levels must be length 1 or greater
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = numeric(0)), "must be length 1 or greater")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = 1))

  # Test 8: heading_levels must not include NaN
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = NaN), "must not include NaN")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = 1))

  # Test 9: heading_levels must not be infinite
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = Inf), "must not be infinite")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = 1))

  # Test 10: tabset_vars must be of length 1 or more
  expect_error(validate_data(data.frame(a = 1, b = 2), NULL, "b"), "must be of length 1 or more")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b"))

  # Test 11: tabset_vars must not contain list columns
  expect_error(validate_data(tibble::tibble(a = list(1), b = 2), "a", "b"), "must not contain list columns")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b"))

  # Test 12: heading_levels and tabset_vars must have the same length
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = c(1, 2)), "must be the same")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b", heading_levels = 1))

  # Test 13: output_vars must be of length 1 or more
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", NULL), "must be of length 1 or more")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b"))

  # Test 14: there must not be variables that are included in both tabset_vars and output_vars
  expect_error(validate_data(data.frame(a = 1, b = 2), "a", "a"), " must not be variables that are included in both")
  expect_no_error(validate_data(data.frame(a = 1, b = 2), "a", "b"))
})
