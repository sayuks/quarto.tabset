data <- data.frame(
  group = c("B", "A", "A", "C"),
  sub_group = c(2, 1, 3, 1),
  output = c(10, 20, 30, 40)
)

test_that("select, order, converts factors to characters", {
  result <- prep_data(data, tabset_names = "group", output_names = "output")
  expect_equal(names(result), c("group", "output"))
  expect_equal(result$group, c("A", "A", "B", "C"))
  expect_equal(result$output, c(20, 30, 10, 40))
  expect_type(result$group, "character")
})

test_that("prep_data handles multiple sorting columns", {
  result <- prep_data(
    data,
    tabset_names = c("group", "sub_group"),
    output_names = "output"
  )

  expected_order <- order(data$group, data$sub_group)
  expect_equal(result$group, data$group[expected_order])
  expect_equal(result$sub_group, data$sub_group[expected_order])
})

test_that("prep_data returns error when columns are missing", {
  expect_error(
    prep_data(
      data,
      tabset_names = "non_existent",
      output_names = "output"
    ),
    "undefined columns selected"
  )
})
