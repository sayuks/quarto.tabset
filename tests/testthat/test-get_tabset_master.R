# Test data
data <- data.frame(
  group1 = c("A", "A", "B", "B"),
  group2 = c("X", "Y", "X", "Y"),
  group3 = c("a", "b", "c", "d"),
  value = c(1, 2, 3, 4),
  stringsAsFactors = TRUE
)

test_that("prep_data orders and adds start/end columns correctly", {
  output <- get_tabset_master(data, c("group1", "group2"))

  # Expected output after prep_data
  expected_output <- data.frame(
    tabset1_start = c(TRUE, FALSE, FALSE, FALSE),
    tabset1_end = c(FALSE, FALSE, FALSE, TRUE),
    tabset2_start = rep(c(TRUE, FALSE), 2),
    tabset2_end = rep(c(FALSE, TRUE), 2)
  )

  expect_equal(output, expected_output)
})

test_that("prep_data handles single grouping variable correctly", {
  single_group_data <- data.frame(
    group1 = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = TRUE
  )
  expected_output_single_group <- data.frame(
    tabset1_start = c(TRUE, FALSE, FALSE, FALSE),
    tabset1_end = c(FALSE, FALSE, FALSE, TRUE)
  )
  output <- get_tabset_master(single_group_data, "group1")
  expect_equal(output, expected_output_single_group)
})
