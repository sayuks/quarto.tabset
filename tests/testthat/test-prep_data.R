# Test data
data <- data.frame(
  group1 = c("A", "A", "B", "B"),
  group2 = c("X", "Y", "X", "Y"),
  group3 = c("a", "b", "c", "d"),
  value = c(1, 2, 3, 4),
  stringsAsFactors = TRUE
)

test_that("prep_data orders and adds start/end columns correctly", {
  output <- prep_data(data, c("group1", "group2"), "value")

  # Expected output after prep_data
  expected_output <- data.frame(
    group1 = rep(c("A", "B"), each = 2L),
    group2 = rep(c("X", "Y"), 2),
    group3 = factor(c("a", "b", "c", "d")),
    value = seq(1, 4, by = 1),
    tabset1_start__ = c(TRUE, FALSE, FALSE, FALSE),
    tabset1_end__ = c(FALSE, FALSE, FALSE, TRUE),
    tabset2_start__ = rep(c(TRUE, FALSE), 2),
    tabset2_end__ = rep(c(FALSE, TRUE), 2)
  )

  expect_equal(output, expected_output)
})

test_that("prep_data keeps original rownames if they are not sequential", {
  data_with_rownames <- data
  rownames(data_with_rownames) <- c("a", "b", "c", "d")
  output <- prep_data(data_with_rownames, c("group1", "group2"), c("value"))
  expect_equal(rownames(output), rownames(data_with_rownames))
})

test_that("prep_data treats fator correctly", {
  data_with_rownames <- data
  rownames(data_with_rownames) <- c("a", "b", "c", "d")
  output <- prep_data(data_with_rownames, c("group1", "group2"), c("value"))
  expect_equal(rownames(output), rownames(data_with_rownames))
})

test_that("prep_data handles single grouping variable correctly", {
  single_group_data <- data.frame(
    group1 = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = TRUE
  )
  expected_output_single_group <- data.frame(
    group1 = rep(c("A", "B"), each = 2L),
    value = seq(1, 4, by = 1),
    tabset1_start__ = c(TRUE, FALSE, FALSE, FALSE),
    tabset1_end__ = c(FALSE, FALSE, FALSE, TRUE)
  )
  output <- prep_data(single_group_data, "group1", "value")
  expect_equal(output, expected_output_single_group)
})
