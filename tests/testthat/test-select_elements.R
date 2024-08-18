test_that("select_elements()", {
  df <- data.frame(
    x = 1,
    y = "a"
  )

  expect_equal(
    select_elements(df, x),
    "x"
  )

  expect_equal(
    select_elements(df, c(x, y)),
    c("x", "y")
  )

  expect_equal(
    select_elements(df, c(x:y)),
    c("x", "y")
  )

  expect_equal(
    select_elements(df, 0),
    character()
  )

  expect_equal(
    select_elements(df, 1),
    "x"
  )

  expect_equal(
    select_elements(df, 1:2),
    c("x", "y")
  )

  expect_equal(
    select_elements(df, -x),
    "y"
  )

  expect_equal(
    select_elements(df, c(-x, -y)),
    character()
  )

  expect_equal(
    select_elements(df, -1),
    "y"
  )

  expect_equal(
    select_elements(df, "x"),
    "x"
  )

  expect_equal(
    select_elements(df, c("x", "y")),
    c("x", "y")
  )

  expect_equal(
    select_elements(df, character()),
    character()
  )
})
