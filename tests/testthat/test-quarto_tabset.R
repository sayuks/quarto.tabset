test_that("quarto_tabset(), basic", {
  df_sample <- data.frame(
    group1 = factor(c(rep("A", 3), rep("B", 3))),
    group2 = rep(c("X", "Y", "Z"), 2),
    var1 = 1:6,
    var2 = 7:12,
    var3 = factor(letters[1:6])
  )

  expected <-
    c(
      "::: {.panel-tabset} ",
      "",
      "# A ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "## Y ",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "## Z ",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      "::: ",
      "",
      "# B ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "## Y ",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "## Z ",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      "::: ",
      "",
      "::: ",
      ""
    )

  res <- utils::capture.output(
    quarto_tabset(df_sample, c(group1, group2), c(var1, var2, var3))
  )

  expect_equal(
    res,
    expected
  )

  # tibble input works
  expect_equal(
    utils::capture.output(
        quarto_tabset(tibble::as_tibble(df_sample), c(group1, group2), c(var1, var2, var3))
    ),
    res
  )
})

test_that("quarto_tabset(), layout argument", {
  df_sample <- data.frame(
    group1 = factor(c(rep("A", 3), rep("B", 3))),
    group2 = rep(c("X", "Y", "Z"), 2),
    var1 = 1:6,
    var2 = 7:12,
    var3 = factor(letters[1:6])
  )

  expected <-
    c(
      "::: {.panel-tabset} ",
      "",
      "# A ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "::: ",
      "",
      "## Y ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "::: ",
      "",
      "## Z ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      "::: ",
      "",
      "::: ",
      "",
      "# B ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "::: ",
      "",
      "## Y ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "::: ",
      "",
      "## Z ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      "::: ",
      "",
      "::: ",
      "",
      "::: ",
      ""
    )

  expect_equal(
    utils::capture.output(
      quarto_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        layout = '::: {layout="[2, 3, 5]"}'
      )
    ),
    expected
  )
})

test_that("quarto_tabset(), heading_levels argument", {
  df_sample <- data.frame(
    group1 = factor(c(rep("A", 3), rep("B", 3))),
    group2 = rep(c("X", "Y", "Z"), 2),
    var1 = 1:6,
    var2 = 7:12,
    var3 = factor(letters[1:6])
  )

  expected <-
    c(
      "# A ",
      "",
      "## X ",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "## Y ",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "## Z ",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      "# B ",
      "",
      "## X ",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "## Y ",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "## Z ",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      ""
    )

  expect_equal(
    utils::capture.output(
      quarto_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        heading_levels = c(1, 2)
      )
    ),
    expected
  )
})

test_that("quarto_tabset(), heading_levels argument with NA", {
  df_sample <- data.frame(
    group1 = factor(c(rep("A", 3), rep("B", 3))),
    group2 = rep(c("X", "Y", "Z"), 2),
    var1 = 1:6,
    var2 = 7:12,
    var3 = factor(letters[1:6])
  )

  expected <-
    c(
      "# A ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "## Y ",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "## Z ",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      "::: ",
      "",
      "# B ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "## Y ",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "## Z ",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      "::: ",
      ""
    )

  expect_equal(
    utils::capture.output(
      quarto_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        heading_levels = c(1, NA)
      )
    ),
    expected
  )
})

test_that("quarto_tabset(), both layout and heading_levels arguments", {
  df_sample <- data.frame(
    group1 = factor(c(rep("A", 3), rep("B", 3))),
    group2 = rep(c("X", "Y", "Z"), 2),
    var1 = 1:6,
    var2 = 7:12,
    var3 = factor(letters[1:6])
  )

  expected <-
    c(
      "# A ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "::: ",
      "",
      "## Y ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "::: ",
      "",
      "## Z ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      "::: ",
      "",
      "::: ",
      "",
      "# B ",
      "",
      "::: {.panel-tabset} ",
      "",
      "## X ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "::: ",
      "",
      "## Y ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "::: ",
      "",
      "## Z ",
      "",
      "::: {layout=\"[2, 3, 5]\"} ",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      "::: ",
      "",
      "::: ",
      ""
    )

  expect_equal(
    utils::capture.output(
      quarto_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        layout = '::: {layout="[2, 3, 5]"}',
        heading_levels = c(1, NA)
      )
    ),
    expected
  )
})
