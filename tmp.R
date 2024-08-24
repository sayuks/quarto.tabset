a <- utils::capture.output(
  quarto_tabset(tibble::as_tibble(df_sample) |> dplyr::group_by(group1, var1), c(group1, group2), c(var1, var2, var3))
)

b <- utils::capture.output(
  quarto_tabset(tibble::as_tibble(df_sample) |> dplyr::group_by(group1, var1), c(group1, group2), c(var1, var2, var3))
)

c <- utils::capture.output(
  quarto_tabset(tibble::as_tibble(df_sample) |> dplyr::rowwise(), c(group1, group2), c(var1, var2, var3))
)

d <- utils::capture.output(
  quarto_tabset(tibble::as_tibble(df_sample) |> dplyr::rowwise(group1, var1), c(group1, group2), c(var1, var2, var3))
)


expect_equal(a, b)
expect_equal(a, c)
expect_equal(a, d)


df_sample <- data.frame(
    group1 = factor(c(rep("A", 3), rep("B", 3))),
    group2 = rep(c("X", "Y", "Z"), 2),
    var1 = 1:6,
    var2 = 7:12,
    var3 = factor(letters[1:6])
  )

e <- df_sample |>
  tibble::as_tibble() |>
  tidyr::nest(data = !group1) |>
  dplyr::mutate(
    fig = lapply(
      data,
      function(data) {
        ggplot2::ggplot(data, ggplot2::aes(var1, var2)) +
          ggplot2::geom_point()
      }
    )
  ) |>
  quarto_tabset(group1, fig)


e
