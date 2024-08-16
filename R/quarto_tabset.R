#' Create tabset panels in quarto markdown
#'
#' The function takes in a data frame or a tibble and produces
#' tabset panels for each unique combination of the tabset variables.
#' ***Only works with .qmd files in HTML format.***
#'
#' - Write `#| results: asis` at the beginning of the chunk or
#'   `results='asis'` in the chunk options.
#' - The `data` is sorted internally in the order of `tabset_vars`.
#'   Define the order beforehand, e.g. using factor.
#' - If multiple `tabset_vars` are given, create nested tabsets.
#' - `output_vars` can also be figures or tables if `data` is a tibble.
#' - If factor columns are included in output_vars, they are converted
#'   internally to character.
#' - When outputting tables or figures that use javascript
#'   (such as `{plotly}`, `{leaflet}`, `{DT}`, `{reactable}`, etc.),
#'   it seems javascript dependencies need to be resolved.
#'   A simple solution is to wrap the output in [`htmltools::div()`]
#'   and create a dummy plot in another chunk. See the demo page for details.
#' - The function has an optional argument, `layout`, which allows for
#'   the addition of layout option to the outputs
#'   (see \url{https://quarto.org/docs/authoring/figures.html}).
#'   However, this is intended for simplified use cases and
#'   complex layouts may not work. See Examples for more details.
#'
#' @param data A data frame.
#' @param tabset_vars <[`tidy-select`][dplyr_tidy_select]>
#' Variables to use as tabset labels.
#' @param output_vars <[`tidy-select`][dplyr_tidy_select]>
#' Variables to display in each tabset panel.
#' @param layout `NULL` or a character vector of length 1 for specifying layout
#' in tabset panel. If not `NULL`, `layout` must begin with at least three
#' or more repetitions of ":" (e.g. ":::").
#' @param heading_levels `NULL` or a positive integer-ish numeric vector of
#' length equal to the number of columns specified in `tabset_vars`.
#' This controls whether it is partially (or entirely) displayed
#' as normal header instead of tabset.
#' * If `NULL`, all output is tabset.
#' * If a positive integer-ish numeric vector, the elements of the vector
#' correspond to the columns specified in `tabset_vars`.
#'    * If the element is integer, the tabset column is displayed as headers
#'    with their level, not tabset. (e.g. 2 means h2 header).
#'    Levels 1 to 6 are recommended. The reason is that quarto supports headers
#'    up to 6. 7 and above will also work, but they are displayed as normal
#'    text. In addition, considering the chapter format,
#'    it is preferable to gradually increase the level, as in 1, 2 and 3.
#'    * If the element is NA, tabset is displayed.
#'
#' @return `NULL` invisibly. (Create tabsest/heading as a side effect.)
#'
#' @examples
#' # sample data
#' df <- data.frame(
#'   group1 = c(rep("A", 3), rep("B", 3)),
#'   group2 = rep(c("X", "Y", "Z"), 2),
#'   var1 = rnorm(6),
#'   var2 = rnorm(6),
#'   var3 = rnorm(6)
#' )
#'
#' # Here are examples of the output before it is converted to tabset.
#' # If you want it to actually work, in the .qmd file,
#' # set `results='asis'` in the chunk options or
#' # write `#| results: asis` at the beginning of the chunk.
#'
#' # Basic usage
#' quarto_tabset(df, c(group1, group2), c(var1, var2, var3))
#'
#' # Here is an example of the `layout` argument.
#' quarto_tabset(
#'   df,
#'   c(group1, group2),
#'   c(var1, var2, var3),
#'   layout = '::: {layout="[2, 3, 5]"}'
#' )
#'
#' # Use heading instead of tabset
#' quarto_tabset(
#'   df,
#'   c(group1, group2),
#'   c(var1, var2, var3),
#'   heading_levels = c(2, 3)
#' )
#' @export
quarto_tabset <- function(
  data,
  tabset_vars,
  output_vars,
  layout = NULL,
  heading_levels = NULL
) {
  # Validation steps ------
  stopifnot(
    "`data` must be a data frame" =
      is.data.frame(data),
    "`data` must have one or more rows." =
      nrow(data) >= 1,
    "`data` must have two or more columns." =
      ncol(data) >= 2
  )

  if (!is.null(layout)) {
    stopifnot(
      "`layout` must be length 1." =
        length(layout) == 1,
      "`layout` must be character." =
        is.character(layout),
      '`layout` must begin with at least three or more repetitions of ":"' =
        grepl("^:{3,}", layout)
    )
  }

  if (!is.null(heading_levels)) {
    stopifnot(
      "`heading_levels` must be numeric." =
        is.numeric(heading_levels),
      "`heading_levels` must be length 1 or greater." =
        length(heading_levels) > 0,
      "`heading_levels` must not include NaN." =
        !is.nan(heading_levels),
      "`heading_levels` must not be infinite." =
        !is.infinite(heading_levels)
    )

    nums <- heading_levels[!is.na(heading_levels)]

    if (length(nums) > 0) {
      stopifnot(
        # ref. examples section of `?integer`
        "`heading_levels` except for NAs must be integer-ish." =
          abs(nums - round(nums)) < .Machine$double.eps^0.5,
        "`heading_levels` except for NAs must be positive." =
          nums > 0
      )
    }

    heading_levels <- as.integer(heading_levels)
  }

  # Ungroup data first
  data <- dplyr::ungroup(data)

  # Get tabset column names from data based on tabset_vars
  tabset_names <- colnames(dplyr::select(data, {{ tabset_vars }}))

  len_tab <- length(tabset_names)

  stopifnot(
    "`tabset_vars` must be of length 1 or more." =
      len_tab > 0
  )

  if (is.null(heading_levels)) {
    heading_levels <- rep(NA_integer_, len_tab)
  }

  stopifnot(
    "The number of columns specified in `tabset_vars`
      and the length of `heading_levels` must be the same." =
      length(heading_levels) == len_tab
  )

  # Get output column names from data based on output_vars
  output_names <- colnames(dplyr::select(data, {{ output_vars }}))

  stopifnot(
    "`output_vars` must be of length 1 or more." =
      length(output_names) > 0,

    "There must not be variables that are included in both `tabset_vars` and `output_vars`." = # nolint line_length_lintr
      length(intersect(tabset_names, output_names)) == 0
  )

  # Restructure data to be in tabset format ----
  df1 <- dplyr::select(data, {{ tabset_vars }}, {{ output_vars }})
  df1 <- dplyr::arrange(df1, dplyr::pick({{ tabset_vars }}))
  df1 <- dplyr::mutate(
    df1,
    # If tabset_vars contains factor, the labels on the tabs will be numbers.
    # If output_vars contains factor, the output using `cat()` produce integers.
    # So, convert all factor columns to characters.
    dplyr::across(dplyr::where(is.factor), as.character)
  )

  df1 <- Reduce(
    f = function(df, idx) {
      gvars <- tabset_names[seq_len(idx) - 1]
      df <- dplyr::group_by(df, dplyr::pick(dplyr::any_of(gvars)))
      df <- dplyr::mutate(
        df,
        # Add start and end markers for each tabset
        # nolint start: object_name_linter, object_usage_linter.
        "tabset{idx}_start__" := dplyr::row_number() == 1,
        "tabset{idx}_end__" := dplyr::row_number() == max(dplyr::row_number())
        # nolint end
      )
      df <- dplyr::ungroup(df)
    },
    x = seq_len(len_tab),
    init = df1
  )


  # For each row of the data, print the tabset and output panels ----
  invisible(
    lapply(
      seq_len(nrow(df1)),
      function(i) {
        # Check if this row contains a tabset1_start__ column
        if (is.na(heading_levels[1]) && df1[[i, "tabset1_start__"]]) {
          # Add a panel-tabset div
          cat("::: {.panel-tabset} \n\n")
        }

        # Loop through each tabset column starting from the second one
        if (len_tab >= 2) {
          for (j in 2:len_tab) {
            # Check if this row contains a column
            # for the start of the current tabset
            if (df1[[i, paste0("tabset", j, "_start__")]]) {

              # Print the heading
              heading_level <- heading_levels[j - 1]

              if (is.na(heading_level)) {
                heading_level <- j - 1
              }

              cat(
                strrep("#", heading_level),
                df1[[i, tabset_names[j - 1]]],
                "\n\n"
              )

              # Print a panel-tabset div if the heading_level is NA
              if (is.na(heading_levels[j])) {
                cat("::: {.panel-tabset} \n\n")
              }
            }
          }
        }

        heading_level <- heading_levels[len_tab]

        if (is.na(heading_level)) {
          heading_level <- len_tab
        }

        # Print the current tab heading
        # (The output is displayed in this tabset/heading)
        cat(
          strrep("#", heading_level),
          df1[[i, tabset_names[len_tab]]],
          "\n\n"
        )

        # Print the layout if it exists
        if (!is.null(layout)) {
          cat(layout, "\n\n")
        }

        # Print the outputs
        for (j in seq_along(output_names)) {
          out_col <- df1[[i, output_names[j]]]

          # add [[1]] at the end because figures and tables
          # are stored as lists in columns
          out <- out_col[[1]]

          # Use `print()` for list-type columns (), otherwise use `cat()`.
          # Using `cat()` avoids unnecessary prefixes
          # such as "[1]" in the output.
          if (is.list(out_col)) {
            print(out)
          } else {
            cat(out)
          }

          cat("\n\n")
        }

        # Close layout-div if layout exists
        if (!is.null(layout)) {
          cat(sub("^(:+).*", "\\1", layout), "\n\n")
        }

        # Loop through each tabset column in reverse order.
        # (To close from the inner tabset.)
        for (j in rev(seq_len(len_tab))) {
          # Check if this row contains a column
          # for the end of the current tabset
          if (is.na(heading_levels[j]) &&
                df1[[i, paste0("tabset", j, "_end__")]]) {
            # Close the panel-tabset div
            cat("::: \n\n")
          }
        }
      }
    )
  )

  invisible(df1)
}
