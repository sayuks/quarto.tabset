# flag_tabset_boundaries <- function(data, tabset_vars) {
#   l <- do.call(
#     validate_data,
#     list(
#       data = data,
#       tabset_vars = substitute(tabset_vars),
#       output_vars = NULL,
#       layout = NULL,
#       heading_levels = NULL
#     )
#   )
#
#   tabset_names <- l$tabset_names
#   output_names <- l$output_names
#   # heading_levels <- l$heading_levels
#   # len_tab <- length(tabset_names)
#
#   data <- prep_data(
#     data,
#     tabset_names,
#     output_names
#   )
#
#   data
# }
