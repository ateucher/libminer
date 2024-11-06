#' R Library Summary
#'
#' Provides a brief summary of the package libraries on your machine
#'
#' @param sizes Should sizes of libraries be calculated. Default `FALSE`.
#' @param by column to group by
#'
#' @return A data.frame containing the count of packages in each of the user's
#'   libraries
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(by = .data$LibPath, sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("'sizes' must be logical")
  }

  pkg_df <- lib() |>
    calculate_sizes(do_calc = sizes)

  pkg_df |>
  dplyr::group_by({{ by }}) |>
    dplyr::summarise(
     n = dplyr::n(),
     dplyr::across(dplyr::any_of("size"), .fns = sum, .names = "size")
    )
}

#' Generate a data frame of installed packages
#'
#' @return a data.frame of all packages installed on your system
#' @export
lib <- function() {
  pkgs <- utils::installed.packages()
  as.data.frame(pkgs, stringsAsFactors = FALSE)
}

#' calculate sizes
#'
#' @param df a data.frame
#'
#' @return df with a lib_size column
#' @noRd
calculate_sizes <- function(df, do_calc = FALSE) {
  if (!do_calc) {
    return(df)
  }

  df |>
    dplyr::mutate(
      size = map_dbl(
        fs::path(.data$LibPath, .data$Package),
        \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
      )
    )
}
