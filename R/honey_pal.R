#' Honey-inspired palette
#'
#' A warm, honey-like palette for categorical data. The palette is designed
#' to work well with discrete categories and can be interpolated when more
#' colors are needed.
#'
#' @param n Number of colors to return.
#' @param direction Direction of the palette. Use `1` for dark-to-light
#'   (default) or `-1` for light-to-dark.
#'
#' @return A character vector of hex color codes.
#'
#' @examples
#' honey_pal()
#' honey_pal(3)
#' honey_pal(8)
#'
#' @export
honey_pal <- function(n = 6, direction = 1) {
  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != floor(n)) {
    rlang::abort("`n` must be a positive integer.")
  }
  if (!is.numeric(direction) || length(direction) != 1 ||
      !direction %in% c(-1, 1)) {
    rlang::abort("`direction` must be either 1 or -1.")
  }

  base <- c(
    "#5A2D0C",
    "#7A3E14",
    "#9C5B1B",
    "#C97A2B",
    "#E1A14A",
    "#F4D27B"
  )

  if (direction == -1) {
    base <- rev(base)
  }

  if (n <= length(base)) {
    return(base[seq_len(n)])
  }

  grDevices::colorRampPalette(base)(n)
}
