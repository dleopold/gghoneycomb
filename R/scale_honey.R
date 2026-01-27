#' Honey-inspired discrete scales
#'
#' Scales for honeycomb plots based on [honey_pal()]. These scales are
#' discrete and intended for categorical data. For more categories than the
#' base palette, colors are interpolated.
#'
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#' @param direction Direction of the palette. Use `1` for dark-to-light
#'   (default) or `-1` for light-to-dark.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' 
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   df <- data.frame(group = LETTERS[1:6], value = 1)
#'   ggplot2::ggplot(df, ggplot2::aes(x = group, y = value, fill = group)) +
#'     ggplot2::geom_col() +
#'     scale_fill_honey()
#' }
#'
#' @name scale_honey
#' @export
scale_fill_honey <- function(..., direction = 1) {
  ggplot2::discrete_scale(
    "fill",
    "honey",
    palette = function(n) honey_pal(n, direction = direction),
    ...
  )
}

#' @rdname scale_honey
#' @export
scale_color_honey <- function(..., direction = 1) {
  ggplot2::discrete_scale(
    "colour",
    "honey",
    palette = function(n) honey_pal(n, direction = direction),
    ...
  )
}
