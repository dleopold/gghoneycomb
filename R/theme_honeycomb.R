#' Minimal theme for honeycomb plots
#'
#' A clean, honeycomb-focused theme based on [ggplot2::theme_void()]. This
#' removes axes and grids while keeping typographic hierarchy for titles and
#' captions.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot() + ggplot2::theme_void() + theme_honeycomb()
#' }
#'
#' @export
theme_honeycomb <- function(base_size = 11, base_family = "") {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(size = base_size * 0.9, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size * 0.85),
      plot.title = ggplot2::element_text(size = base_size * 1.2, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.95, margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(size = base_size * 0.8, color = "grey40"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
