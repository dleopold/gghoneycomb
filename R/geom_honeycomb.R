#' Honeycomb waffle chart
#'
#' `geom_honeycomb()` creates a honeycomb-style waffle chart where hexagonal
#' cells represent proportional data. Each category forms a contiguous region
#' of connected hexagons, making it easy to compare relative sizes.
#'
#' @section Aesthetics:
#' `geom_honeycomb()` understands the following aesthetics:
#'
#' **Required aesthetics:**
#' \describe{
#'   \item{`fill`}{The categorical variable defining groups. Each unique value
#'     gets its own contiguous region of hexagonal cells.}
#'
#'   \item{`weight`}{Numeric values representing counts or proportions for each
#'     category. These determine how many cells each category receives.
#'     **This aesthetic is required** and the geom will error if not provided.}
#' }
#'
#' **Optional aesthetics:**
#' \describe{
#'   \item{`colour`}{Border color of hexagonal cells. Default is `NA` (no border).}
#'   \item{`alpha`}{Transparency of fill color. Default is 1 (opaque).}
#'   \item{`linewidth`}{Width of cell borders. Default is 0.5.
#'     (Note: only visible if `colour` is set.)}
#'   \item{`linetype`}{Type of cell borders. Default is 1 (solid).}
#' }
#'
#' **Ignored aesthetics:**
#' \describe{
#'   \item{`x`, `y`}{These are **ignored** because the honeycomb layout is
#'     computed internally based on the weights. You do not need to (and should
#'     not) map variables to x or y.}
#' }
#'
#' @section Value Interpretation:
#' The `values_are` parameter controls how `weight` values are interpreted:
#'
#' \describe{
#'   \item{`"auto"` (default)}{If weights sum to 1 (within tolerance 1e-6),
#'     they are treated as proportions. Otherwise, they are treated as counts
#'     and normalized internally.}
#'   \item{`"counts"`}{Weights are raw counts (e.g., 40, 30, 20, 10).
#'     They will be normalized to proportions internally.
#'   }
#'   \item{`"proportions"`}{Weights are proportions that must sum to 1
#'     (e.g., 0.4, 0.3, 0.2, 0.1). An error is raised if they don't.}
#' }
#'
#' @section Layout Algorithm:
#' The honeycomb layout ensures:
#' \itemize{
#'   \item Each category forms exactly one contiguous region (connected hexagons)
#'   \item Cell counts are proportional to weights (using largest-remainder rounding)
#'   \item Every non-zero category gets at least one cell
#'   \item Layouts are reproducible when `seed` is set
#' }
#'
#' @section Interactive Use:
#' The stat computes `tooltip_label` and `pct`, which can be used with optional
#' interactive backends such as `ggiraph`. Keep core plots static; when you need
#' tooltips, you can layer `ggiraph::geom_polygon_interactive()` and map
#' `ggplot2::after_stat(tooltip_label)`.
#'
#' @inheritParams stat_honeycomb
#' @param stat The statistical transformation to use on the data.
#'   Default is `"honeycomb"`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer that can be added to a plot.
#'
#' @seealso
#' \itemize{
#'   \item [stat_honeycomb()] for the underlying stat
#'   \item [scale_honey()] for honey-inspired discrete scales
#'   \item [theme_honeycomb()] for a minimal theme suited to honeycomb plots
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic honeycomb with counts
#' pollen_data <- data.frame(
#'   species = c("Clover", "Wildflower", "Lavender", "Sunflower", "Other"),
#'   count = c(45, 25, 15, 10, 5)
#' )
#'
#' ggplot(pollen_data, aes(fill = species, weight = count)) +
#'   geom_honeycomb() +
#'   coord_equal()
#'
#' # With proportions (explicit)
#' pollen_prop <- data.frame(
#'   species = c("Clover", "Wildflower", "Lavender", "Sunflower", "Other"),
#'   prop = c(0.45, 0.25, 0.15, 0.10, 0.05)
#' )
#'
#' ggplot(pollen_prop, aes(fill = species, weight = prop)) +
#'   geom_honeycomb(values_are = "proportions") +
#'   coord_equal()
#'
#' # Control cell count and layout
#' ggplot(pollen_data, aes(fill = species, weight = count)) +
#'   geom_honeycomb(n_cells = 50, compaction = 0.8, seed = 42) +
#'   coord_equal()
#'
#' # Optional tooltip usage with ggiraph
#' if (requireNamespace("ggiraph", quietly = TRUE)) {
#'   ggplot(pollen_data, aes(fill = species, weight = count)) +
#'     ggiraph::geom_polygon_interactive(
#'       stat = "honeycomb",
#'       aes(tooltip = ggplot2::after_stat(tooltip_label))
#'     ) +
#'     coord_equal()
#' }
#'
#' # With grid constraints
#' ggplot(pollen_data, aes(fill = species, weight = count)) +
#'   geom_honeycomb(min_width = 8, max_width = 12) +
#'   coord_equal()
#' }
#'
#' @export
geom_honeycomb <- function(mapping = NULL,
                           data = NULL,
                           stat = "honeycomb",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           values_are = c("auto", "counts", "proportions"),
                           n_cells = NULL,
                           compaction = 1,
                           min_width = NULL,
                           max_width = NULL,
                           min_height = NULL,
                           max_height = NULL,
                           seed = NULL,
                           ...) {
  values_are <- match.arg(values_are)

  # Validate compaction
  if (!is.numeric(compaction) || length(compaction) != 1 ||
      compaction <= 0 || compaction > 1) {
    rlang::abort(
      "`compaction` must be a single number in the range (0, 1]."
    )
  }

  # Validate n_cells if provided
  if (!is.null(n_cells)) {
    if (!is.numeric(n_cells) || length(n_cells) != 1 ||
        n_cells < 1 || n_cells != floor(n_cells)) {
      rlang::abort(
        "`n_cells` must be a positive integer."
      )
    }
    if (n_cells > 2500) {
      rlang::abort(
        paste0("`n_cells` must be at most 2500, got ", n_cells, ".")
      )
    }
  }

  ggplot2::layer(
    geom = GeomHoneycomb,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      values_are = values_are,
      n_cells = n_cells,
      compaction = compaction,
      min_width = min_width,
      max_width = max_width,
      min_height = min_height,
      max_height = max_height,
      seed = seed,
      ...
    )
  )
}

#' @rdname geom_honeycomb
#' @format NULL
#' @usage NULL
#' @export
GeomHoneycomb <- ggplot2::ggproto("GeomHoneycomb", ggplot2::GeomPolygon,
  required_aes = c("x", "y", "fill"),

  default_aes = ggplot2::aes(
    colour = NA,
    fill = "grey50",
    linewidth = 0.5,
    linetype = 1,
    alpha = 1
  ),

  # Draw method inherits from GeomPolygon
  # The stat provides x, y, group, fill columns that GeomPolygon can draw

  draw_key = ggplot2::draw_key_polygon
)
