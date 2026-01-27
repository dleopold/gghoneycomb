# Honeycomb waffle chart

`geom_honeycomb()` creates a honeycomb-style waffle chart where
hexagonal cells represent proportional data. Each category forms a
contiguous region of connected hexagons, making it easy to compare
relative sizes.

## Usage

``` r
geom_honeycomb(
  mapping = NULL,
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
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  **Required**: `fill` (category) and `weight` (numeric values).

- data:

  The data to be displayed in this layer.

- stat:

  The statistical transformation to use on the data. Default is
  `"honeycomb"`.

- position:

  Position adjustment, either as a string or the result of a position
  adjustment function. Not typically used for honeycomb plots.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics rather than combining
  with them.

- values_are:

  How to interpret the `weight` aesthetic values:

  `"auto"`

  :   (Default) If weights sum to 1 (within tolerance 1e-6), treat as
      proportions; otherwise treat as counts.

  `"counts"`

  :   Treat weights as raw counts to be normalized.

  `"proportions"`

  :   Treat weights as proportions (must sum to 1).

- n_cells:

  Number of hexagonal cells to use. If `NULL` (default), automatically
  chosen based on the number of categories and minimum proportion.
  Maximum allowed is 2500.

- compaction:

  Controls the regularity of the honeycomb boundary:

  `1`

  :   (Default) Rectangular bounding grid.

  `< 1`

  :   Increasingly irregular silhouette via boundary erosion.

  Must be in the range (0, 1\].

- min_width, max_width:

  Constraints on grid width (number of columns). Default `NULL` means no
  constraint.

- min_height, max_height:

  Constraints on grid height (number of rows). Default `NULL` means no
  constraint.

- seed:

  Random seed for reproducible layouts when `compaction < 1`. If `NULL`,
  uses a random seed.

- ...:

  Other arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

## Value

A ggplot2 layer that can be added to a plot.

## Aesthetics

`geom_honeycomb()` understands the following aesthetics:

**Required aesthetics:**

- `fill`:

  The categorical variable defining groups. Each unique value gets its
  own contiguous region of hexagonal cells.

- `weight`:

  Numeric values representing counts or proportions for each category.
  These determine how many cells each category receives. **This
  aesthetic is required** and the geom will error if not provided.

**Optional aesthetics:**

- `colour`:

  Border color of hexagonal cells. Default is `NA` (no border).

- `alpha`:

  Transparency of fill color. Default is 1 (opaque).

- `linewidth`:

  Width of cell borders. Default is 0.5. (Note: only visible if `colour`
  is set.)

- `linetype`:

  Type of cell borders. Default is 1 (solid).

**Ignored aesthetics:**

- `x`, `y`:

  These are **ignored** because the honeycomb layout is computed
  internally based on the weights. You do not need to (and should not)
  map variables to x or y.

## Value Interpretation

The `values_are` parameter controls how `weight` values are interpreted:

- `"auto"` (default):

  If weights sum to 1 (within tolerance 1e-6), they are treated as
  proportions. Otherwise, they are treated as counts and normalized
  internally.

- `"counts"`:

  Weights are raw counts (e.g., 40, 30, 20, 10). They will be normalized
  to proportions internally.

- `"proportions"`:

  Weights are proportions that must sum to 1 (e.g., 0.4, 0.3, 0.2, 0.1).
  An error is raised if they don't.

## Layout Algorithm

The honeycomb layout ensures:

- Each category forms exactly one contiguous region (connected hexagons)

- Cell counts are proportional to weights (using largest-remainder
  rounding)

- Every non-zero category gets at least one cell

- Layouts are reproducible when `seed` is set

## Interactive Use

The stat computes `tooltip_label` and `pct`, which can be used with
optional interactive backends such as `ggiraph`. Keep core plots static;
when you need tooltips, you can layer
[`ggiraph::geom_polygon_interactive()`](https://davidgohel.github.io/ggiraph/reference/geom_polygon_interactive.html)
and map `ggplot2::after_stat(tooltip_label)`.

## See also

- [`stat_honeycomb()`](https://dleopold.github.io/gghoneycomb/reference/stat_honeycomb.md)
  for the underlying stat

- [`scale_honey()`](https://dleopold.github.io/gghoneycomb/reference/scale_honey.md)
  for honey-inspired discrete scales

- [`theme_honeycomb()`](https://dleopold.github.io/gghoneycomb/reference/theme_honeycomb.md)
  for a minimal theme suited to honeycomb plots

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Basic honeycomb with counts
pollen_data <- data.frame(
  species = c("Clover", "Wildflower", "Lavender", "Sunflower", "Other"),
  count = c(45, 25, 15, 10, 5)
)

ggplot(pollen_data, aes(fill = species, weight = count)) +
  geom_honeycomb() +
  coord_equal()

# With proportions (explicit)
pollen_prop <- data.frame(
  species = c("Clover", "Wildflower", "Lavender", "Sunflower", "Other"),
  prop = c(0.45, 0.25, 0.15, 0.10, 0.05)
)

ggplot(pollen_prop, aes(fill = species, weight = prop)) +
  geom_honeycomb(values_are = "proportions") +
  coord_equal()

# Control cell count and layout
ggplot(pollen_data, aes(fill = species, weight = count)) +
  geom_honeycomb(n_cells = 50, compaction = 0.8, seed = 42) +
  coord_equal()

# Optional tooltip usage with ggiraph
if (requireNamespace("ggiraph", quietly = TRUE)) {
  ggplot(pollen_data, aes(fill = species, weight = count)) +
    ggiraph::geom_polygon_interactive(
      stat = "honeycomb",
      aes(tooltip = ggplot2::after_stat(tooltip_label))
    ) +
    coord_equal()
}

# With grid constraints
ggplot(pollen_data, aes(fill = species, weight = count)) +
  geom_honeycomb(min_width = 8, max_width = 12) +
  coord_equal()
} # }
```
