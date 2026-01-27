# Compute honeycomb layout statistics

`stat_honeycomb()` computes the layout for a honeycomb waffle chart,
transforming categorical data with associated weights into hexagonal
cell positions. This stat is typically used with
[`geom_honeycomb()`](https://dleopold.github.io/gghoneycomb/reference/geom_honeycomb.md).

## Usage

``` r
stat_honeycomb(
  mapping = NULL,
  data = NULL,
  geom = "honeycomb",
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

- geom:

  The geometric object to use to display the data.

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

  Other arguments passed to the stat.

## Value

A ggplot2 layer that can be added to a plot.

## Data Contract

The stat expects data with:

- A categorical variable mapped to `fill` (the category for each cell)

- A numeric variable mapped to `weight` (counts or proportions)

The stat **ignores** any `x` and `y` mappings in the input data because
the honeycomb layout is computed internally based on the weights.

## Computed Variables

The stat computes the following variables for use by the geom:

- x, y:

  Vertex coordinates for each hexagon

- group:

  Unique cell identifier (one per hexagon)

- cell_id:

  Same as group, explicit cell identifier

- grid_col, grid_row:

  Integer grid coordinates (odd-q offset indices)

- pct:

  Normalized proportion for the category

- tooltip_label:

  Preformatted string ": %"

## See also

[`geom_honeycomb()`](https://dleopold.github.io/gghoneycomb/reference/geom_honeycomb.md)
for the geom that draws the hexagons.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Example with counts
df <- data.frame(
  category = c("A", "B", "C", "D"),
  count = c(40, 30, 20, 10)
)

ggplot(df, aes(fill = category, weight = count)) +
  stat_honeycomb()

# Example with proportions
df_prop <- data.frame(
  category = c("A", "B", "C", "D"),
  prop = c(0.4, 0.3, 0.2, 0.1)
)

ggplot(df_prop, aes(fill = category, weight = prop)) +
  stat_honeycomb(values_are = "proportions")
} # }
```
