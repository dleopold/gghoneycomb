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
  silhouette = c("rect", "rounded", "organic"),
  layout = c("compact", "free"),
  compact_style = c("perimeter", "blocky"),
  temperature = NULL,
  min_width = NULL,
  max_width = NULL,
  min_height = NULL,
  max_height = NULL,
  seed = NULL,
  rotation = 0,
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

- silhouette:

  Shape of the overall honeycomb boundary. Accepted values: `"rect"`
  (default), `"rounded"`, `"organic"`. All three silhouettes produce a
  connected mask of the requested size. `"rounded"` softens the corners
  of a rectangular grid; `"organic"` grows an irregular boundary from a
  central seed.

- layout:

  Region allocation strategy. Accepted values: `"compact"` (default),
  `"free"`. `"compact"` assigns cells deterministically (`temperature`
  must be 0); `"free"` uses stochastic top-K sampling controlled by
  `temperature`.

- compact_style:

  Style of region packing when `layout = "compact"`. Accepted values:
  `"perimeter"` (default), `"blocky"`. `"perimeter"` minimises region
  perimeter via greedy growth with a carve fallback; `"blocky"` uses
  directional face-carving. `"blocky"` requires `silhouette = "rect"`.

- temperature:

  Controls randomness in region allocation. A single non-negative
  number:

  `NULL`

  :   (Default) Automatically set: 0 for `layout = "compact"`, 0.35 for
      `layout = "free"`.

  `0`

  :   Deterministic allocation. Required when `layout = "compact"`.

  `> 0`

  :   Increasing randomness. Only valid with `layout = "free"`.

- min_width, max_width:

  Constraints on grid width (number of columns). Default `NULL` means no
  constraint.

- min_height, max_height:

  Constraints on grid height (number of rows). Default `NULL` means no
  constraint.

- seed:

  Random seed for reproducible layouts. If `NULL`, uses a random seed.

- rotation:

  Rotation of the rendered honeycomb in degrees. Must be one of `0`,
  `90`, `180`, `270`. Rotation is applied to output coordinates only
  (grid allocation/contiguity are unchanged).

- ...:

  Other arguments passed to the stat. Passing the removed argument will
  raise an error with migration guidance.

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

# Free layout with high temperature and rotation
ggplot(df, aes(fill = category, weight = count)) +
  stat_honeycomb(
    n_cells     = 100,
    silhouette  = "organic",
    layout      = "free",
    temperature = 0.8,
    rotation    = 90,
    seed        = 7
  )

# Blocky compact style (requires silhouette = "rect")
ggplot(df, aes(fill = category, weight = count)) +
  stat_honeycomb(
    n_cells       = 100,
    silhouette    = "rect",
    layout        = "compact",
    compact_style = "blocky"
  )
} # }
```
