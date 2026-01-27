# Honey-inspired palette

A warm, honey-like palette for categorical data. The palette is designed
to work well with discrete categories and can be interpolated when more
colors are needed.

## Usage

``` r
honey_pal(n = 6, direction = 1)
```

## Arguments

- n:

  Number of colors to return.

- direction:

  Direction of the palette. Use `1` for dark-to-light (default) or `-1`
  for light-to-dark.

## Value

A character vector of hex color codes.

## Examples

``` r
honey_pal()
#> [1] "#5A2D0C" "#7A3E14" "#9C5B1B" "#C97A2B" "#E1A14A" "#F4D27B"
honey_pal(3)
#> [1] "#5A2D0C" "#7A3E14" "#9C5B1B"
honey_pal(8)
#> [1] "#5A2D0C" "#703911" "#884A17" "#A25F1D" "#C27528" "#D6903C" "#E6AE57"
#> [8] "#F4D27B"
```
