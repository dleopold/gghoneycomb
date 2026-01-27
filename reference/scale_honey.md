# Honey-inspired discrete scales

Scales for honeycomb plots based on
[`honey_pal()`](https://dleopold.github.io/gghoneycomb/reference/honey_pal.md).
These scales are discrete and intended for categorical data. For more
categories than the base palette, colors are interpolated.

## Usage

``` r
scale_fill_honey(..., direction = 1)

scale_color_honey(..., direction = 1)
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- direction:

  Direction of the palette. Use `1` for dark-to-light (default) or `-1`
  for light-to-dark.

## Value

A ggplot2 scale object.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  df <- data.frame(group = LETTERS[1:6], value = 1)
  ggplot2::ggplot(df, ggplot2::aes(x = group, y = value, fill = group)) +
    ggplot2::geom_col() +
    scale_fill_honey()
}

```
