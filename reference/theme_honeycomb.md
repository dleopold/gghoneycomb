# Minimal theme for honeycomb plots

A clean, honeycomb-focused theme based on
[`ggplot2::theme_void()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
This removes axes and grids while keeping typographic hierarchy for
titles and captions.

## Usage

``` r
theme_honeycomb(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Base font size.

- base_family:

  Base font family.

## Value

A ggplot2 theme object.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot() + ggplot2::theme_void() + theme_honeycomb()
}

```
