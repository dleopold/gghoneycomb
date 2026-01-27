#' Hex grid geometry helpers
#'
#' Internal functions for flat-top hexagonal grid geometry using odd-q offset
#' coordinates. These helpers compute vertex positions, grid centers, and
#' neighbor offsets for honeycomb layouts.
#'
#' @name hex-geometry
#' @keywords internal
NULL

#' Compute vertices of a flat-top hexagon
#'
#' Given a center point and radius, returns the 6 vertices of a flat-top
#' hexagon in counter-clockwise order starting from the rightmost vertex.
#'
#' @param cx Numeric. X-coordinate of the hexagon center.
#' @param cy Numeric. Y-coordinate of the hexagon center.
#' @param r Numeric. Radius (distance from center to vertex).
#'
#' @return A data.frame with columns `x` and `y`, containing 6 rows
#'   (one per vertex).
#'
#' @details
#' For flat-top hexagons, vertices are at angles 0, 60, 120, 180, 240, 300

#' degrees from the positive x-axis. The radius is the distance from the
#' center to each vertex (circumradius).
#'
#' @keywords internal
#' @noRd
hex_vertices <- function(cx, cy, r) {
  # Flat-top hex: vertices at 0, 60, 120, 180, 240, 300 degrees

  angles_deg <- c(0, 60, 120, 180, 240, 300)
  angles_rad <- angles_deg * pi / 180

  data.frame(
    x = cx + r * cos(angles_rad),
    y = cy + r * sin(angles_rad)
  )
}

#' Compute center coordinates for a hex grid
#'
#' Generates center coordinates for a rectangular grid of flat-top hexagons
#' using odd-q offset coordinates.
#'
#' @param n_cols Integer. Number of columns in the grid.
#' @param n_rows Integer. Number of rows in the grid.
#' @param r Numeric. Radius of each hexagon (distance from center to vertex).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{col}{Integer column index (0-based).}
#'     \item{row}{Integer row index (0-based).}
#'     \item{x}{X-coordinate of the hexagon center.}
#'     \item{y}{Y-coordinate of the hexagon center.}
#'   }
#'
#' @details
#' Uses odd-q offset coordinates where odd-numbered columns are shifted
#' down by half the vertical spacing.
#'
#' Spacing formulas for flat-top hexagons:
#' \itemize{
#'   \item Horizontal spacing between column centers: dx = 1.5 * r
#'   \item Vertical spacing between row centers: dy = sqrt(3) * r
#'   \item Odd columns are shifted down by dy / 2
#' }
#'
#' @keywords internal
#' @noRd
hex_grid_centers <- function(n_cols, n_rows, r) {
  if (n_cols < 1 || n_rows < 1) {
    return(data.frame(col = integer(0), row = integer(0),
                      x = numeric(0), y = numeric(0)))
  }

  # Spacing for flat-top hexagons
  dx <- 1.5 * r
  dy <- sqrt(3) * r

  # Generate all (col, row) combinations
  grid <- expand.grid(col = seq(0L, n_cols - 1L), row = seq(0L, n_rows - 1L))

  # Compute x: columns are spaced by dx

  grid$x <- grid$col * dx

  # Compute y: rows are spaced by dy, odd columns shifted down by dy/2
  # In odd-q: odd columns have +0.5 row offset
  grid$y <- grid$row * dy + (grid$col %% 2) * (dy / 2)

  grid[, c("col", "row", "x", "y")]
}

#' Get neighbor offsets for odd-q hex coordinates
#'
#' Returns the 6 neighbor offsets for a cell at the given column, using
#' odd-q offset coordinate conventions.
#'
#' @param col Integer. Column index of the cell.
#' @param row Integer. Row index of the cell (not used in offset calculation,
#'   but included for API consistency).
#'
#' @return A data.frame with columns `dcol` and `drow`, containing 6 rows
#'   representing the offsets to add to (col, row) to get each neighbor.
#'
#' @details
#' In odd-q offset coordinates, the neighbor offsets depend on whether the
#' column is odd or even:
#'
#' For even columns (col %% 2 == 0):
#' \itemize{
#'   \item (+1, +1), (+1, 0), (0, -1), (-1, 0), (-1, +1), (0, +1)
#' }
#'
#' For odd columns (col %% 2 == 1):
#' \itemize{
#'   \item (+1, 0), (+1, -1), (0, -1), (-1, -1), (-1, 0), (0, +1)
#' }
#'
#' @keywords internal
#' @noRd
hex_neighbors <- function(col, row = 0L) {
  # Odd-q offset neighbor directions
  # Reference: https://www.redblobgames.com/grids/hexagons/#neighbors-offset

  if (col %% 2 == 0) {
    # Even column
    offsets <- data.frame(
      dcol = c(1L, 1L, 0L, -1L, -1L, 0L),
      drow = c(1L, 0L, -1L, 0L, 1L, 1L)
    )
  } else {
    # Odd column
    offsets <- data.frame(
      dcol = c(1L, 1L, 0L, -1L, -1L, 0L),
      drow = c(0L, -1L, -1L, -1L, 0L, 1L)
    )
  }

  offsets
}
