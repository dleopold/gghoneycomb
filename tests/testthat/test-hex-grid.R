# Tests for hex grid geometry helpers

test_that("hex_vertices returns 6 vertices", {

  verts <- gghoneycomb:::hex_vertices(0, 0, 1)
  expect_equal(nrow(verts), 6)
  expect_named(verts, c("x", "y"))
})

test_that("hex_vertices produces correct radius", {

  r <- 2.5
  verts <- gghoneycomb:::hex_vertices(0, 0, r)

  # All vertices should be at distance r from center
  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_equal(distances, rep(r, 6), tolerance = 1e-10)
})

test_that("hex_vertices respects center offset", {
  cx <- 5
  cy <- 3
  r <- 1

  verts <- gghoneycomb:::hex_vertices(cx, cy, r)

  # Centroid of vertices should be at (cx, cy)
  expect_equal(mean(verts$x), cx, tolerance = 1e-10)
  expect_equal(mean(verts$y), cy, tolerance = 1e-10)
})

test_that("hex_vertices produces flat-top orientation",
{
  # For flat-top hex, first vertex (angle 0) should be at (cx + r, cy)
  r <- 1
  verts <- gghoneycomb:::hex_vertices(0, 0, r)

  # First vertex at angle 0 degrees
  expect_equal(verts$x[1], r, tolerance = 1e-10)
  expect_equal(verts$y[1], 0, tolerance = 1e-10)

  # Fourth vertex at angle 180 degrees
  expect_equal(verts$x[4], -r, tolerance = 1e-10)
  expect_equal(verts$y[4], 0, tolerance = 1e-10)
})

test_that("hex_grid_centers returns correct structure", {
  grid <- gghoneycomb:::hex_grid_centers(3, 2, 1)

  expect_named(grid, c("col", "row", "x", "y"))
  expect_equal(nrow(grid), 6)  # 3 cols * 2 rows
})

test_that("hex_grid_centers returns empty for zero dimensions", {
  grid0 <- gghoneycomb:::hex_grid_centers(0, 5, 1)
  expect_equal(nrow(grid0), 0)

  grid1 <- gghoneycomb:::hex_grid_centers(5, 0, 1)
  expect_equal(nrow(grid1), 0)
})

test_that("hex_grid_centers uses correct horizontal spacing dx = 1.5*r", {
  r <- 2
  grid <- gghoneycomb:::hex_grid_centers(3, 1, r)

  # Expected dx = 1.5 * r = 3
  dx_expected <- 1.5 * r

  # Check spacing between consecutive columns
  col0 <- grid[grid$col == 0, ]
  col1 <- grid[grid$col == 1, ]
  col2 <- grid[grid$col == 2, ]

  expect_equal(col1$x - col0$x, dx_expected, tolerance = 1e-10)
  expect_equal(col2$x - col1$x, dx_expected, tolerance = 1e-10)
})

test_that("hex_grid_centers uses correct vertical spacing dy = sqrt(3)*r", {
  r <- 2
  grid <- gghoneycomb:::hex_grid_centers(1, 3, r)

  # Expected dy = sqrt(3) * r
 dy_expected <- sqrt(3) * r

  # Check spacing between consecutive rows (column 0 is even, no offset)
  row0 <- grid[grid$row == 0, ]
  row1 <- grid[grid$row == 1, ]
  row2 <- grid[grid$row == 2, ]

  expect_equal(row1$y - row0$y, dy_expected, tolerance = 1e-10)
  expect_equal(row2$y - row1$y, dy_expected, tolerance = 1e-10)
})

test_that("hex_grid_centers applies odd column vertical shift dy/2", {
  r <- 2
  grid <- gghoneycomb:::hex_grid_centers(2, 1, r)

  dy <- sqrt(3) * r

  # Column 0 (even): row 0 at y = 0
  col0 <- grid[grid$col == 0, ]
  expect_equal(col0$y, 0, tolerance = 1e-10)

  # Column 1 (odd): row 0 at y = dy/2
  col1 <- grid[grid$col == 1, ]
  expect_equal(col1$y, dy / 2, tolerance = 1e-10)
})

test_that("hex_grid_centers col/row indices are 0-based integers", {
  grid <- gghoneycomb:::hex_grid_centers(4, 3, 1)

  expect_type(grid$col, "integer")
  expect_type(grid$row, "integer")

  expect_equal(min(grid$col), 0L)
  expect_equal(max(grid$col), 3L)
  expect_equal(min(grid$row), 0L)
  expect_equal(max(grid$row), 2L)
})

test_that("hex_neighbors returns 6 neighbors", {
  # Even column
  neighbors_even <- gghoneycomb:::hex_neighbors(0)
  expect_equal(nrow(neighbors_even), 6)
  expect_named(neighbors_even, c("dcol", "drow"))

  # Odd column
  neighbors_odd <- gghoneycomb:::hex_neighbors(1)
  expect_equal(nrow(neighbors_odd), 6)
  expect_named(neighbors_odd, c("dcol", "drow"))
})

test_that("hex_neighbors offsets differ for even vs odd columns", {
  neighbors_even <- gghoneycomb:::hex_neighbors(0)
  neighbors_odd <- gghoneycomb:::hex_neighbors(1)

  # The offsets should be different (odd-q coordinate system)
  # Specifically, the drow values differ
  expect_false(identical(neighbors_even, neighbors_odd))
})

test_that("hex_neighbors even column offsets are correct", {
  # For even columns in odd-q:
  # Neighbors at: (+1,+1), (+1,0), (0,-1), (-1,0), (-1,+1), (0,+1)
  neighbors <- gghoneycomb:::hex_neighbors(0)

  # Check that we have the expected set of offsets
  expected <- data.frame(
    dcol = c(1L, 1L, 0L, -1L, -1L, 0L),
    drow = c(1L, 0L, -1L, 0L, 1L, 1L)
  )

  expect_equal(neighbors, expected)
})

test_that("hex_neighbors odd column offsets are correct", {
  # For odd columns in odd-q:
  # Neighbors at: (+1,0), (+1,-1), (0,-1), (-1,-1), (-1,0), (0,+1)
  neighbors <- gghoneycomb:::hex_neighbors(1)

  expected <- data.frame(
    dcol = c(1L, 1L, 0L, -1L, -1L, 0L),
    drow = c(0L, -1L, -1L, -1L, 0L, 1L)
  )

  expect_equal(neighbors, expected)
})

test_that("hex_neighbors row parameter is accepted but doesn't affect result", {
  # The row parameter exists for API consistency but doesn't change offsets
  neighbors_r0 <- gghoneycomb:::hex_neighbors(0, 0)
  neighbors_r5 <- gghoneycomb:::hex_neighbors(0, 5)

  expect_equal(neighbors_r0, neighbors_r5)
})

test_that("hex_grid_centers single cell grid works", {
  grid <- gghoneycomb:::hex_grid_centers(1, 1, 1)

  expect_equal(nrow(grid), 1)
  expect_equal(grid$col, 0L)
  expect_equal(grid$row, 0L)
  expect_equal(grid$x, 0)
  expect_equal(grid$y, 0)
})

test_that("hex_vertices with different radii scale correctly", {
  verts1 <- gghoneycomb:::hex_vertices(0, 0, 1)
  verts2 <- gghoneycomb:::hex_vertices(0, 0, 2)

  # Vertices should scale linearly with radius
  expect_equal(verts2$x, verts1$x * 2, tolerance = 1e-10)
  expect_equal(verts2$y, verts1$y * 2, tolerance = 1e-10)
})

test_that("hex_grid_centers larger grid has correct count", {
  grid <- gghoneycomb:::hex_grid_centers(10, 10, 1)
  expect_equal(nrow(grid), 100)
})

test_that("hex_neighbors covers all 6 directions", {
  # For any column, we should have 6 unique neighbor directions
  neighbors_even <- gghoneycomb:::hex_neighbors(0)
  neighbors_odd <- gghoneycomb:::hex_neighbors(1)

  # Each should have 6 unique (dcol, drow) pairs
  unique_even <- unique(paste(neighbors_even$dcol, neighbors_even$drow))
  unique_odd <- unique(paste(neighbors_odd$dcol, neighbors_odd$drow))

  expect_equal(length(unique_even), 6)
  expect_equal(length(unique_odd), 6)
})
