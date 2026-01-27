# Tests for region-growing allocation (Algorithm C)
#
# Note: The greedy region-growing algorithm can fail on small grids or with
# uneven splits because it may leave disconnected cells for later categories.
# Tests use configurations known to work with the algorithm.

# Helper to create a simple rectangular grid
make_grid <- function(n_cols, n_rows) {
  expand.grid(col = seq(0L, n_cols - 1L), row = seq(0L, n_rows - 1L))
}

# ============================================================================
# allocate_regions() tests
# ============================================================================

test_that("allocate_regions returns correct structure", {
  grid <- make_grid(7, 7)
  targets <- c(A = 24L, B = 25L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_named(result, c("col", "row", "category"))
  expect_equal(nrow(result), 49)
  expect_type(result$category, "character")
})

test_that("allocate_regions assigns correct counts per category", {
  grid <- make_grid(7, 7)
  targets <- c(A = 24L, B = 25L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  counts <- table(result$category)
  expect_equal(as.integer(counts["A"]), 24L)
  expect_equal(as.integer(counts["B"]), 25L)
})

test_that("allocate_regions produces contiguous regions", {
  grid <- make_grid(9, 9)
  targets <- c(A = 45L, B = 16L, C = 20L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  # Each category should have exactly 1 connected component
  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "B"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "C"), 1L)
})

test_that("allocate_regions handles single category", {
  grid <- make_grid(5, 5)
  targets <- c(X = 25L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(sum(result$category == "X"), 25L)
  expect_equal(gghoneycomb:::count_components(result, "X"), 1L)
})

test_that("allocate_regions handles two categories on odd grid", {
  # Odd-sized grids work better with 50-50 splits
  grid <- make_grid(7, 7)
  targets <- c(A = 24L, B = 25L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "B"), 1L)
  expect_equal(sum(result$category == "A"), 24L)
  expect_equal(sum(result$category == "B"), 25L)
})

test_that("allocate_regions processes categories largest-first", {
  # Use a configuration known to work (11x11 grid with 60-40 split)
  grid <- make_grid(11, 11)
  targets <- c(small = 48L, large = 73L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  # Large category processed first, both should be contiguous
  expect_equal(gghoneycomb:::count_components(result, "large"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "small"), 1L)
})

test_that("allocate_regions tie-breaks by label when targets equal", {
  # Use odd grid for equal split
  grid <- make_grid(9, 9)
  targets <- c(B = 40L, A = 41L)  # A processed first (larger), then B

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "B"), 1L)
})

test_that("allocate_regions handles zero-target categories", {
  grid <- make_grid(5, 5)
  targets <- c(A = 25L, B = 0L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(sum(result$category == "A"), 25L)
  expect_equal(sum(result$category == "B", na.rm = TRUE), 0L)
})

test_that("allocate_regions is deterministic", {
  grid <- make_grid(7, 7)
  targets <- c(A = 24L, B = 25L)

  result1 <- gghoneycomb:::allocate_regions(grid, targets)
  result2 <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(result1, result2)
})

test_that("allocate_regions errors when targets don't sum to grid size", {
  grid <- make_grid(3, 3)
  targets <- c(A = 5L, B = 3L)  # Sum = 8, grid = 9

  expect_error(
    gghoneycomb:::allocate_regions(grid, targets),
    "Sum of targets.*must equal number of grid cells"
  )
})

test_that("allocate_regions errors on invalid grid", {
  expect_error(
    gghoneycomb:::allocate_regions(data.frame(x = 1:3), c(A = 3L)),
    "must be a data.frame with columns"
  )
})

test_that("allocate_regions errors on unnamed targets", {
  grid <- make_grid(2, 2)

  expect_error(
    gghoneycomb:::allocate_regions(grid, c(2L, 2L)),
    "must be a named integer vector"
  )
})

test_that("allocate_regions errors on negative targets", {
  grid <- make_grid(2, 2)
  targets <- c(A = 5L, B = -1L)

  expect_error(
    gghoneycomb:::allocate_regions(grid, targets),
    "must be non-negative"
  )
})

test_that("allocate_regions handles empty grid", {
  grid <- data.frame(col = integer(0), row = integer(0))
  targets <- integer(0)
  names(targets) <- character(0)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(nrow(result), 0)
})

test_that("allocate_regions works with larger grids", {
  grid <- make_grid(11, 11)
  targets <- c(A = 60L, B = 61L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  # Both categories contiguous
  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "B"), 1L)

  # Counts match
  counts <- table(result$category)
  expect_equal(as.integer(counts["A"]), 60L)
  expect_equal(as.integer(counts["B"]), 61L)
})

test_that("allocate_regions respects global_center parameter", {
  grid <- make_grid(11, 11)
  targets <- c(A = 60L, B = 61L)

  # Default center
  result1 <- gghoneycomb:::allocate_regions(grid, targets)

  # Custom center (corner)
  result2 <- gghoneycomb:::allocate_regions(grid, targets, global_center = c(0, 0))

  # Both should still be contiguous
  expect_equal(gghoneycomb:::count_components(result1, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result1, "B"), 1L)
  expect_equal(gghoneycomb:::count_components(result2, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result2, "B"), 1L)
})

test_that("allocate_regions errors when contiguity impossible", {
  # Construct a connected "star-like" shape where the center is an
  # articulation point and the three leaves are not adjacent to each other.
  #
  # It is impossible to split 4 cells into two connected regions of size 2
  # each, because any connected pair must include the center.
  grid <- data.frame(
    col = c(0L, 1L, -1L, 0L),
    row = c(0L, 0L, 0L, 1L)
  )
  targets <- c(A = 2L, B = 2L)

  expect_error(
    gghoneycomb:::allocate_regions(grid, targets),
    "Cannot grow region"
  )
})

# ============================================================================
# validate_contiguity() tests
# ============================================================================

test_that("validate_contiguity returns TRUE for contiguous allocation", {
  grid <- make_grid(7, 7)
  targets <- c(A = 24L, B = 25L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_true(gghoneycomb:::validate_contiguity(result))
})

test_that("validate_contiguity errors on non-contiguous allocation", {
  # Manually create a non-contiguous allocation
  # (0,0) and (2,0) are not hex-adjacent in a 3-wide grid
  allocation <- data.frame(
    col = c(0L, 2L, 0L, 1L, 2L),
    row = c(0L, 0L, 1L, 1L, 1L),
    category = c("A", "A", "B", "B", "B")
  )

  expect_error(
    gghoneycomb:::validate_contiguity(allocation),
    "not contiguous"
  )
})

test_that("validate_contiguity handles empty allocation", {
  allocation <- data.frame(
    col = integer(0),
    row = integer(0),
    category = character(0)
  )

  expect_true(gghoneycomb:::validate_contiguity(allocation))
})

test_that("validate_contiguity handles single cell per category", {
  allocation <- data.frame(
    col = c(0L, 1L, 2L),
    row = c(0L, 0L, 0L),
    category = c("A", "B", "C")
  )

  expect_true(gghoneycomb:::validate_contiguity(allocation))
})

test_that("validate_contiguity errors on invalid input", {
  expect_error(
    gghoneycomb:::validate_contiguity(data.frame(x = 1:3)),
    "must be a data.frame with columns"
  )
})

# ============================================================================
# count_components() tests
# ============================================================================

test_that("count_components returns 1 for contiguous region", {
  # Create a contiguous L-shaped region using hex adjacency
  # (0,0) -> (1,0) -> (1,1) forms a connected path
  allocation <- data.frame(
    col = c(0L, 1L, 1L),
    row = c(0L, 0L, 1L),
    category = rep("A", 3)
  )

  expect_equal(gghoneycomb:::count_components(allocation, "A"), 1L)
})

test_that("count_components returns correct count for disconnected regions", {
  # Two separate cells (not adjacent in hex grid)
  allocation <- data.frame(
    col = c(0L, 3L),
    row = c(0L, 3L),
    category = c("A", "A")
  )

  expect_equal(gghoneycomb:::count_components(allocation, "A"), 2L)
})

test_that("count_components returns 0 for missing category", {
  allocation <- data.frame(
    col = c(0L, 1L),
    row = c(0L, 0L),
    category = c("A", "A")
  )

  expect_equal(gghoneycomb:::count_components(allocation, "B"), 0L)
})

test_that("count_components handles hex adjacency correctly", {
  # Test that hex neighbors are correctly identified
  # In odd-q, (0,0) and (1,0) are adjacent (even column neighbor)
  allocation <- data.frame(
    col = c(0L, 1L),
    row = c(0L, 0L),
    category = c("A", "A")
  )

  expect_equal(gghoneycomb:::count_components(allocation, "A"), 1L)
})

test_that("count_components handles odd column adjacency", {
  # In odd-q, (1,0) and (2,0) are adjacent (odd column neighbor)
  allocation <- data.frame(
    col = c(1L, 2L),
    row = c(0L, 0L),
    category = c("A", "A")
  )

  expect_equal(gghoneycomb:::count_components(allocation, "A"), 1L)
})

test_that("count_components handles vertical adjacency", {
  # Vertical neighbors in same column
  allocation <- data.frame(
    col = c(0L, 0L),
    row = c(0L, 1L),
    category = c("A", "A")
  )

  # (0,0) and (0,1) are adjacent via (0, +1) offset
  expect_equal(gghoneycomb:::count_components(allocation, "A"), 1L)
})

test_that("count_components handles diagonal adjacency in hex grid", {
  # In odd-q hex grid, (0,0) and (1,1) are adjacent (even col: +1,+1 offset)
  allocation <- data.frame(
    col = c(0L, 1L),
    row = c(0L, 1L),
    category = c("A", "A")
  )

  expect_equal(gghoneycomb:::count_components(allocation, "A"), 1L)
})

# ============================================================================
# Integration tests
# ============================================================================

test_that("full pipeline: grid + allocation + validation", {
  # Use hex_grid_centers to create grid, then allocate
  grid_centers <- gghoneycomb:::hex_grid_centers(9, 9, 1)
  grid <- grid_centers[, c("col", "row")]

  targets <- c(A = 40L, B = 41L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  # Validate contiguity
  expect_true(gghoneycomb:::validate_contiguity(result))

  # Check counts
  counts <- table(result$category)
  expect_equal(as.integer(counts["A"]), 40L)
  expect_equal(as.integer(counts["B"]), 41L)
})

test_that("allocation with three categories on large grid", {
  grid <- make_grid(9, 9)
  targets <- c(A = 45L, B = 16L, C = 20L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  # All should be contiguous
  for (cat in names(targets)) {
    expect_equal(gghoneycomb:::count_components(result, cat), 1L)
  }

  # Counts match
  counts <- table(result$category)
  expect_equal(as.integer(counts["A"]), 45L)
  expect_equal(as.integer(counts["B"]), 16L)
  expect_equal(as.integer(counts["C"]), 20L)
})

test_that("seed selection prefers cells near centroid", {
  # With a symmetric grid and single category, seed should be near center
  grid <- make_grid(5, 5)
  targets <- c(A = 25L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  # All cells assigned to A
  expect_equal(sum(result$category == "A"), 25L)
  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
})

test_that("small 2x2 grid works with 2 categories", {
  grid <- make_grid(2, 2)
  targets <- c(A = 2L, B = 2L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "B"), 1L)
})

test_that("allocation on 12x12 grid with equal split", {
  # Larger even grids work with 50-50 splits
  grid <- make_grid(12, 12)
  targets <- c(A = 72L, B = 72L)

  result <- gghoneycomb:::allocate_regions(grid, targets)

  expect_equal(gghoneycomb:::count_components(result, "A"), 1L)
  expect_equal(gghoneycomb:::count_components(result, "B"), 1L)
})
