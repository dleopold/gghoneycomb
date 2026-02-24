test_that("hex_region_perimeter: single cell = 6", {
  alloc <- data.frame(col = 0L, row = 0L, category = "A")
  expect_equal(hex_region_perimeter(alloc, "A"), 6L)
})

test_that("hex_region_perimeter: two adjacent cells = 10", {
  # (0,0) and (1,0) are adjacent in odd-q offset coords
  alloc <- data.frame(
    col = c(0L, 1L),
    row = c(0L, 0L),
    category = c("A", "A")
  )
  expect_equal(hex_region_perimeter(alloc, "A"), 10L)
})

test_that("hex_region_perimeter: three-in-line chain = 14", {
  # (0,0), (1,0), (2,0) form a line
  alloc <- data.frame(
    col = c(0L, 1L, 2L),
    row = c(0L, 0L, 0L),
    category = c("A", "A", "A")
  )
  expect_equal(hex_region_perimeter(alloc, "A"), 14L)
})

test_that("hex_region_perimeter: empty region returns 0", {
  alloc <- data.frame(col = 0L, row = 0L, category = "A")
  expect_equal(hex_region_perimeter(alloc, "B"), 0L)
})

test_that("hex_region_perimeter: filters by category", {
  alloc <- data.frame(
    col = c(0L, 1L, 2L),
    row = c(0L, 0L, 0L),
    category = c("A", "B", "A")
  )
  # Two A-cells at (0,0) and (2,0) are NOT adjacent (separated by B at (1,0))
  # Each has 0 in-region neighbors → 6 + 6 = 12
  expect_equal(hex_region_perimeter(alloc, "A"), 12L)
  # One B-cell at (1,0) → 6
  expect_equal(hex_region_perimeter(alloc, "B"), 6L)
})

test_that("hex_region_perimeter: input validation", {
  expect_error(hex_region_perimeter("not_df", "A"), "must be a data.frame")
  expect_error(
    hex_region_perimeter(data.frame(col = 1L, row = 1L), "A"),
    "must have columns"
  )
  alloc <- data.frame(col = 0L, row = 0L, category = "A")
  expect_error(hex_region_perimeter(alloc, 42), "length-1 character")
  expect_error(hex_region_perimeter(alloc, c("A", "B")), "length-1 character")
})

test_that("hex_isoperimetric_lower_bound: known values", {
  expect_equal(hex_isoperimetric_lower_bound(1), 6L)
  expect_equal(hex_isoperimetric_lower_bound(2), 10L)
  expect_equal(hex_isoperimetric_lower_bound(7), 18L)
})

test_that("hex_isoperimetric_lower_bound: n < 1 returns 0", {
  expect_equal(hex_isoperimetric_lower_bound(0), 0L)
  expect_equal(hex_isoperimetric_lower_bound(-1), 0L)
})

test_that("hex_region_compactness_ratio: single cell = 1", {
  alloc <- data.frame(col = 0L, row = 0L, category = "A")
  expect_equal(hex_region_compactness_ratio(alloc, "A"), 1)
})

test_that("hex_region_compactness_ratio: empty region = 0", {
  alloc <- data.frame(col = 0L, row = 0L, category = "A")
  expect_equal(hex_region_compactness_ratio(alloc, "B"), 0)
})

test_that("hex_region_compactness_ratio: ratio >= 1 for non-empty", {
  # Three-in-line: perimeter 14, lower bound for n=3 is
  # 2*ceiling(sqrt(33)) = 2*ceiling(5.745) = 2*6 = 12
  # ratio = 14/12 ≈ 1.167
  alloc <- data.frame(
    col = c(0L, 1L, 2L),
    row = c(0L, 0L, 0L),
    category = c("A", "A", "A")
  )
  ratio <- hex_region_compactness_ratio(alloc, "A")
  expect_true(ratio >= 1)
  expect_equal(ratio, 14 / 12)
})
