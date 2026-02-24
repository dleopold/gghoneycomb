test_that("round_proportions: basic case with exact division", {
  # 3 categories, 100 cells, exact proportions
  props <- c(0.5, 0.3, 0.2)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(result, c(50, 30, 20))
  expect_equal(sum(result), 100)
})

test_that("round_proportions: sum preservation with rounding", {
  # 3 categories, 97 cells (not evenly divisible)
  props <- c(0.5, 0.3, 0.2)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 97, labels)

  # Must sum to 97
  expect_equal(sum(result), 97)
  # All must be positive
  expect_true(all(result > 0))
})

test_that("round_proportions: deterministic tie-breaking by label", {
  # Equal proportions should be broken by label order
  props <- c(1/3, 1/3, 1/3)
  labels <- c("C", "B", "A")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  # With 100 cells and 3 equal categories: 33, 33, 34 (or similar)
  # The largest remainder goes to the category with largest fractional part
  # All should be close to 33
  expect_true(all(result >= 33 & result <= 34))
})

test_that("round_proportions: minimum 1 enforcement for nonzero categories", {
  # Very small category should still get 1 cell
  # With 100 cells and 3 categories, we have room for minimum enforcement
  props <- c(0.98, 0.01, 0.01)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  # B and C should each get at least 1
  expect_true(result[2] >= 1)
  expect_true(result[3] >= 1)
  # A should get most
  expect_true(result[1] >= 98)
})

test_that("round_proportions: minimum enforcement can subtract repeatedly", {
  # One dominant category and many tiny categories.
  # With 20 cells and 10 nonzero categories, everyone must get >= 1.
  props <- c(0.91, rep(0.01, 9))
  labels <- c("A", paste0("B", 1:9))
  result <- gghoneycomb:::round_proportions(props, 20, labels)

  expect_equal(sum(result), 20)
  expect_equal(result[1], 11)
  expect_true(all(result[-1] == 1))
})

test_that("round_proportions: error when n_cells < nonzero categories", {
  # 3 nonzero categories but only 2 cells
  props <- c(0.5, 0.3, 0.2)
  labels <- c("A", "B", "C")

  expect_error(
    gghoneycomb:::round_proportions(props, 2, labels),
    "n_cells.*must be >= number of nonzero categories"
  )
})

test_that("round_proportions: error when proportions don't sum to 1", {
  props <- c(0.5, 0.3, 0.1)  # sums to 0.9
  labels <- c("A", "B", "C")

  expect_error(
    gghoneycomb:::round_proportions(props, 100, labels),
    "proportions must sum to 1"
  )
})

test_that("round_proportions: error when n_cells is not positive", {
  props <- c(0.5, 0.3, 0.2)
  labels <- c("A", "B", "C")

  expect_error(
    gghoneycomb:::round_proportions(props, 0, labels),
    "n_cells must be a positive integer"
  )

  expect_error(
    gghoneycomb:::round_proportions(props, -10, labels),
    "n_cells must be a positive integer"
  )
})

test_that("round_proportions: error when lengths don't match", {
  props <- c(0.5, 0.3, 0.2)
  labels <- c("A", "B")  # Only 2 labels

  expect_error(
    gghoneycomb:::round_proportions(props, 100, labels),
    "proportions and labels must have the same length"
  )
})

test_that("round_proportions: single category", {
  props <- c(1.0)
  labels <- c("A")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(result, 100)
})

test_that("round_proportions: two categories with fractional split", {
  # 50-50 split, 101 cells
  props <- c(0.5, 0.5)
  labels <- c("A", "B")
  result <- gghoneycomb:::round_proportions(props, 101, labels)

  expect_equal(sum(result), 101)
  # One should get 51, one should get 50
  expect_true((result[1] == 51 && result[2] == 50) ||
    (result[1] == 50 && result[2] == 51))
})

test_that("round_proportions: many small categories", {
  # 10 equal categories, 100 cells
  props <- rep(0.1, 10)
  labels <- paste0("Cat", 1:10)
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  expect_equal(length(result), 10)
  # All should be 10
  expect_true(all(result == 10))
})

test_that("round_proportions: very small n_cells with minimum enforcement", {
  # 3 categories, 3 cells (minimum feasible)
  # With equal proportions, each gets 1
  props <- c(1/3, 1/3, 1/3)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 3, labels)

  expect_equal(sum(result), 3)
  expect_equal(result, c(1, 1, 1))
})

test_that("round_proportions: zero proportions are ignored", {
  # One category has zero proportion
  props <- c(0.6, 0.4, 0.0)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  # C should get 0 cells
  expect_equal(result[3], 0)
  # A and B should split 100
  expect_equal(result[1] + result[2], 100)
})

test_that("round_proportions: floating-point precision handling", {
  # Proportions that sum to 1 but with floating-point error
  props <- c(0.1, 0.2, 0.3, 0.4)
  labels <- c("A", "B", "C", "D")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  expect_equal(result, c(10, 20, 30, 40))
})

test_that("round_proportions: large n_cells", {
  # Test with larger cell count
  props <- c(0.25, 0.25, 0.25, 0.25)
  labels <- c("A", "B", "C", "D")
  result <- gghoneycomb:::round_proportions(props, 1000, labels)

  expect_equal(sum(result), 1000)
  expect_true(all(result == 250))
})

test_that("round_proportions: remainder distribution by fractional part", {
  # Proportions that create specific fractional remainders
  # 0.333... * 100 = 33.333...
  props <- c(1/3, 1/3, 1/3)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  # Should be 33, 33, 34 (or permutation)
  # The one with largest fractional part gets the extra
  expect_true(all(result %in% c(33, 34)))
  expect_equal(sum(result == 34), 1)
})

test_that("round_proportions: stable tie-breaking with identical fractional parts", {
  # Create a scenario where multiple categories have same fractional part
  # and verify tie-break is by label
  props <- c(0.25, 0.25, 0.25, 0.25)
  labels <- c("D", "C", "B", "A")
  result <- gghoneycomb:::round_proportions(props, 101, labels)

  expect_equal(sum(result), 101)
  # One category gets 26, others get 25
  # With tie-break by label, "A" should get the extra (alphabetically first)
  expect_equal(result[4], 26)  # "A" is at index 4
})

test_that("round_proportions: edge case with very unequal proportions", {
  # One dominant category
  props <- c(0.98, 0.01, 0.01)
  labels <- c("A", "B", "C")
  result <- gghoneycomb:::round_proportions(props, 100, labels)

  expect_equal(sum(result), 100)
  # A should get ~98, B and C should each get at least 1
  expect_true(result[1] >= 98)
  expect_true(result[2] >= 1)
  expect_true(result[3] >= 1)
})
