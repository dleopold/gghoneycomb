test_that("generate_silhouette_mask returns integer col/row with exact size", {
  for (shape in c("rect", "rounded", "organic")) {
    mask <- gghoneycomb:::generate_silhouette_mask(100, silhouette = shape)
    expect_s3_class(mask, "data.frame")
    expect_identical(names(mask), c("col", "row"))
    expect_equal(nrow(mask), 100)
    expect_type(mask$col, "integer")
    expect_type(mask$row, "integer")
  }
})

test_that("generate_silhouette_mask always returns connected masks", {
  for (shape in c("rect", "rounded", "organic")) {
    mask <- gghoneycomb:::generate_silhouette_mask(80, silhouette = shape)
    expect_true(gghoneycomb:::hex_is_connected(mask), info = shape)
  }
})

test_that("organic silhouette is reproducible with fixed RNG seed", {
  set.seed(123)
  mask1 <- gghoneycomb:::generate_silhouette_mask(80, silhouette = "organic")
  set.seed(123)
  mask2 <- gghoneycomb:::generate_silhouette_mask(80, silhouette = "organic")
  expect_identical(mask1, mask2)
})

test_that("generate_silhouette_mask respects width and height constraints", {
  mask <- gghoneycomb:::generate_silhouette_mask(
    n_cells = 24,
    silhouette = "rect",
    min_width = 6,
    max_width = 6,
    min_height = 4,
    max_height = 4
  )

  expect_equal(nrow(mask), 24)
  expect_true(all(mask$col >= 0L & mask$col <= 5L))
  expect_true(all(mask$row >= 0L & mask$row <= 3L))

  rounded <- gghoneycomb:::generate_silhouette_mask(
    n_cells = 30,
    silhouette = "rounded",
    min_width = 7,
    max_width = 7,
    min_height = 6,
    max_height = 6
  )

  expect_equal(nrow(rounded), 30)
  expect_true(all(rounded$col >= 0L & rounded$col <= 6L))
  expect_true(all(rounded$row >= 0L & rounded$row <= 5L))
})
