test_that("honey_pal returns valid hex colors", {
  pal <- honey_pal(6)
  expect_length(pal, 6)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("honey_pal interpolates for larger n", {
  pal <- honey_pal(10)
  expect_length(pal, 10)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("honey scales return discrete scale objects", {
  expect_s3_class(suppressWarnings(scale_fill_honey()), "ScaleDiscrete")
  expect_s3_class(suppressWarnings(scale_color_honey()), "ScaleDiscrete")
})
