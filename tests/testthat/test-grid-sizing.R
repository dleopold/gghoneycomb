# Tests for choose_grid_size() - Algorithm B implementation
# Algorithm prioritizes squareness (abs(w-h)) first, then area, then width

test_that("n_cells=100 returns (10, 10)", {
  result <- choose_grid_size(100)
  expect_equal(result$width, 10)
  expect_equal(result$height, 10)
})

test_that("n_cells=97 returns (10, 10)", {
  # 97 cells need ceiling(97/10) = 10 rows, so 10x10 grid
  # Algorithm prioritizes squareness: diff=0 for 10x10 beats diff>0 for others
  result <- choose_grid_size(97)
  expect_equal(result$width, 10)
  expect_equal(result$height, 10)
})

test_that("impossible constraints produce error", {
  # max_width=5, max_height=5 can only fit 25 cells
  expect_error(
    choose_grid_size(100, max_width = 5, max_height = 5),
    "Cannot fit 100 cells"
  )
})

test_that("min/max boundaries are respected", {
  # Force a specific width range
  result <- choose_grid_size(100, min_width = 20, max_width = 25)
  expect_gte(result$width, 20)
  expect_lte(result$width, 25)
  expect_gte(result$width * result$height, 100)

  # Force a specific height range
  result <- choose_grid_size(100, min_height = 5, max_height = 10)
  expect_gte(result$height, 5)
  expect_lte(result$height, 10)
  expect_gte(result$width * result$height, 100)
})

test_that("prioritizes squareness over area", {
  # For n_cells=99:
  # w=9: h=ceiling(99/9)=11, area=99, diff=2
  # w=10: h=ceiling(99/10)=10, area=100, diff=0
  # Squareness first: diff=0 beats diff=2, so (10,10) wins
  result <- choose_grid_size(99)
  expect_equal(result$width, 10)
  expect_equal(result$height, 10)
})

test_that("tie-breaks by area then w", {
  # For n_cells=12:
  # w=3: h=ceiling(12/3)=4, area=12, diff=1
  # w=4: h=ceiling(12/4)=3, area=12, diff=1
  # w=6: h=ceiling(12/6)=2, area=12, diff=4
  # Best diff=1, then smallest area (both 12), then smallest w: w=3
  result <- choose_grid_size(12)
  expect_equal(result$width, 3)
  expect_equal(result$height, 4)
})

test_that("prime number of cells uses square-ish grid", {
  # n_cells=97 is prime
  # Algorithm finds the most square grid: 10x10 (diff=0)
  result <- choose_grid_size(97)
  expect_equal(result$width, 10)
  expect_equal(result$height, 10)
})

test_that("small n_cells values work", {
  result <- choose_grid_size(1)
  expect_equal(result$width, 1)
  expect_equal(result$height, 1)

  result <- choose_grid_size(2)
  expect_equal(result$width, 1)
  expect_equal(result$height, 2)

  result <- choose_grid_size(4)
  expect_equal(result$width, 2)
  expect_equal(result$height, 2)
})

test_that("constraints that force non-square grid", {
  # Force width to be exactly 5
  result <- choose_grid_size(100, min_width = 5, max_width = 5)
  expect_equal(result$width, 5)
  expect_equal(result$height, 20)

  # Force height to be exactly 5
  result <- choose_grid_size(100, min_height = 5, max_height = 5)
  expect_equal(result$width, 20)
  expect_equal(result$height, 5)
})

test_that("infeasible height constraint produces error", {
  # n_cells=100, force width=1 and height must be exactly 50
  # w=1: h=ceiling(100/1)=100, but max_height=50, so infeasible
  expect_error(
    choose_grid_size(100, min_width = 1, max_width = 1,
                     min_height = 50, max_height = 50),
    "Cannot fit|No feasible"
  )
})

test_that("input validation works", {
  expect_error(choose_grid_size(0), "positive integer")
  expect_error(choose_grid_size(-1), "positive integer")
  expect_error(choose_grid_size(1.5), "positive integer")
  expect_error(choose_grid_size(NA), "positive integer")
  expect_error(choose_grid_size("100"), "positive integer")

  expect_error(choose_grid_size(100, min_width = 0), "positive integer")
  expect_error(choose_grid_size(100, max_width = -1), "positive integer")
  expect_error(choose_grid_size(100, min_height = 1.5), "positive integer")
  expect_error(choose_grid_size(100, max_height = NA), "positive integer")
})

test_that("min > max constraint produces error", {
  expect_error(
    choose_grid_size(100, min_width = 20, max_width = 10),
    "cannot exceed"
  )
  expect_error(
    choose_grid_size(100, min_height = 20, max_height = 10),
    "cannot exceed"
  )
})

test_that("large n_cells handled correctly", {
  result <- choose_grid_size(2500)
  expect_equal(result$width, 50)
  expect_equal(result$height, 50)
  expect_gte(result$width * result$height, 2500)
})

test_that("result always has capacity >= n_cells", {
  for (n in c(1, 7, 13, 50, 99, 100, 101, 500)) {
    result <- choose_grid_size(n)
    expect_true(result$width * result$height >= n,
                info = paste("n_cells =", n))
  }
})

test_that("explicit wide constraints still prefer square grids", {
  # Even with wide constraints, algorithm prefers square grids
  # For n_cells=97 with max_width=100, max_height=100:
  # w=10, h=10 has diff=0, which beats w=1, h=97 with diff=96
  result <- choose_grid_size(97, max_width = 100, max_height = 100)
  expect_equal(result$width, 10)
  expect_equal(result$height, 10)
})

test_that("when forced to non-square, minimizes area", {
  # Force width to be 1-2, so grid must be tall
  # n_cells=10: w=1 gives h=10 (diff=9), w=2 gives h=5 (diff=3)
  # diff=3 < diff=9, so w=2 wins
  result <- choose_grid_size(10, min_width = 1, max_width = 2)
  expect_equal(result$width, 2)
  expect_equal(result$height, 5)
})
