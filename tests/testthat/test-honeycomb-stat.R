test_that("stat_honeycomb returns expected computed columns", {
  df <- data.frame(
    category = c("A", "B"),
    count = c(24, 25)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 49, seed = 1)

  ld <- ggplot2::layer_data(p)

  expect_true(all(c(
    "x", "y", "group", "fill", "cell_id", "grid_col", "grid_row", "pct", "tooltip_label"
  ) %in% names(ld)))
  expect_true(all(!is.na(ld$x)))
  expect_true(all(!is.na(ld$y)))
})

test_that("stat_honeycomb produces contiguous regions per category", {
  df <- data.frame(
    category = c("A", "B", "C"),
    count = c(45, 16, 20)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 81, seed = 42)

  ld <- ggplot2::layer_data(p)
  cells <- ld[!duplicated(ld$cell_id), c("cell_id", "tooltip_label", "grid_col", "grid_row")]
  cells$category <- sub(":.*", "", cells$tooltip_label)

  is_contiguous <- function(cell_df) {
    if (nrow(cell_df) == 0) {
      return(TRUE)
    }

    key <- paste(cell_df$grid_col, cell_df$grid_row, sep = ":")
    idx_map <- stats::setNames(seq_len(nrow(cell_df)), key)
    visited <- rep(FALSE, nrow(cell_df))
    queue <- integer(0)

    queue <- c(queue, 1L)
    visited[1] <- TRUE

    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]

      col <- cell_df$grid_col[current]
      row <- cell_df$grid_row[current]
      offsets <- gghoneycomb:::hex_neighbors(col, row)

      for (i in seq_len(nrow(offsets))) {
        ncol <- col + offsets$dcol[i]
        nrow <- row + offsets$drow[i]
        key_neighbor <- paste(ncol, nrow, sep = ":")

        if (key_neighbor %in% names(idx_map)) {
          idx <- idx_map[[key_neighbor]]
          if (!visited[idx]) {
            visited[idx] <- TRUE
            queue <- c(queue, idx)
          }
        }
      }
    }

    all(visited)
  }

  for (cat in unique(cells$category)) {
    subset_cells <- cells[cells$category == cat, , drop = FALSE]
    expect_true(is_contiguous(subset_cells))
  }
})

test_that("stat_honeycomb computes full coordinates with multiple categories", {
  df <- data.frame(
    category = c("Clover", "Wildflower", "Lavender", "Sunflower", "Other"),
    count = c(45, 25, 15, 10, 5)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 80, seed = 1)

  ld <- ggplot2::layer_data(p)
  expect_true(all(!is.na(ld$x)))
  expect_true(all(!is.na(ld$y)))

  cells <- ld[!duplicated(ld$cell_id), c("grid_col", "grid_row", "tooltip_label")]
  allocation <- data.frame(
    col = cells$grid_col,
    row = cells$grid_row,
    category = sub(":.*", "", cells$tooltip_label),
    stringsAsFactors = FALSE
  )
  for (cat in unique(allocation$category)) {
    expect_equal(gghoneycomb:::count_components(allocation, cat), 1L)
  }
})

test_that("stat_honeycomb is reproducible with seed", {
  df <- data.frame(
    category = "A",
    count = 100
  )
  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 100, seed = 99)
  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 100, seed = 99)
  ld1 <- ggplot2::layer_data(p1)
  ld2 <- ggplot2::layer_data(p2)
  cells1 <- ld1[!duplicated(ld1$cell_id), c("cell_id", "grid_col", "grid_row")]
  cells2 <- ld2[!duplicated(ld2$cell_id), c("cell_id", "grid_col", "grid_row")]
  cells1 <- cells1[order(cells1$cell_id), ]
  cells2 <- cells2[order(cells2$cell_id), ]
  expect_equal(cells1, cells2)
})

test_that("stat_honeycomb errors on impossible constraints", {
  df <- data.frame(
    category = c("A", "B"),
    count = c(60, 40)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 20, min_width = 1, max_width = 1, min_height = 1, max_height = 1)

  expect_error(ggplot2::layer_data(p), "Cannot fit 20 cells")
})

test_that("stat_honeycomb errors on invalid weights", {
  df_zero <- data.frame(
    category = c("A", "B"),
    count = c(0, 0)
  )

  p_zero <- ggplot2::ggplot(df_zero, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb()
  expect_error(ggplot2::layer_data(p_zero), "No positive weights")

  df_neg <- data.frame(
    category = c("A", "B"),
    count = c(-10, -5)
  )

  p_neg <- ggplot2::ggplot(df_neg, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb()
  expect_error(ggplot2::layer_data(p_neg), "No positive weights")
})

test_that("stat_honeycomb rejects non-summing proportions", {
  df <- data.frame(
    category = c("A", "B", "C"),
    prop = c(0.5, 0.3, 0.1)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = prop)) +
    stat_honeycomb(values_are = "proportions")

  expect_error(ggplot2::layer_data(p), "must sum to 1")
})

test_that("stat_honeycomb handles single category", {
  df <- data.frame(
    category = "Only",
    count = 100
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 20)

  ld <- ggplot2::layer_data(p)
  cells <- ld[!duplicated(ld$cell_id), c("cell_id", "tooltip_label")]
  categories <- sub(":.*", "", cells$tooltip_label)
  expect_true(all(categories == "Only"))
  expect_equal(nrow(cells), 20)
})

test_that("free mode is reproducible with same seed and temperature", {
  df <- data.frame(
    category = c("A", "B", "C"),
    count = c(50, 30, 20)
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 49, layout = "free", temperature = 0.35, seed = 7)
  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 49, layout = "free", temperature = 0.35, seed = 7)

  ld1 <- ggplot2::layer_data(p1)
  ld2 <- ggplot2::layer_data(p2)

  cells1 <- ld1[!duplicated(ld1$cell_id), c("cell_id", "grid_col", "grid_row", "fill")]
  cells2 <- ld2[!duplicated(ld2$cell_id), c("cell_id", "grid_col", "grid_row", "fill")]
  cells1 <- cells1[order(cells1$cell_id), ]
  cells2 <- cells2[order(cells2$cell_id), ]

  expect_equal(cells1, cells2)
})

test_that("blocky + organic silhouette errors", {
  expect_error(
    stat_honeycomb(compact_style = "blocky", silhouette = "organic"),
    "blocky.*requires.*rect"
  )
})

test_that("blocky end-to-end produces non-NA coordinates", {
  df <- data.frame(
    category = c("X", "Y", "Z"),
    count = c(40, 35, 25)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(
      n_cells = 49, silhouette = "rect", layout = "compact",
      compact_style = "blocky", seed = 42
    )

  ld <- ggplot2::layer_data(p)

  expect_true(all(c("x", "y", "group", "fill", "cell_id",
                    "grid_col", "grid_row", "pct", "tooltip_label") %in% names(ld)))
  expect_true(all(!is.na(ld$x)))
  expect_true(all(!is.na(ld$y)))
  expect_equal(length(unique(ld$fill)), 3)
})

test_that("rotation transforms output coordinates in 90-degree increments", {
  df <- data.frame(
    category = c("A", "B", "C"),
    count = c(45, 30, 25)
  )

  p0 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 81, seed = 11, rotation = 0)
  p90 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 81, seed = 11, rotation = 90)
  p180 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 81, seed = 11, rotation = 180)

  ld0 <- ggplot2::layer_data(p0)
  ld90 <- ggplot2::layer_data(p90)
  ld180 <- ggplot2::layer_data(p180)

  expect_equal(ld90$x, -ld0$y)
  expect_equal(ld90$y, ld0$x)
  expect_equal(ld180$x, -ld0$x)
  expect_equal(ld180$y, -ld0$y)

  expect_equal(ld90$grid_col, ld0$grid_col)
  expect_equal(ld90$grid_row, ld0$grid_row)
  expect_equal(ld90$cell_id, ld0$cell_id)
  expect_equal(ld90$fill, ld0$fill)
})

test_that("rotation validates allowed values", {
  expect_error(
    stat_honeycomb(rotation = 45),
    "rotation.*0, 90, 180, 270"
  )

  expect_error(
    geom_honeycomb(rotation = -90),
    "rotation.*0, 90, 180, 270"
  )
})
