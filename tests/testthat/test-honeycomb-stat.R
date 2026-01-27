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

test_that("stat_honeycomb is reproducible with seed", {
  df <- data.frame(
    category = "A",
    count = 100
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 100, compaction = 0.8, seed = 99)
  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = category, weight = count)) +
    stat_honeycomb(n_cells = 100, compaction = 0.8, seed = 99)

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
