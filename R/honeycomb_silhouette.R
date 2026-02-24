# Connected silhouette mask generation for honeycomb layouts

#' Generate a connected silhouette mask
#'
#' @param n_cells Integer target number of cells.
#' @param silhouette One of "rect", "rounded", or "organic".
#' @param seed Optional integer seed. If NULL, current RNG state is used.
#' @param min_width Integer minimum grid width.
#' @param max_width Integer maximum grid width, or NULL.
#' @param min_height Integer minimum grid height.
#' @param max_height Integer maximum grid height, or NULL.
#'
#' @return Data frame with integer `col` and `row` columns (0-based), exactly
#'   `n_cells` rows.
#'
#' @keywords internal
#' @noRd
generate_silhouette_mask <- function(n_cells,
                                     silhouette,
                                     seed = NULL,
                                     min_width = 1L,
                                     max_width = NULL,
                                     min_height = 1L,
                                     max_height = NULL) {
  if (!is.numeric(n_cells) || length(n_cells) != 1 || is.na(n_cells) ||
      n_cells < 1 || n_cells != floor(n_cells)) {
    rlang::abort("`n_cells` must be a positive integer.")
  }
  n_cells <- as.integer(n_cells)

  valid_silhouettes <- c("rect", "rounded", "organic")
  if (!is.character(silhouette) || length(silhouette) != 1 ||
      !silhouette %in% valid_silhouettes) {
    rlang::abort(paste0(
      "`silhouette` must be one of: ",
      paste0('"', valid_silhouettes, '"', collapse = ", "),
      "."
    ))
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
      rlang::abort("`seed` must be a single integer or NULL.")
    }
    set.seed(as.integer(seed))
  }

  if (silhouette == "rect") {
    grid_size <- choose_grid_size(
      n_cells,
      min_width = min_width,
      max_width = max_width,
      min_height = min_height,
      max_height = max_height
    )
    grid <- hex_grid_centers(grid_size$width, grid_size$height, r = 1)
    mask <- grid[seq_len(n_cells), c("col", "row"), drop = FALSE]
    mask$col <- as.integer(mask$col)
    mask$row <- as.integer(mask$row)
    rownames(mask) <- NULL
    return(mask)
  }

  density <- if (silhouette == "rounded") 0.85 else 0.65
  n0 <- ceiling(n_cells / density)
  grid_size <- choose_grid_size(
    n0,
    min_width = min_width,
    max_width = max_width,
    min_height = min_height,
    max_height = max_height
  )
  candidate <- hex_grid_centers(grid_size$width, grid_size$height, r = 1)
  candidate <- candidate[, c("col", "row"), drop = FALSE]
  candidate$col <- as.integer(candidate$col)
  candidate$row <- as.integer(candidate$row)

  center_col <- mean(candidate$col)
  center_row <- mean(candidate$row)

  dist2 <- (candidate$col - center_col)^2 + (candidate$row - center_row)^2
  start_idx <- order(dist2, candidate$row, candidate$col)[1]

  selected <- candidate[start_idx, c("col", "row"), drop = FALSE]
  selected_set <- new.env(hash = TRUE, parent = emptyenv())
  assign(paste(selected$col[1], selected$row[1], sep = ","), TRUE,
         envir = selected_set)

  candidate_set <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(nrow(candidate))) {
    assign(paste(candidate$col[i], candidate$row[i], sep = ","), TRUE,
           envir = candidate_set)
  }

  while (nrow(selected) < n_cells) {
    frontier_env <- new.env(hash = TRUE, parent = emptyenv())
    frontier_col <- integer(0)
    frontier_row <- integer(0)

    for (i in seq_len(nrow(selected))) {
      offsets <- hex_neighbors(selected$col[i], selected$row[i])
      for (j in seq_len(nrow(offsets))) {
        n_col <- selected$col[i] + offsets$dcol[j]
        n_row <- selected$row[i] + offsets$drow[j]
        key <- paste(n_col, n_row, sep = ",")

        if (exists(key, envir = candidate_set, inherits = FALSE) &&
            !exists(key, envir = selected_set, inherits = FALSE) &&
            !exists(key, envir = frontier_env, inherits = FALSE)) {
          assign(key, TRUE, envir = frontier_env)
          frontier_col <- c(frontier_col, n_col)
          frontier_row <- c(frontier_row, n_row)
        }
      }
    }

    if (length(frontier_col) == 0) {
      rlang::abort("Failed to grow a connected silhouette mask.")
    }

    frontier <- data.frame(col = frontier_col, row = frontier_row)
    frontier_dist2 <- (frontier$col - center_col)^2 + (frontier$row - center_row)^2
    ord <- order(frontier_dist2, frontier$row, frontier$col)

    pick_idx <- if (silhouette == "rounded") {
      ord[1]
    } else {
      rank_positions <- seq_along(ord)
      weights <- exp(-rank_positions / 4)
      ord[sample.int(length(ord), 1, prob = weights)]
    }

    next_col <- frontier$col[pick_idx]
    next_row <- frontier$row[pick_idx]
    selected <- rbind(selected, data.frame(col = next_col, row = next_row))
    assign(paste(next_col, next_row, sep = ","), TRUE, envir = selected_set)
  }

  rownames(selected) <- NULL
  selected$col <- as.integer(selected$col)
  selected$row <- as.integer(selected$row)

  if (nrow(selected) != n_cells) {
    rlang::abort("Internal error: silhouette mask row count mismatch.")
  }
  if (!hex_is_connected(selected)) {
    rlang::abort("Internal error: generated silhouette mask is not connected.")
  }

  selected
}
