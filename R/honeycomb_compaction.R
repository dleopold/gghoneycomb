# Compaction mask generation for irregular honeycomb boundaries

#' Generate a compaction mask via seeded erosion (Algorithm D)
#'
#' Produces a connected set of exactly `n_cells` available cells by starting
#' with a larger rectangular grid and eroding perimeter cells while maintaining
#' connectivity.
#'
#' @param n_cells Integer. Target number of cells in the final mask.
#' @param compaction Numeric. Compaction factor in (0, 1]. At 1, returns a
#'   rectangular grid. Lower values produce more irregular boundaries.
#' @param seed Integer or NULL. Random seed for reproducibility. If NULL,
#'   uses current RNG state.
#' @param min_width Integer. Minimum grid width (default 1).
#' @param max_width Integer. Maximum grid width (default NULL).
#' @param min_height Integer. Minimum grid height (default 1).
#' @param max_height Integer. Maximum grid height (default NULL).
#'
#' @return A data.frame with columns `col` and `row` representing the
#'   available cells in the mask (0-based integer coordinates).
#'
#' @details
#' Algorithm D (Seeded Erosion):
#' 1. Compute n0 = ceiling(n_cells / compaction).
#' 2. Build a rectangular grid of n0 cells (via grid sizing), mark all as available.
#' 3. While available count > n_cells:
#'    - Find perimeter cells (cells with < 6 available neighbors).
#'    - Sample a perimeter cell to remove using seeded RNG, with higher
#'      probability for cells farther from center.
#'    - Tentatively remove it.
#'    - If remaining available cells are still connected (single flood-fill
#'      component), keep; else revert.
#'    - If no perimeter cell can be removed while preserving connectivity:
#'      error (suggest increasing compaction).
#' 4. Return the remaining set as the mask for allocation.
#'
#' @keywords internal
#' @noRd
generate_compaction_mask <- function(n_cells,
                                     compaction,
                                     seed = NULL,
                                     min_width = 1L,
                                     max_width = NULL,
                                     min_height = 1L,
                                     max_height = NULL) {
  # Input validation

if (!is.numeric(n_cells) || length(n_cells) != 1 || is.na(n_cells) ||
      n_cells < 1 || n_cells != floor(n_cells)) {
    rlang::abort("`n_cells` must be a positive integer.")
  }
  n_cells <- as.integer(n_cells)

  if (!is.numeric(compaction) || length(compaction) != 1 || is.na(compaction) ||
      compaction <= 0 || compaction > 1) {
    rlang::abort("`compaction` must be a number in (0, 1].")
  }

  # Set seed if provided
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
      rlang::abort("`seed` must be a single integer or NULL.")
    }
    set.seed(as.integer(seed))
  }

  # For compaction = 1, just return a rectangular grid
  if (compaction == 1) {
    grid_size <- choose_grid_size(n_cells, min_width, max_width,
                                   min_height, max_height)
    grid <- hex_grid_centers(grid_size$width, grid_size$height, r = 1)
    # Return exactly n_cells (grid may have more due to ceiling)
    return(grid[seq_len(n_cells), c("col", "row"), drop = FALSE])
  }

  # Compute initial grid size: n0 = ceiling(n_cells / compaction)
  n0 <- ceiling(n_cells / compaction)

  # Build initial rectangular grid
  grid_size <- choose_grid_size(n0, min_width, max_width, min_height, max_height)
  initial_grid <- hex_grid_centers(grid_size$width, grid_size$height, r = 1)

  # Initialize available cells (all cells start as available)
  available <- initial_grid[, c("col", "row"), drop = FALSE]
  available$available <- TRUE

  # Compute center of grid for distance weighting
  center_col <- mean(available$col)
  center_row <- mean(available$row)

  # Erode until we reach n_cells
  n_available <- nrow(available)
  cells_to_remove <- n_available - n_cells

  if (cells_to_remove < 0) {
    rlang::abort(
      paste0(
        "Initial grid has fewer cells (", n_available, ") than target (",
        n_cells, "). This should not happen with compaction < 1."
      )
    )
  }

  removed_count <- 0
  max_attempts_per_cell <- n_available * 2  # Safety limit

  while (removed_count < cells_to_remove) {
    # Get current available cells
    current_available <- available[available$available, , drop = FALSE]
    n_current <- nrow(current_available)

    if (n_current <= n_cells) {
      break
    }

    # Find perimeter cells (cells with < 6 available neighbors)
    perimeter_idx <- find_perimeter_cells(current_available)

    if (length(perimeter_idx) == 0) {
      rlang::abort(
        paste0(
          "No perimeter cells found with ", n_current, " cells remaining. ",
          "This should not happen for a connected grid."
        )
      )
    }

    # Compute distances from center for weighting
    perimeter_cells <- current_available[perimeter_idx, , drop = FALSE]
    distances <- sqrt(
      (perimeter_cells$col - center_col)^2 +
      (perimeter_cells$row - center_row)^2
    )

    # Weight by distance (farther = higher probability)
    # Add small constant to avoid zero weights
    weights <- distances + 0.1

    # Try to remove cells, starting with weighted random selection
    removed_this_round <- FALSE
    attempts <- 0
    tried_indices <- integer(0)

    while (!removed_this_round && length(tried_indices) < length(perimeter_idx)) {
      attempts <- attempts + 1

      if (attempts > max_attempts_per_cell) {
        rlang::abort(
          paste0(
            "Cannot remove more cells while preserving connectivity. ",
            "Removed ", removed_count, " of ", cells_to_remove, " needed. ",
            "Try increasing `compaction` (current: ", compaction, ")."
          )
        )
      }

      # Sample from remaining untried perimeter cells
      remaining_idx <- setdiff(seq_along(perimeter_idx), tried_indices)
      if (length(remaining_idx) == 0) {
        break
      }

      remaining_weights <- weights[remaining_idx]
      if (length(remaining_idx) == 1) {
        sample_idx <- remaining_idx[1]
      } else {
        sample_idx <- remaining_idx[sample.int(
          length(remaining_idx), 1,
          prob = remaining_weights / sum(remaining_weights)
        )]
      }
      tried_indices <- c(tried_indices, sample_idx)

      # Get the cell to try removing
      cell_to_remove <- perimeter_cells[sample_idx, , drop = FALSE]
      cell_row_idx <- which(
        available$col == cell_to_remove$col &
        available$row == cell_to_remove$row
      )

      # Tentatively remove
      available$available[cell_row_idx] <- FALSE

      # Check connectivity
      remaining <- available[available$available, c("col", "row"), drop = FALSE]
      if (is_connected(remaining)) {
        # Keep the removal
        removed_count <- removed_count + 1
        removed_this_round <- TRUE
      } else {
        # Revert
        available$available[cell_row_idx] <- TRUE
      }
    }

    if (!removed_this_round) {
      rlang::abort(
        paste0(
          "Cannot remove any perimeter cell while preserving connectivity. ",
          "Removed ", removed_count, " of ", cells_to_remove, " needed. ",
          "Try increasing `compaction` (current: ", compaction, ")."
        )
      )
    }
  }

  # Return the final mask
  result <- available[available$available, c("col", "row"), drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Find perimeter cells in a set of available cells
#'
#' Perimeter cells are those with fewer than 6 available neighbors.
#'
#' @param cells Data.frame with col and row columns.
#'
#' @return Integer vector of row indices in cells that are perimeter cells.
#'
#' @keywords internal
#' @noRd
find_perimeter_cells <- function(cells) {
  if (nrow(cells) == 0) {
    return(integer(0))
  }

  # Create lookup set for O(1) membership testing
  cell_set <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(nrow(cells))) {
    key <- paste(cells$col[i], cells$row[i], sep = ",")
    assign(key, TRUE, envir = cell_set)
  }

  perimeter_idx <- integer(0)

  for (i in seq_len(nrow(cells))) {
    col <- cells$col[i]
    row <- cells$row[i]

    # Count available neighbors
    offsets <- hex_neighbors(col, row)
    n_neighbors <- 0L

    for (j in seq_len(nrow(offsets))) {
      n_col <- col + offsets$dcol[j]
      n_row <- row + offsets$drow[j]
      n_key <- paste(n_col, n_row, sep = ",")

      if (exists(n_key, envir = cell_set)) {
        n_neighbors <- n_neighbors + 1L
      }
    }

    # Perimeter if < 6 neighbors
    if (n_neighbors < 6L) {
      perimeter_idx <- c(perimeter_idx, i)
    }
  }

  perimeter_idx
}


#' Check if a set of cells is connected
#'
#' Uses BFS flood fill to verify all cells are reachable from the first cell.
#'
#' @param cells Data.frame with col and row columns.
#'
#' @return TRUE if all cells form a single connected component, FALSE otherwise.
#'
#' @keywords internal
#' @noRd
is_connected <- function(cells) {
  n_cells <- nrow(cells)

  if (n_cells <= 1) {
    return(TRUE)
  }

  # Create lookup set
  cell_set <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(n_cells)) {
    key <- paste(cells$col[i], cells$row[i], sep = ",")
    assign(key, TRUE, envir = cell_set)
  }

  # BFS from first cell
  visited <- new.env(hash = TRUE, parent = emptyenv())
  queue <- list(list(col = cells$col[1], row = cells$row[1]))
  start_key <- paste(cells$col[1], cells$row[1], sep = ",")
  assign(start_key, TRUE, envir = visited)
  visited_count <- 1L

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    offsets <- hex_neighbors(current$col, current$row)

    for (i in seq_len(nrow(offsets))) {
      n_col <- current$col + offsets$dcol[i]
      n_row <- current$row + offsets$drow[i]
      n_key <- paste(n_col, n_row, sep = ",")

      if (exists(n_key, envir = cell_set) && !exists(n_key, envir = visited)) {
        assign(n_key, TRUE, envir = visited)
        visited_count <- visited_count + 1L
        queue[[length(queue) + 1]] <- list(col = n_col, row = n_row)
      }
    }
  }

  visited_count == n_cells
}
