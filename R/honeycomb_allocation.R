# Region-growing allocation for honeycomb layouts

#' Allocate cells to categories using region-growing (Algorithm C)
#'
#' Sequential largest-first region-growing that guarantees contiguity by
#' construction. Each category grows from a seed cell outward, ensuring
#' all cells of a category form a single connected component.
#'
#' @param grid A data.frame with columns `col` and `row` representing available
#'   cells (0-based integer coordinates).
#' @param targets Named integer vector of target cell counts per category.
#'   Names are category labels. Must sum to nrow(grid).
#' @param global_center Numeric vector of length 2 (x, y) representing the

#'   global center for distance tie-breaking. If NULL, computed as centroid
#'   of grid cell coordinates.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{col}{Integer column index (0-based).}
#'     \item{row}{Integer row index (0-based).}
#'     \item{category}{Character category label assigned to this cell.}
#'   }
#'
#' @details
#' Algorithm C (Sequential Largest-First Region-Growing):
#'
#' 1. Sort categories by target count (descending); tie-break by label (ascending).
#' 2. Maintain set of free (unassigned) cells.
#' 3. For each category:
#'    a. Choose seed = free cell closest to centroid of free cells
#'       (tie-break by row ascending, then col ascending).
#'    b. Initialize region = {seed}.
#'    c. Maintain priority queue of frontier candidates. Priority (ascending):
#'       - Graph distance to seed
#'       - Euclidean distance to global center
#'       - Row, then col (stable tie-break)
#'    d. While |region| < target:
#'       - Pop next cell from frontier
#'       - If cell is free, add to region and push its free neighbors
#'       - If frontier empties before reaching target, error
#'    e. Mark region cells as assigned; remove from free set.
#'
#' @keywords internal
#' @noRd
allocate_regions <- function(grid, targets, global_center = NULL) {
  # Input validation
  if (!is.data.frame(grid) || !all(c("col", "row") %in% names(grid))) {
    rlang::abort("`grid` must be a data.frame with columns `col` and `row`.")
  }

  if (!is.integer(targets) || is.null(names(targets))) {
    rlang::abort("`targets` must be a named integer vector.")
  }

  if (sum(targets) != nrow(grid)) {
    rlang::abort(
      paste0(
        "Sum of targets (", sum(targets), ") must equal number of grid cells (",
        nrow(grid), ")."
      )
    )
  }

  if (any(targets < 0)) {
    rlang::abort("All target counts must be non-negative.")
  }

  n_cells <- nrow(grid)
  if (n_cells == 0) {
    return(data.frame(col = integer(0), row = integer(0), category = character(0)))
  }

  # Compute global center if not provided
  if (is.null(global_center)) {
    global_center <- c(mean(grid$col), mean(grid$row))
  }

  # Sort categories: descending by target, ascending by label for tie-break
  category_order <- names(targets)[order(-targets, names(targets))]

  # Initialize free cells as a set (using row indices for efficiency)
  # Create a lookup: (col, row) -> row index in grid
  grid$idx <- seq_len(nrow(grid))
  free_idx <- grid$idx  # All cells start as free

  # Result: category assignment for each cell
  result <- data.frame(
    col = grid$col,
    row = grid$row,
    category = NA_character_,
    stringsAsFactors = FALSE
  )

  # Process each category
  for (cat in category_order) {
    target_count <- targets[cat]

    # Skip categories with 0 target
    if (target_count == 0) {
      next
    }

    # Get current free cells
    free_cells <- grid[free_idx, , drop = FALSE]

    if (nrow(free_cells) < target_count) {
      rlang::abort(
        paste0(
          "Not enough free cells for category '", cat, "': need ", target_count,
          " but only ", nrow(free_cells), " available."
        )
      )
    }

    # Find seed: free cell closest to centroid of free cells
    free_centroid <- c(mean(free_cells$col), mean(free_cells$row))

    # Compute distances to centroid
    free_cells$dist_to_centroid <- sqrt(
      (free_cells$col - free_centroid[1])^2 +
      (free_cells$row - free_centroid[2])^2
    )

    # Sort by distance, then row, then col for tie-break
    free_cells <- free_cells[order(free_cells$dist_to_centroid,
                                    free_cells$row,
                                    free_cells$col), ]

    seed_idx <- free_cells$idx[1]
    seed_col <- free_cells$col[1]
    seed_row <- free_cells$row[1]

    # Initialize region with seed
    region_idx <- seed_idx
    assigned_set <- new.env(hash = TRUE, parent = emptyenv())
    assign(paste(seed_col, seed_row, sep = ","), TRUE, envir = assigned_set)

    # Initialize frontier with neighbors of seed
    # Frontier is a list of candidate cells with priority info
    frontier <- initialize_frontier(
      seed_col, seed_row,
      global_center, free_idx, grid, assigned_set
    )

    # Grow region until target reached
    while (length(region_idx) < target_count) {
      if (length(frontier) == 0) {
        rlang::abort(
          paste0(
            "Frontier exhausted for category '", cat, "': allocated ",
            length(region_idx), " of ", target_count, " cells. ",
            "Consider increasing grid size or adjusting category proportions."
          )
        )
      }

      # Pop best candidate from frontier
      best <- frontier[[1]]
      frontier <- frontier[-1]

      cell_key <- paste(best$col, best$row, sep = ",")

      # Skip if already assigned (could have been added multiple times)
      if (exists(cell_key, envir = assigned_set)) {
        next
      }

      # Check if cell is still free
      cell_idx <- grid$idx[grid$col == best$col & grid$row == best$row]
      if (length(cell_idx) == 0 || !(cell_idx %in% free_idx)) {
        next
      }

      # Add to region
      region_idx <- c(region_idx, cell_idx)
      assign(cell_key, TRUE, envir = assigned_set)

      # Add neighbors to frontier (using parent's graph_dist)
      new_candidates <- get_frontier_candidates(
        best$col, best$row, best$graph_dist,
        global_center, free_idx, grid, assigned_set
      )

      # Merge and re-sort frontier
      frontier <- merge_frontier(frontier, new_candidates)
    }

    # Mark region as assigned
    result$category[region_idx] <- cat

    # Remove from free set
    free_idx <- setdiff(free_idx, region_idx)
  }

  # Return result without the idx column
  result[, c("col", "row", "category")]
}


#' Initialize frontier with neighbors of seed cell
#'
#' @param seed_col Integer. Seed cell column.
#' @param seed_row Integer. Seed cell row.
#' @param global_center Numeric vector of length 2.
#' @param free_idx Integer vector of free cell indices.
#' @param grid Data.frame with col, row, idx columns.
#' @param assigned_set Environment used as hash set of assigned cells.
#'
#' @return List of frontier candidates, sorted by priority.
#'
#' @keywords internal
#' @noRd
initialize_frontier <- function(seed_col, seed_row,
                                 global_center, free_idx, grid, assigned_set) {
  # Seed has graph_dist = 0, so neighbors have graph_dist = 1
  get_frontier_candidates(
    seed_col, seed_row, 0L,
    global_center, free_idx, grid, assigned_set
  )
}


#' Get frontier candidates from a cell's neighbors
#'
#' @param cell_col Integer. Cell column.
#' @param cell_row Integer. Cell row.
#' @param parent_graph_dist Integer. Graph distance of the parent cell from seed.
#' @param global_center Numeric vector of length 2.
#' @param free_idx Integer vector of free cell indices.
#' @param grid Data.frame with col, row, idx columns.
#' @param assigned_set Environment used as hash set of assigned cells.
#'
#' @return List of frontier candidates with priority info.
#'
#' @keywords internal
#' @noRd
get_frontier_candidates <- function(cell_col, cell_row, parent_graph_dist,
                                     global_center, free_idx, grid, assigned_set) {
  # Get neighbor offsets using hex_neighbors
  offsets <- hex_neighbors(cell_col, cell_row)

  candidates <- list()

  # Neighbors are one step further from seed than parent

  neighbor_graph_dist <- parent_graph_dist + 1L

  for (i in seq_len(nrow(offsets))) {
    n_col <- cell_col + offsets$dcol[i]
    n_row <- cell_row + offsets$drow[i]

    # Check if neighbor exists in grid and is free
    cell_key <- paste(n_col, n_row, sep = ",")

    # Skip if already assigned
    if (exists(cell_key, envir = assigned_set)) {
      next
    }

    # Find cell in grid
    match_idx <- which(grid$col == n_col & grid$row == n_row)
    if (length(match_idx) == 0) {
      next  # Cell not in grid
    }

    cell_idx <- grid$idx[match_idx]
    if (!(cell_idx %in% free_idx)) {
      next  # Cell not free
    }

    # Distance to global center (Euclidean for tie-breaking)
    dist_to_center <- sqrt(
      (n_col - global_center[1])^2 +
      (n_row - global_center[2])^2
    )

    candidates[[length(candidates) + 1]] <- list(
      col = n_col,
      row = n_row,
      graph_dist = neighbor_graph_dist,
      dist_to_center = dist_to_center
    )
  }

  # Sort candidates by priority
  if (length(candidates) > 0) {
    sort_frontier(candidates)
  } else {
    list()
  }
}


#' Sort frontier candidates by priority
#'
#' Priority (ascending): graph_dist, dist_to_center, row, col
#'
#' @param candidates List of candidate lists with col, row, graph_dist, dist_to_center.
#'
#' @return Sorted list of candidates.
#'
#' @keywords internal
#' @noRd
sort_frontier <- function(candidates) {
  if (length(candidates) == 0) {
    return(list())
  }

  # Extract priority values
  # graph_dist is integer (BFS step count), others are numeric/integer
  graph_dists <- vapply(candidates, function(x) x$graph_dist, integer(1))
  center_dists <- vapply(candidates, function(x) x$dist_to_center, numeric(1))
  rows <- vapply(candidates, function(x) x$row, integer(1))
  cols <- vapply(candidates, function(x) x$col, integer(1))

  # Sort order: graph_dist, dist_to_center, row, col (all ascending)
  ord <- order(graph_dists, center_dists, rows, cols)

  candidates[ord]
}


#' Merge new candidates into existing frontier
#'
#' @param frontier Existing sorted frontier list.
#' @param new_candidates New candidates to add.
#'
#' @return Merged and sorted frontier.
#'
#' @keywords internal
#' @noRd
merge_frontier <- function(frontier, new_candidates) {
  if (length(new_candidates) == 0) {
    return(frontier)
  }

  # Simple approach: concatenate and re-sort
  # For large frontiers, a proper priority queue would be more efficient
  combined <- c(frontier, new_candidates)
  sort_frontier(combined)
}


#' Validate contiguity of allocated regions using flood fill
#'
#' Checks that each category's cells form exactly one connected component
#' using BFS flood fill over hex adjacency.
#'
#' @param allocation A data.frame with columns `col`, `row`, `category` as
#'   returned by `allocate_regions()`.
#'
#' @return TRUE if all categories are contiguous, otherwise throws an error
#'   with details about which category is not contiguous.
#'
#' @details
#' For each category:
#' 1. Get all cells belonging to that category.
#' 2. Start BFS from the first cell.
#' 3. Visit all reachable cells via hex adjacency.
#' 4. If visited count != total count, category is not contiguous.
#'
#' @keywords internal
#' @noRd
validate_contiguity <- function(allocation) {
  if (!is.data.frame(allocation) ||
      !all(c("col", "row", "category") %in% names(allocation))) {
    rlang::abort(
      "`allocation` must be a data.frame with columns `col`, `row`, `category`."
    )
  }

  if (nrow(allocation) == 0) {
    return(TRUE)
  }

  categories <- unique(allocation$category)
  categories <- categories[!is.na(categories)]

  for (cat in categories) {
    cat_cells <- allocation[allocation$category == cat, , drop = FALSE]
    n_cells <- nrow(cat_cells)

    if (n_cells == 0) {
      next
    }

    # Create a set of cells for O(1) lookup
    cell_set <- new.env(hash = TRUE, parent = emptyenv())
    for (i in seq_len(n_cells)) {
      key <- paste(cat_cells$col[i], cat_cells$row[i], sep = ",")
      assign(key, TRUE, envir = cell_set)
    }

    # BFS from first cell
    visited <- new.env(hash = TRUE, parent = emptyenv())
    queue <- list(list(col = cat_cells$col[1], row = cat_cells$row[1]))
    start_key <- paste(cat_cells$col[1], cat_cells$row[1], sep = ",")
    assign(start_key, TRUE, envir = visited)
    visited_count <- 1L

    while (length(queue) > 0) {
      current <- queue[[1]]
      queue <- queue[-1]

      # Get neighbors
      offsets <- hex_neighbors(current$col, current$row)

      for (i in seq_len(nrow(offsets))) {
        n_col <- current$col + offsets$dcol[i]
        n_row <- current$row + offsets$drow[i]
        n_key <- paste(n_col, n_row, sep = ",")

        # Check if neighbor is in category and not visited
        if (exists(n_key, envir = cell_set) && !exists(n_key, envir = visited)) {
          assign(n_key, TRUE, envir = visited)
          visited_count <- visited_count + 1L
          queue[[length(queue) + 1]] <- list(col = n_col, row = n_row)
        }
      }
    }

    if (visited_count != n_cells) {
      rlang::abort(
        paste0(
          "Category '", cat, "' is not contiguous: found ", visited_count,
          " connected cells but expected ", n_cells, "."
        )
      )
    }
  }

  TRUE
}


#' Count connected components for a category
#'
#' Returns the number of connected components for cells of a given category.
#' Useful for testing.
#'
#' @param allocation A data.frame with columns `col`, `row`, `category`.
#' @param category Character. The category to check.
#'
#' @return Integer count of connected components.
#'
#' @keywords internal
#' @noRd
count_components <- function(allocation, category) {
  cat_cells <- allocation[allocation$category == category, , drop = FALSE]
  n_cells <- nrow(cat_cells)

  if (n_cells == 0) {
    return(0L)
  }

  # Create a set of cells for O(1) lookup
  cell_set <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(n_cells)) {
    key <- paste(cat_cells$col[i], cat_cells$row[i], sep = ",")
    assign(key, TRUE, envir = cell_set)
  }

  # Track visited cells
  visited <- new.env(hash = TRUE, parent = emptyenv())
  n_components <- 0L

  for (i in seq_len(n_cells)) {
    start_key <- paste(cat_cells$col[i], cat_cells$row[i], sep = ",")

    if (exists(start_key, envir = visited)) {
      next
    }

    # New component found - BFS
    n_components <- n_components + 1L
    queue <- list(list(col = cat_cells$col[i], row = cat_cells$row[i]))
    assign(start_key, TRUE, envir = visited)

    while (length(queue) > 0) {
      current <- queue[[1]]
      queue <- queue[-1]

      offsets <- hex_neighbors(current$col, current$row)

      for (j in seq_len(nrow(offsets))) {
        n_col <- current$col + offsets$dcol[j]
        n_row <- current$row + offsets$drow[j]
        n_key <- paste(n_col, n_row, sep = ",")

        if (exists(n_key, envir = cell_set) && !exists(n_key, envir = visited)) {
          assign(n_key, TRUE, envir = visited)
          queue[[length(queue) + 1]] <- list(col = n_col, row = n_row)
        }
      }
    }
  }

  n_components
}
