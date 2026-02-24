# Graph utilities for hex-grid connectivity

#' Check whether hex cells form a connected component
#'
#' Uses breadth-first search over odd-q hex adjacency to verify that all cells
#' are reachable from the first cell.
#'
#' @param cells Data frame with integer-compatible `col` and `row` columns.
#'
#' @return TRUE when all cells are connected, FALSE otherwise.
#'
#' @keywords internal
#' @noRd
hex_is_connected <- function(cells) {
  if (!is.data.frame(cells) || !all(c("col", "row") %in% names(cells))) {
    rlang::abort("`cells` must be a data frame with `col` and `row` columns.")
  }

  n_cells <- nrow(cells)
  if (n_cells <= 1L) {
    return(TRUE)
  }

  cell_set <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(n_cells)) {
    key <- paste(cells$col[i], cells$row[i], sep = ",")
    assign(key, TRUE, envir = cell_set)
  }

  visited <- new.env(hash = TRUE, parent = emptyenv())
  queue_col <- integer(n_cells)
  queue_row <- integer(n_cells)
  head <- 1L
  tail <- 1L

  queue_col[1] <- as.integer(cells$col[1])
  queue_row[1] <- as.integer(cells$row[1])
  start_key <- paste(queue_col[1], queue_row[1], sep = ",")
  assign(start_key, TRUE, envir = visited)
  visited_count <- 1L

  while (head <= tail) {
    current_col <- queue_col[head]
    current_row <- queue_row[head]
    head <- head + 1L

    offsets <- hex_neighbors(current_col, current_row)
    for (i in seq_len(nrow(offsets))) {
      n_col <- current_col + offsets$dcol[i]
      n_row <- current_row + offsets$drow[i]
      n_key <- paste(n_col, n_row, sep = ",")

      if (exists(n_key, envir = cell_set, inherits = FALSE) &&
          !exists(n_key, envir = visited, inherits = FALSE)) {
        assign(n_key, TRUE, envir = visited)
        visited_count <- visited_count + 1L
        tail <- tail + 1L
        queue_col[tail] <- n_col
        queue_row[tail] <- n_row
      }
    }
  }

  visited_count == n_cells
}
