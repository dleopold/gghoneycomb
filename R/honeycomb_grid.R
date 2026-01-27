# Grid sizing helpers for honeycomb layouts

#' Choose optimal grid dimensions for a given number of cells
#'
#' Finds the (width, height) pair that minimizes (abs(w-h), w*h, w) subject to
#' constraints, ensuring w*h >= n_cells. This prioritizes square grids over
#' minimal area, which produces better visualizations.
#'
#' @param n_cells Integer. Number of cells to fit in the grid.
#' @param min_width Integer. Minimum grid width (default 1).
#' @param max_width Integer. Maximum grid width (default NULL, computed as
#'   ceiling(sqrt(n_cells)) * 2 to allow reasonably square grids).
#' @param min_height Integer. Minimum grid height (default 1).
#' @param max_height Integer. Maximum grid height (default NULL, computed as
#'   ceiling(sqrt(n_cells)) * 2 to allow reasonably square grids).
#'
#' @return A named list with elements `width` and `height`.
#'
#' @details
#' Algorithm B from the plan:
#' 1. Enumerate candidate widths w in \code{min_width..max_width}.
#' 2. For each w, compute h = ceiling(n_cells / w).
#' 3. Keep (w, h) if h respects \code{min_height..max_height}.
#' 4. Pick the feasible (w, h) minimizing (abs(w-h), w*h, w) - squareness first.
#' 5. If none feasible: error with constraints and n_cells in message.
#'
#' @examples
#' # Perfect square
#' choose_grid_size(100)
#' # => list(width = 10, height = 10)
#'
#' # Non-perfect square rounds up

#' choose_grid_size(97)
#' # => list(width = 10, height = 10)
#'
#' @keywords internal
#' @noRd
choose_grid_size <- function(n_cells,
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


  # Set defaults - use sqrt-based bounds for reasonably square grids
  # This ensures n_cells=97 returns (10,10) not (1,97)
  sqrt_n <- ceiling(sqrt(n_cells))
  if (is.null(max_width)) max_width <- sqrt_n * 2L
  if (is.null(max_height)) max_height <- sqrt_n * 2L

  # Validate constraints
  if (!is.numeric(min_width) || length(min_width) != 1 || is.na(min_width) ||
      min_width < 1 || min_width != floor(min_width)) {
    rlang::abort("`min_width` must be a positive integer.")
  }
  if (!is.numeric(max_width) || length(max_width) != 1 || is.na(max_width) ||
      max_width < 1 || max_width != floor(max_width)) {
    rlang::abort("`max_width` must be a positive integer.")
  }
  if (!is.numeric(min_height) || length(min_height) != 1 || is.na(min_height) ||
      min_height < 1 || min_height != floor(min_height)) {
    rlang::abort("`min_height` must be a positive integer.")
  }
  if (!is.numeric(max_height) || length(max_height) != 1 || is.na(max_height) ||
      max_height < 1 || max_height != floor(max_height)) {
    rlang::abort("`max_height` must be a positive integer.")
  }

  min_width <- as.integer(min_width)
  max_width <- as.integer(max_width)
  min_height <- as.integer(min_height)
  max_height <- as.integer(max_height)

  if (min_width > max_width) {
    rlang::abort("`min_width` cannot exceed `max_width`.")
  }
  if (min_height > max_height) {
    rlang::abort("`min_height` cannot exceed `max_height`.")
  }

  # Check if constraints can possibly fit n_cells
  max_capacity <- max_width * max_height
  if (max_capacity < n_cells) {
    rlang::abort(
      paste0(
        "Cannot fit ", n_cells, " cells within constraints: ",
        "max_width=", max_width, ", max_height=", max_height,
        " (max capacity=", max_capacity, ")."
      )
    )
  }

  # Enumerate candidates
  best <- NULL
  best_score <- NULL

  for (w in seq(min_width, max_width)) {
    h <- ceiling(n_cells / w)

    # Check height constraint
    if (h < min_height || h > max_height) {
      next
    }

    # Compute score tuple: (abs(w-h), w*h, w)
    # We want to minimize lexicographically - squareness first, then area
    area <- w * h
    diff <- abs(w - h)
    score <- c(diff, area, w)

    if (is.null(best_score) ||
        score[1] < best_score[1] ||
        (score[1] == best_score[1] && score[2] < best_score[2]) ||
        (score[1] == best_score[1] && score[2] == best_score[2] && score[3] < best_score[3])) {
      best <- list(width = w, height = h)
      best_score <- score
    }
  }

  if (is.null(best)) {
    rlang::abort(
      paste0(
        "No feasible grid size found for n_cells=", n_cells,
        " with constraints: min_width=", min_width, ", max_width=", max_width,
        ", min_height=", min_height, ", max_height=", max_height, "."
      )
    )
  }

  best
}
