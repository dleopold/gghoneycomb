#' Hex region compactness utilities
#'
#' Internal functions for measuring perimeter and compactness of regions
#' on a hexagonal grid. Used for scoring allocator quality.
#'
#' @name hex-compactness
#' @keywords internal
NULL

#' Count the perimeter of a hex region
#'
#' Counts boundary edges for all cells belonging to a given category.
#' Each cell has 6 edges; an edge is a boundary edge if the neighboring
#' cell is not in the same region. Perimeter = sum(6 - neighbors_in_region)
#' across all cells in the region.
#'
#' @param allocation A data.frame with columns `col`, `row`, `category`.
#' @param category A length-1 character string identifying the region.
#'
#' @return Integer. The perimeter (number of boundary edges). Returns 0
#'   if no cells belong to the given category.
#'
#' @keywords internal
#' @noRd
hex_region_perimeter <- function(allocation, category) {
  # --- input validation ---
  if (!is.data.frame(allocation)) {
    stop("`allocation` must be a data.frame", call. = FALSE)
  }
  required <- c("col", "row", "category")
  missing_cols <- setdiff(required, names(allocation))
  if (length(missing_cols) > 0L) {
    stop(
      "`allocation` must have columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.character(category) || length(category) != 1L) {
    stop("`category` must be a length-1 character string", call. = FALSE)
  }


  # --- extract region cells ---
  region <- allocation[allocation$category == category, , drop = FALSE]
  n <- nrow(region)
  if (n == 0L) return(0L)

  # --- build membership set using environment for O(1) lookup ---
  member_env <- new.env(hash = TRUE, parent = emptyenv(), size = max(n, 1L))
  for (i in seq_len(n)) {
    key <- paste0(region$col[i], ",", region$row[i])
    assign(key, TRUE, envir = member_env)
  }

  # --- count boundary edges ---
  perimeter <- 0L
  for (i in seq_len(n)) {
    col_i <- region$col[i]
    row_i <- region$row[i]
    offsets <- hex_neighbors(col_i, row_i)
    neighbors_in <- 0L
    for (j in seq_len(6L)) {
      nb_key <- paste0(col_i + offsets$dcol[j], ",", row_i + offsets$drow[j])
      if (exists(nb_key, envir = member_env, inherits = FALSE)) {
        neighbors_in <- neighbors_in + 1L
      }
    }
    perimeter <- perimeter + (6L - neighbors_in)
  }

  perimeter
}

#' Isoperimetric lower bound for hex polyhex perimeter
#'
#' Returns the theoretical minimum perimeter for a connected region of
#' `n` hexagons on a hex grid (OEIS A135711).
#'
#' @param n Integer. Number of cells in the region.
#'
#' @return Integer. The lower bound perimeter. Returns 0 for `n < 1`.
#'
#' @keywords internal
#' @noRd
hex_isoperimetric_lower_bound <- function(n) {
  n <- as.integer(n)
  if (n < 1L) return(0L)
  as.integer(ceiling(2L * sqrt(12 * n - 3)))
}

#' Compactness ratio of a hex region
#'
#' Ratio of actual perimeter to the isoperimetric lower bound.
#' A ratio of 1.0 means the region is maximally compact (approaching
#' a hexagonal shape). Higher values indicate more irregular shapes.
#'
#' @param allocation A data.frame with columns `col`, `row`, `category`.
#' @param category A length-1 character string identifying the region.
#'
#' @return Numeric. The compactness ratio (perimeter / lower_bound).
#'   Returns 0 if the region is empty (no cells for that category).
#'
#' @keywords internal
#' @noRd
hex_region_compactness_ratio <- function(allocation, category) {
  # validation delegated to hex_region_perimeter
  perimeter <- hex_region_perimeter(allocation, category)
  if (perimeter == 0L) return(0)

  n <- sum(allocation$category == category)
  lower <- hex_isoperimetric_lower_bound(n)
  if (lower == 0L) return(0)

  perimeter / lower
}
