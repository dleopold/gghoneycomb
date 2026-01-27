#' Compute honeycomb layout statistics
#'
#' `stat_honeycomb()` computes the layout for a honeycomb waffle chart,
#' transforming categorical data with associated weights into hexagonal cell
#' positions. This stat is typically used with [geom_honeycomb()].
#'
#' @section Data Contract:
#' The stat expects data with:
#' \itemize{
#'   \item A categorical variable mapped to `fill` (the category for each cell)
#'   \item A numeric variable mapped to `weight` (counts or proportions)
#' }
#'
#' The stat **ignores** any `x` and `y` mappings in the input data because
#' the honeycomb layout is computed internally based on the weights.
#'
#' @section Computed Variables:
#' The stat computes the following variables for use by the geom:
#' \describe{
#'   \item{x, y}{Vertex coordinates for each hexagon}
#'   \item{group}{Unique cell identifier (one per hexagon)}
#'   \item{cell_id}{Same as group, explicit cell identifier}
#'   \item{grid_col, grid_row}{Integer grid coordinates (odd-q offset indices)}
#'   \item{pct}{Normalized proportion for the category}
#'   \item{tooltip_label}{Preformatted string "<category>: <pct>%"}
#' }
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   **Required**: `fill` (category) and `weight` (numeric values).
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use to display the data.
#' @param position Position adjustment, either as a string or the result of
#'   a position adjustment function. Not typically used for honeycomb plots.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @param values_are How to interpret the `weight` aesthetic values:
#'   \describe{
#'     \item{`"auto"`}{(Default) If weights sum to 1 (within tolerance 1e-6),
#'       treat as proportions; otherwise treat as counts.
#'     }
#'     \item{`"counts"`}{Treat weights as raw counts to be normalized.}
#'     \item{`"proportions"`}{Treat weights as proportions (must sum to 1).}
#'   }
#' @param n_cells Number of hexagonal cells to use. If `NULL` (default),
#'   automatically chosen based on the number of categories and minimum
#'   proportion. Maximum allowed is 2500.
#' @param compaction Controls the regularity of the honeycomb boundary:
#'   \describe{
#'     \item{`1`}{(Default) Rectangular bounding grid.}
#'     \item{`< 1`}{Increasingly irregular silhouette via boundary erosion.}
#'   }
#'   Must be in the range (0, 1].
#' @param min_width,max_width Constraints on grid width (number of columns).
#'   Default `NULL` means no constraint.
#' @param min_height,max_height Constraints on grid height (number of rows).
#'   Default `NULL` means no constraint.
#' @param seed Random seed for reproducible layouts when `compaction < 1`.
#'   If `NULL`, uses a random seed.
#' @param ... Other arguments passed to the stat.
#'
#' @return A ggplot2 layer that can be added to a plot.
#'
#' @seealso [geom_honeycomb()] for the geom that draws the hexagons.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Example with counts
#' df <- data.frame(
#'   category = c("A", "B", "C", "D"),
#'   count = c(40, 30, 20, 10)
#' )
#'
#' ggplot(df, aes(fill = category, weight = count)) +
#'   stat_honeycomb()
#'
#' # Example with proportions
#' df_prop <- data.frame(
#'   category = c("A", "B", "C", "D"),
#'   prop = c(0.4, 0.3, 0.2, 0.1)
#' )
#'
#' ggplot(df_prop, aes(fill = category, weight = prop)) +
#'   stat_honeycomb(values_are = "proportions")
#' }
#'
#' @export
stat_honeycomb <- function(mapping = NULL,
                           data = NULL,
                           geom = "honeycomb",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           values_are = c("auto", "counts", "proportions"),
                           n_cells = NULL,
                           compaction = 1,
                           min_width = NULL,
                           max_width = NULL,
                           min_height = NULL,
                           max_height = NULL,
                           seed = NULL,
                           ...) {
  values_are <- match.arg(values_are)

  # Validate compaction
  if (!is.numeric(compaction) || length(compaction) != 1 ||
      compaction <= 0 || compaction > 1) {
    rlang::abort(
      "`compaction` must be a single number in the range (0, 1]."
    )
  }

  # Validate n_cells if provided
  if (!is.null(n_cells)) {
    if (!is.numeric(n_cells) || length(n_cells) != 1 ||
        n_cells < 1 || n_cells != floor(n_cells)) {
      rlang::abort(
        "`n_cells` must be a positive integer."
      )
    }
    if (n_cells > 2500) {
      rlang::abort(
        paste0("`n_cells` must be at most 2500, got ", n_cells, ".")
      )
    }
  }

  ggplot2::layer(
    stat = StatHoneycomb,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      values_are = values_are,
      n_cells = n_cells,
      compaction = compaction,
      min_width = min_width,
      max_width = max_width,
      min_height = min_height,
      max_height = max_height,
      seed = seed,
      ...
    )
  )
}

#' @rdname stat_honeycomb
#' @format NULL
#' @usage NULL
#' @export
StatHoneycomb <- ggplot2::ggproto("StatHoneycomb", ggplot2::Stat,
  required_aes = c("fill", "weight"),

  # Note: x and y are intentionally NOT required; layout is computed internally
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(x),
    y = ggplot2::after_stat(y)
  ),

  setup_params = function(data, params) {
    # Validate that weight aesthetic is present
    if (!"weight" %in% names(data)) {
      rlang::abort(c(
        "The `weight` aesthetic is required for `stat_honeycomb()`.",
        "i" = "Map a numeric variable to `weight` representing counts or proportions.",
        "i" = "Example: aes(fill = category, weight = count)"
      ))
    }

    params
  },

  compute_panel = function(data, scales, values_are = "auto",
                           n_cells = NULL, compaction = 1,
                           min_width = NULL, max_width = NULL,
                           min_height = NULL, max_height = NULL,
                           seed = NULL, na.rm = FALSE) {
    # Validate weight is present
    if (!"weight" %in% names(data)) {
      rlang::abort(c(
        "The `weight` aesthetic is required for `stat_honeycomb()`.",
        "i" = "Map a numeric variable to `weight` representing counts or proportions.",
        "i" = "Example: aes(fill = category, weight = count)"
      ))
    }

    # Aggregate by fill category
    agg <- stats::aggregate(
      data$weight,
      by = list(fill = data$fill),
      FUN = sum,
      na.rm = na.rm
    )
    names(agg) <- c("fill", "weight")

    # Remove zero/negative weights
    agg <- agg[agg$weight > 0, , drop = FALSE]

    if (nrow(agg) == 0) {
      rlang::abort("No positive weights found in data.")
    }

    # Determine if values are counts or proportions
    weight_sum <- sum(agg$weight)
    is_proportions <- if (values_are == "auto") {
      abs(weight_sum - 1) <= 1e-6
    } else {
      values_are == "proportions"
    }

    # Normalize to proportions
    if (is_proportions) {
      if (abs(weight_sum - 1) > 1e-6) {
        rlang::abort(c(
          "When `values_are` is \"proportions\", weights must sum to 1.",
          "x" = paste0("Weights sum to ", weight_sum, ".")
        ))
      }
      proportions <- agg$weight
    } else {
      proportions <- agg$weight / weight_sum
    }

    # Store proportions and percentages
    agg$pct <- proportions * 100
    labels <- as.character(agg$fill)

    # Algorithm A: Choose n_cells when NULL
    if (is.null(n_cells)) {
      k <- nrow(agg)  # number of nonzero categories
      p_min <- min(proportions)

      n_base <- 100L
      n_by_k <- 10L * k
      n_by_min <- min(ceiling(1 / p_min), 500L)
      n_cells <- min(max(n_base, n_by_k, n_by_min), 500L)
      n_cells <- min(n_cells, 2500L)  # enforce hard limit
    }
    n_cells <- as.integer(n_cells)

    # Algorithm A0: Round proportions to integer cell counts
    targets <- as.integer(round_proportions(proportions, n_cells, labels))
    names(targets) <- labels

    # Set seed for reproducibility (affects compaction mask)
    if (!is.null(seed)) {
      set.seed(as.integer(seed))
    }

    # Set default values for width/height constraints
    min_width <- if (is.null(min_width)) 1L else min_width
    max_width <- max_width  # NULL is valid, handled by choose_grid_size
    min_height <- if (is.null(min_height)) 1L else min_height
    max_height <- max_height  # NULL is valid, handled by choose_grid_size

    # Algorithm D: Generate compaction mask (or rectangular grid if compaction=1)
    mask <- generate_compaction_mask(
      n_cells = n_cells,
      compaction = compaction,
      seed = NULL,  # seed already set above
      min_width = min_width,
      max_width = max_width,
      min_height = min_height,
      max_height = max_height
    )

    # Algorithm C: Allocate regions (contiguous by construction)
    allocation <- allocate_regions(
      grid = mask,
      targets = targets,
      global_center = NULL  # computed internally
    )

    # Algorithm B: Get grid dimensions for hex geometry
    # Use the actual mask dimensions for center computation
    grid_size <- choose_grid_size(
      n_cells = n_cells,
      min_width = min_width,
      max_width = max_width,
      min_height = min_height,
      max_height = max_height
    )

    # Algorithm E: Generate hex centers and vertices
    # Use radius = 1 for unit hexagons (can be scaled by coord_equal)
    r <- 1
    centers <- hex_grid_centers(grid_size$width, grid_size$height, r)

    # Merge allocation with centers to get (col, row, category, x, y)
    # allocation has: col, row, category
    # centers has: col, row, x, y
    cell_data <- merge(allocation, centers, by = c("col", "row"), all.x = TRUE)

    # Add cell_id (unique per cell)
    cell_data$cell_id <- seq_len(nrow(cell_data))

    # Add pct and tooltip_label from agg
    pct_lookup <- stats::setNames(agg$pct, as.character(agg$fill))
    cell_data$pct <- pct_lookup[as.character(cell_data$category)]
    cell_data$tooltip_label <- paste0(
      cell_data$category, ": ", round(cell_data$pct, 1), "%"
    )

    # Generate polygon vertices for each cell
    # Each cell becomes 6 rows (one per vertex)
    vertex_list <- lapply(seq_len(nrow(cell_data)), function(i) {
      verts <- hex_vertices(cell_data$x[i], cell_data$y[i], r)
      verts$group <- cell_data$cell_id[i]
      verts$fill <- cell_data$category[i]
      verts$cell_id <- cell_data$cell_id[i]
       verts$grid_col <- cell_data$col[i]
       verts$grid_row <- cell_data$row[i]
      verts$pct <- cell_data$pct[i]
      verts$tooltip_label <- cell_data$tooltip_label[i]
      verts
    })

    result <- do.call(rbind, vertex_list)
    rownames(result) <- NULL

    # Ensure correct column order and types
    result <- data.frame(
      x = result$x,
      y = result$y,
      group = as.integer(result$group),
      fill = result$fill,
      cell_id = as.integer(result$cell_id),
      grid_col = as.integer(result$grid_col),
      grid_row = as.integer(result$grid_row),
      pct = result$pct,
      tooltip_label = result$tooltip_label,
      stringsAsFactors = FALSE
    )

    result
  }
)
