#' Round Proportions to Integer Cell Counts (Algorithm A0)
#'
#' Converts normalized proportions and a target cell count into integer cell
#' allocations using the largest remainder method with minimum-1 enforcement
#' for nonzero categories.
#'
#' @param proportions Numeric vector of proportions (must sum to 1, within 1e-6 tolerance).
#' @param n_cells Positive integer target cell count.
#' @param labels Character vector of category labels (same length as proportions).
#'   Used for deterministic tie-breaking.
#'
#' @return Integer vector of cell counts (same length as proportions), summing to n_cells.
#'
#' @details
#' Algorithm A0 (Largest Remainder with Minimum-1 Enforcement):
#'
#' 1. Check: if n_cells < number of nonzero categories, error.
#' 2. Compute expected counts: e_i = proportions_i * n_cells.
#' 3. Initialize targets: t_i = floor(e_i).
#' 4. Enforce minimum 1 for nonzero categories:
#'    - For any category with proportions_i > 0 and t_i == 0, set t_i = 1.
#'    - Subtract required cells from largest t_i categories (never below 1).
#' 5. Compute remainder: r = n_cells - sum(t_i).
#' 6. If r > 0, distribute +1 to r categories with largest fractional remainder
#'    (e_i - floor(e_i)), breaking ties by label (alphabetical).
#' 7. If r < 0 after minimum enforcement, error (constraints too tight).
#'
#' @keywords internal
#' @noRd
round_proportions <- function(proportions, n_cells, labels) {
  # Input validation
  if (!is.numeric(proportions) || !is.numeric(n_cells) || !is.character(labels)) {
    rlang::abort("proportions must be numeric, n_cells must be numeric, labels must be character")
  }

  if (length(proportions) != length(labels)) {
    rlang::abort("proportions and labels must have the same length")
  }

  if (n_cells != as.integer(n_cells) || n_cells <= 0) {
    rlang::abort("n_cells must be a positive integer")
  }

  n_cells <- as.integer(n_cells)

  # Check proportions sum to 1 (within tolerance)
  prop_sum <- sum(proportions)
  if (abs(prop_sum - 1.0) > 1e-6) {
    rlang::abort(
      paste0(
        "proportions must sum to 1 (within 1e-6 tolerance); got sum = ",
        round(prop_sum, 6)
      )
    )
  }

  # Count nonzero categories
  nonzero_mask <- proportions > 0
  n_nonzero <- sum(nonzero_mask)

  # Check: n_cells >= n_nonzero
  if (n_cells < n_nonzero) {
    rlang::abort(
      paste0(
        "n_cells (", n_cells, ") must be >= number of nonzero categories (",
        n_nonzero, ")"
      )
    )
  }

  # Compute expected counts
  expected <- proportions * n_cells

  # Initialize targets as floor
  targets <- floor(expected)

  # Enforce minimum 1 for nonzero categories
  # First, identify which nonzero categories have t_i == 0
  needs_one <- nonzero_mask & (targets == 0)
  n_needs_one <- sum(needs_one)

  if (n_needs_one > 0) {
    # Set these to 1
    targets[needs_one] <- 1L

    # If minimum enforcement caused us to exceed n_cells, subtract the overflow
    # from the largest targets, never dropping below 1.
    overflow <- sum(targets) - n_cells
    if (overflow > 0) {
      overflow <- as.integer(overflow)
      sorted_idx <- order(-targets, labels)

      remaining <- overflow
      for (idx in sorted_idx) {
        if (remaining <= 0) break
        cap <- targets[idx] - 1L
        if (cap <= 0) next
        take <- min(cap, remaining)
        targets[idx] <- targets[idx] - take
        remaining <- remaining - take
      }

      if (remaining > 0) {
        rlang::abort(
          paste0(
            "Cannot enforce minimum 1 for all nonzero categories; ",
            "n_cells too small relative to number of categories"
          )
        )
      }
    }
  }

  # Compute remainder
  remainder <- n_cells - sum(targets)

  # Distribute remainder using largest fractional part
  if (remainder > 0) {
    # Compute fractional parts
    fractional <- expected - floor(expected)

    # Create a data frame for sorting
    df <- data.frame(
      idx = seq_along(proportions),
      frac = fractional,
      label = labels,
      stringsAsFactors = FALSE
    )

    # Sort by fractional part (descending), then by label (ascending) for tie-break
    df <- df[order(-df$frac, df$label), ]

    # Distribute +1 to top remainder categories
    for (i in seq_len(remainder)) {
      idx <- df$idx[i]
      targets[idx] <- targets[idx] + 1L
    }
  } else if (remainder < 0) {
    rlang::abort(
      paste0(
        "Remainder is negative (", remainder, ") after minimum enforcement; ",
        "this indicates a constraint violation"
      )
    )
  }

  # Final validation
  if (sum(targets) != n_cells) {
    rlang::abort(
      paste0(
        "Internal error: targets sum to ", sum(targets), " instead of ", n_cells
      )
    )
  }

  targets
}
