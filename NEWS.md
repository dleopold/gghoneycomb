# gghoneycomb 0.0.0.9000 (development)

## Breaking changes

- The `compaction` parameter has been removed. Replace it with the new
  `silhouette`, `layout`, `compact_style`, and `temperature` parameters.
  Passing `compaction` now raises an error with migration guidance:
  - `compaction = 1` -> `silhouette = "rect"`
  - `compaction < 1` -> `silhouette = "rounded"` or `silhouette = "organic"`

## New features

### Silhouette shapes

- `silhouette = "rect"` (default): clean rectangular grid, same footprint as
  before.
- `silhouette = "rounded"`: rectangular grid with softened corners for a
  friendlier look.
- `silhouette = "organic"`: irregular boundary grown outward from a central
  seed, producing a natural, blob-like shape.

### Layout modes

- `layout = "compact"` (default): deterministic region allocation.
  `temperature` must be 0 (the default).
- `layout = "free"`: stochastic top-K sampling. `temperature` controls
  randomness; the default is 0.35. Requires `seed` for reproducibility.

### Compact styles

- `compact_style = "perimeter"` (default): greedy growth that minimizes region
  perimeter at each step, with a carve fallback to guarantee contiguity.
- `compact_style = "blocky"`: directional face-carving that produces
  rectangular, treemap-like blocks. Requires `silhouette = "rect"`.

### Temperature control

- `temperature`: a single non-negative number. `NULL` (default) picks 0 for
  compact layouts and 0.35 for free layouts automatically.

### Other

- Category contiguity is guaranteed in all layout modes.
- Compactness scoring utilities are available as internal helpers
  (`hex-compactness` topic).
- `seed` parameter now works consistently across all silhouette and layout
  combinations.
- `n_cells` hard cap raised to 2500.

- `rotation` parameter (0, 90, 180, 270) rotates the rendered honeycomb.
  Applied to output coordinates only; allocation and contiguity are unchanged.
- Higher `temperature` values in free mode produce more irregular, less compact
  category regions while preserving per-category connectivity.

## Documentation

- New "Getting Started" vignette covering silhouette shapes, layout modes,
  cell count selection, proportions input, and grid constraints.
- README rewritten with a Features overview, Quick Start example, and a
  four-example Gallery.
- pkgdown reference page organized into logical groups: honeycomb plots,
  scales and themes, hex grid utilities.
