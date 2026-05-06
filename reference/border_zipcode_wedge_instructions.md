# Border ZIP/ZCTA Wedge Construction Guide for Codex

This file converts the border-handling procedure in Rohlin, Rosenthal, and Ross (2014), *Tax avoidance and business location in a state border model*, into an implementation guide. The goal is to construct ZIP/ZCTA-level local comparison units near state borders.

The core idea is:

> Use a distance buffer to define which ZIP/ZCTA areas are close enough to a state border, and use grid-based wedges to define which nearby ZIP/ZCTA areas are locally opposite each other across the same border segment.

---

## 1. Concepts

### State border
An internal boundary shared by two U.S. states. Do not include coastlines, international borders, or outer state boundaries that are not shared with another state.

### 20-by-20 mile grid
A regular square grid laid over the continental United States. Each cell is 20 miles by 20 miles. Only grid cells that intersect state borders are retained.

### Wedge
A piece of a retained grid cell after it is cut by a state border. For a simple two-state border crossing one grid cell, there will usually be one wedge on each side of the border.

### Wedge-pair
A pair of wedges that belong to the same grid cell and lie on opposite sides of the same state border. This is the local comparison unit.

### 10-mile buffer
A buffer extending 10 miles on each side of internal state borders. It is used to define which ZIP/ZCTA areas are close enough to the border to enter the analysis sample.

### ZIP/ZCTA cluster
The union of all ZIP/ZCTA polygons assigned to the same wedge. A wedge-pair therefore has two ZIP/ZCTA clusters, one on each side of the border.

---

## 2. Why both wedges and a buffer are needed

Do not treat the wedge and the buffer as substitutes.

The buffer answers:

> Which ZIP/ZCTA areas are close enough to the state border?

The wedge-pair answers:

> Among those nearby ZIP/ZCTA areas, which ones are locally opposite each other across the same border segment?

If we only use the 10-mile buffer, we know that ZIP/ZCTA areas are near a border, but we do not know which local section of the opposite border side should be used as the comparison group.

If we only use wedges, we may include ZIP/ZCTA areas that fall inside a border-intersecting grid cell but are not truly near the border. Since each grid cell is 20 by 20 miles, some included areas could be far from the actual state boundary.

Therefore:

- Use the 10-mile buffer for sample restriction.
- Use wedge-pairs for local matching and fixed effects.

---

## 3. Required inputs

Use geometries in a projected CRS with meter units. For the contiguous United States, EPSG:5070 is recommended.

Required spatial inputs:

1. Lower-48 state polygons.
2. ZIP or ZCTA polygons.
3. Optional: establishment, employment, population, or business-pattern data linked to ZIP/ZCTA codes.

Recommended R packages:

```r
library(sf)
library(dplyr)
library(data.table)
library(tigris)
library(stringr)
```

Recommended global settings:

```r
options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)
```

---

## 4. Parameters

```r
crs_projected <- 5070
mile_to_meter <- 1609.344

grid_size_m <- 20 * mile_to_meter
main_buffer_m <- 10 * mile_to_meter
robust_buffer_m <- 1 * mile_to_meter
```

Use `main_buffer_m` for the baseline sample. Use `robust_buffer_m` for a robustness check.

---

## 5. Algorithm overview

### Step 1: Load and clean lower-48 states

1. Load U.S. state polygons.
2. Drop Alaska, Hawaii, Puerto Rico, territories, and DC if not needed.
3. Transform to EPSG:5070.
4. Make geometries valid.

Expected object:

```r
states_sf
# columns: STATEFP, STUSPS, NAME, geometry
```

---

### Step 2: Construct internal state-pair borders

Construct borders only between adjacent states.

Output should have one row per state-pair border segment or multipolygon/multiline geometry.

Expected object:

```r
state_pair_borders
# columns: state_a, state_b, state_pair_id, geometry
```

Recommended `state_pair_id` format:

```r
state_pair_id = paste(pmin(state_a, state_b), pmax(state_a, state_b), sep = "_")
```

Implementation idea:

1. Take boundaries of all state polygons.
2. Identify where boundaries of two distinct states overlap or touch along a line.
3. Keep only line intersections with positive length.
4. Drop point-only contacts unless explicitly needed for four-corners logic.

Important:

- Do not include coastlines.
- Do not include national borders.
- Do not include state boundary pieces that are not shared with another state.

---

### Step 3: Create the 20-by-20 mile grid

Create a regular grid over the bounding box of the lower-48 states.

```r
grid <- st_make_grid(
  states_sf,
  cellsize = c(grid_size_m, grid_size_m),
  square = TRUE
)

grid_sf <- st_sf(
  grid_id = seq_along(grid),
  geometry = grid
)
```

Keep only cells that intersect at least one internal state border.

```r
border_grid <- grid_sf[lengths(st_intersects(grid_sf, state_pair_borders)) > 0, ]
```

Notes:

- The exact grid origin is arbitrary.
- Record the grid origin and CRS for reproducibility.
- For robustness, consider shifting the grid by 5 or 10 miles east/north and rebuilding the sample.

---

### Step 4: Construct wedges and wedge-pairs

For each retained grid cell and each state-pair border that intersects that grid cell:

1. Identify the two states in the state pair.
2. Intersect the grid cell with state A.
3. Intersect the grid cell with state B.
4. The resulting pieces are the two wedges.
5. Assign a common `wedge_pair_id` to the two wedges.

Expected object:

```r
wedges_sf
# columns:
# wedge_id
# wedge_pair_id
# grid_id
# state_pair_id
# statefp
# side
# geometry
```

Recommended side convention:

```r
side = 1 if statefp == min(state_a, state_b)
side = 2 if statefp == max(state_a, state_b)
```

This follows the paper's spirit of assigning side labels mechanically rather than based on tax rates or outcomes.

Important edge cases:

- At three-state or four-state junctions, one grid cell may generate more than one state-pair wedge-pair.
- The same wedge-like area can be associated with multiple state-pairs near corners.
- Keep these cases if they are valid, but flag them for diagnostics.
- If results are sensitive to these cases, run a robustness check dropping multi-state-corner wedge-pairs.

---

### Step 5: Create the state-border buffer

Create a 10-mile buffer around internal state-pair borders.

```r
border_buffer_10 <- st_buffer(state_pair_borders, main_buffer_m)
```

For robustness:

```r
border_buffer_1 <- st_buffer(state_pair_borders, robust_buffer_m)
```

The buffer is only for selecting near-border ZIP/ZCTA areas. It should not replace wedge-pair construction.

---

### Step 6: Load ZIP/ZCTA polygons

Load ZIP or ZCTA polygons and transform to the same projected CRS.

Expected object:

```r
zcta_sf
# columns: GEOID, geometry
```

Clean geometries:

```r
zcta_sf <- zcta_sf |>
  st_make_valid() |>
  st_transform(crs_projected)
```

---

### Step 7: Select near-border ZIP/ZCTA areas

For the baseline sample, keep ZIP/ZCTA polygons that satisfy both conditions:

1. The ZIP/ZCTA intersects the 10-mile border buffer.
2. The ZIP/ZCTA intersects at least one wedge.

```r
zcta_near_border <- zcta_sf[
  lengths(st_intersects(zcta_sf, border_buffer_10)) > 0 &
  lengths(st_intersects(zcta_sf, wedges_sf)) > 0,
]
```

This matches the paper's rule: retain ZIP codes that lie at least partly within the buffer and intersect or lie within a wedge.

---

### Step 8: Assign each ZIP/ZCTA to exactly one wedge

A ZIP/ZCTA may overlap multiple wedges because ZIP/ZCTA polygons and grid cells are irregular. Assign each ZIP/ZCTA to the wedge with the largest overlap area.

Procedure:

1. Intersect near-border ZIP/ZCTA polygons with wedges.
2. Compute overlap area.
3. For each ZIP/ZCTA, keep the wedge with the largest overlap.

Pseudo-code:

```r
zcta_wedge_intersections <- st_intersection(
  zcta_near_border |> select(GEOID),
  wedges_sf |> select(wedge_id, wedge_pair_id, state_pair_id, statefp, side)
)

zcta_wedge_intersections$overlap_area <- as.numeric(st_area(zcta_wedge_intersections))

zcta_wedge_crosswalk <- zcta_wedge_intersections |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  slice_max(order_by = overlap_area, n = 1, with_ties = FALSE) |>
  ungroup()
```

Expected output:

```r
zcta_wedge_crosswalk
# columns:
# GEOID
# wedge_id
# wedge_pair_id
# state_pair_id
# statefp
# side
# overlap_area
```

Important replication choice:

- Once a ZIP/ZCTA is assigned to a wedge, assign the full ZIP/ZCTA's establishments or attributes to that wedge.
- Do not split establishments across wedges unless you are deliberately modifying the paper's method.
- If using population or employment counts from ZCTA-level data, decide explicitly whether to assign the full count or area-apportion it. To stay closest to the paper's ZIP-code business-location logic, use full assignment.

---

### Step 9: Construct ZIP/ZCTA clusters by wedge

For each wedge, union the geometries of all assigned ZIP/ZCTA polygons.

```r
zcta_assigned_sf <- zcta_sf |>
  inner_join(zcta_wedge_crosswalk, by = "GEOID")

wedge_zip_clusters <- zcta_assigned_sf |>
  group_by(wedge_id, wedge_pair_id, state_pair_id, statefp, side) |>
  summarise(geometry = st_union(geometry), .groups = "drop")
```

Each wedge-pair should usually have two clusters: one for side 1 and one for side 2.

---

### Step 10: Attach business or population data

Merge ZIP/ZCTA-level data to `zcta_wedge_crosswalk` by ZIP/ZCTA code.

Example:

```r
zbp_border <- zbp_data |>
  inner_join(zcta_wedge_crosswalk, by = c("ZIP" = "GEOID"))
```

For establishment-level reconstruction from ZIP-level counts, if a ZIP has `x` new establishments, create `x` rows with the same ZIP/wedge/state attributes.

Outcome convention:

```r
I_it = 1 if establishment locates on side 2
I_it = 0 if establishment locates on side 1
```

---

## 6. Main outputs Codex should create

### 1. ZIP/ZCTA-to-wedge crosswalk

File name suggestion:

```text
zcta_wedge_crosswalk_10mile.csv
```

Columns:

```text
GEOID
wedge_id
wedge_pair_id
grid_id
state_pair_id
statefp
side
overlap_area
overlap_share_optional
buffer_distance_m
```

### 2. Wedge geometries

File name suggestion:

```text
wedges_20mile_grid.gpkg
```

Layer columns:

```text
wedge_id
wedge_pair_id
grid_id
state_pair_id
statefp
side
geometry
```

### 3. Wedge-pair ZIP/ZCTA clusters

File name suggestion:

```text
wedge_zip_clusters_10mile.gpkg
```

Layer columns:

```text
wedge_id
wedge_pair_id
state_pair_id
statefp
side
n_zctas
geometry
```

### 4. Diagnostics table

File name suggestion:

```text
border_wedge_diagnostics.csv
```

Useful diagnostics:

```text
number_of_state_pairs
number_of_grid_cells_intersecting_borders
number_of_wedges
number_of_wedge_pairs
number_of_zctas_in_10mile_sample
number_of_zctas_assigned_to_wedges
number_of_wedge_pairs_with_two_sides
number_of_wedge_pairs_missing_one_side
number_of_multistate_corner_wedge_pairs
```

---

## 7. Quality checks

Codex should implement explicit checks.

### Geometry checks

- All spatial layers use the same projected CRS.
- Geometries are valid after each major operation.
- State borders are internal borders only.
- Buffer distances are in meters after converting from miles.

### Assignment checks

- Each ZIP/ZCTA in the analysis sample is assigned to exactly one wedge.
- No ZIP/ZCTA has duplicate final assignments.
- Each assigned wedge belongs to exactly one state side within a state-pair.
- Most wedge-pairs should have both side 1 and side 2 represented.

### Replication checks

- 10-mile buffer should produce a larger sample than 1-mile buffer.
- Wedge-pair counts should be stable across small coding changes.
- Results should be checked under alternative grid origins because the square grid placement is arbitrary.

---

## 8. Robustness variants

Implement these as optional flags or separate scripts.

### 1-mile buffer sample

Repeat the same procedure using `robust_buffer_m <- 1 * 1609.344`.

### Alternative grid cell size

Try:

```r
grid_size_m <- 10 * mile_to_meter
grid_size_m <- 30 * mile_to_meter
```

### Shifted grid origin

Because the grid origin is arbitrary, create robustness samples by shifting the grid by:

```text
5 miles east
5 miles north
10 miles east
10 miles north
10 miles east and 10 miles north
```

### Drop multi-state-corner cells

Drop wedge-pairs where one grid cell is associated with more than one state-pair, especially near three-state or four-state junctions.

### Alternative assignment rule

Compare maximum-overlap assignment with nearest-border-segment or centroid-based assignment. Treat this only as a robustness check, not the baseline.

---

## 9. Common mistakes to avoid

Do not include all ZIP/ZCTA polygons inside border-intersecting grid cells without applying the 10-mile buffer. That would include areas that may not be close to the border.

Do not use the 10-mile buffer alone as the comparison unit. It does not define local opposite-side matches along the same border segment.

Do not assign ZIP/ZCTA polygons to multiple wedges in the baseline. The paper assigns each ZIP code to the wedge with which it most overlaps.

Do not use longitude-latitude degrees to measure 10 miles or 20 miles. Always project first.

Do not include coastal or international borders.

Do not define side 1 and side 2 based on tax rates. Use a mechanical state ordering rule, then construct tax differences separately.

Do not silently drop complicated state-corner cases. Either keep and flag them, or drop them in a documented robustness check.

---

## 10. Minimal R skeleton

```r
library(sf)
library(dplyr)
library(data.table)
library(tigris)

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

crs_projected <- 5070
mile_to_meter <- 1609.344
grid_size_m <- 20 * mile_to_meter
buffer_m <- 10 * mile_to_meter

# 1. Load states
states_sf <- states(cb = TRUE, year = 2022) |>
  filter(!STUSPS %in% c("AK", "HI", "PR", "DC")) |>
  st_make_valid() |>
  st_transform(crs_projected)

# 2. Construct internal state-pair borders
# TODO: implement robust state-pair boundary construction.
# Expected output: state_pair_borders with state_a, state_b, state_pair_id, geometry.

# 3. Build 20-by-20 mile grid
grid <- st_make_grid(states_sf, cellsize = c(grid_size_m, grid_size_m), square = TRUE)
grid_sf <- st_sf(grid_id = seq_along(grid), geometry = grid)

border_grid <- grid_sf[lengths(st_intersects(grid_sf, state_pair_borders)) > 0, ]

# 4. Build wedges
# TODO: for each border_grid cell and state_pair, intersect the cell with each state in the pair.
# Expected output: wedges_sf.

# 5. Create 10-mile border buffer
border_buffer <- st_buffer(state_pair_borders, buffer_m)

# 6. Load ZCTA polygons
zcta_sf <- zctas(cb = TRUE, year = 2020) |>
  st_make_valid() |>
  st_transform(crs_projected)

# 7. Keep ZCTAs near border and intersecting wedges
zcta_near_border <- zcta_sf[
  lengths(st_intersects(zcta_sf, border_buffer)) > 0 &
    lengths(st_intersects(zcta_sf, wedges_sf)) > 0,
]

# 8. Assign ZCTAs to largest-overlap wedge
zcta_wedge_intersections <- st_intersection(
  zcta_near_border |> select(GEOID),
  wedges_sf |> select(wedge_id, wedge_pair_id, grid_id, state_pair_id, statefp, side)
)

zcta_wedge_intersections$overlap_area <- as.numeric(st_area(zcta_wedge_intersections))

zcta_wedge_crosswalk <- zcta_wedge_intersections |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  slice_max(overlap_area, n = 1, with_ties = FALSE) |>
  ungroup()

# 9. Construct ZIP/ZCTA clusters by wedge
zcta_assigned_sf <- zcta_sf |>
  inner_join(zcta_wedge_crosswalk, by = "GEOID")

wedge_zip_clusters <- zcta_assigned_sf |>
  group_by(wedge_id, wedge_pair_id, state_pair_id, statefp, side) |>
  summarise(
    n_zctas = n(),
    geometry = st_union(geometry),
    .groups = "drop"
  )

# 10. Save outputs
write.csv(zcta_wedge_crosswalk, "zcta_wedge_crosswalk_10mile.csv", row.names = FALSE)
st_write(wedges_sf, "wedges_20mile_grid.gpkg", delete_dsn = TRUE)
st_write(wedge_zip_clusters, "wedge_zip_clusters_10mile.gpkg", delete_dsn = TRUE)
```

---

## 11. Suggested file structure for the project

```text
code/
  01_load_geographies.R
  02_construct_state_pair_borders.R
  03_construct_grid_and_wedges.R
  04_assign_zctas_to_wedges.R
  05_merge_business_data.R
  06_diagnostics.R

data/raw/
  states/
  zctas/
  zbp/

data/processed/
  state_pair_borders.gpkg
  wedges_20mile_grid.gpkg
  zcta_wedge_crosswalk_10mile.csv
  zcta_wedge_crosswalk_1mile.csv
  wedge_zip_clusters_10mile.gpkg
  border_wedge_diagnostics.csv
```

---

## 12. Method summary for paper notes

A concise description suitable for writing:

> I follow the border-wedge approach of Rohlin, Rosenthal, and Ross (2014). I overlay a 20-by-20 mile grid on the continental United States and retain grid cells that intersect internal state borders. State borders divide these cells into wedges, and wedges on opposite sides of the same border within the same grid cell define wedge-pairs. I then construct a 10-mile buffer on each side of state borders and retain ZIP/ZCTA areas that lie at least partly within the buffer and intersect a wedge. Each ZIP/ZCTA is assigned to the wedge with which it has the largest area of overlap. The ZIP/ZCTA areas assigned to each wedge are unioned to form local border-side clusters. These clusters define the two sides of each local border comparison unit.
```

