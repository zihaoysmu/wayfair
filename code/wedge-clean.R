library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(sf)
library(tigris)
library(data.table)
library(units)

# Build balanced ZIP-year-wedge and wedge-year panels from Census API outputs.
# Spatial design: configurable state-border buffer + 20-mile grid wedges.
options(tigris_use_cache = TRUE)
dir.create("data/temp", recursive = TRUE, showWarnings = FALSE)
dir.create("data/temp/tigris_cache", recursive = TRUE, showWarnings = FALSE)
options(
  tigris_cache_dir = normalizePath("data/temp/tigris_cache", winslash = "/")
)
sf::sf_use_s2(FALSE)

years <- 2015:2022
crs_projected <- 5070
mile_to_meter <- 1609.344
grid_size_m <- 20 * mile_to_meter
buffer_miles <- as.numeric(Sys.getenv("WEDGE_BUFFER_MILES", "10"))
buffer_m <- buffer_miles * mile_to_meter
market_radius_km <- 1000

format_buffer_suffix <- function(x) {
  label <- format(x, trim = TRUE, scientific = FALSE)
  label <- sub("(\\.\\d*?)0+$", "\\1", label)
  label <- sub("\\.$", "", label)
  paste0(str_replace(label, "\\.", "p"), "mile")
}

buffer_suffix <- format_buffer_suffix(buffer_miles)

temp_csv_path <- function(stem, legacy_stem = stem) {
  if (identical(buffer_suffix, "10mile")) {
    file.path("data/temp", paste0(legacy_stem, ".csv"))
  } else {
    file.path("data/temp", paste0(stem, "_", buffer_suffix, ".csv"))
  }
}

# Standardize Census/API column names before joins.
clean_names <- function(df) {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))
  df
}

# Assert that state/treatment variables are unique inside a wedge-year group.
first_unique <- function(x) {
  ux <- unique(stats::na.omit(x))
  if (length(ux) == 0) return(NA)
  if (length(ux) > 1) {
    stop("Expected a unique value within group, found: ", paste(ux, collapse = ", "))
  }
  ux[[1]]
}

# Economic nexus / Wayfair effective year by state abbreviation.
adoption_year_from_abbr <- function(state_abbr) {
  case_when(
    state_abbr %in% c("MA", "NY", "OH", "PA") ~ 2018L,
    state_abbr %in% c("AZ", "AR", "NM", "OK", "RI", "TN", "TX", "VA") ~ 2020L,
    state_abbr == "LA" ~ 2021L,
    state_abbr %in% c("FL", "KS") ~ 2022L,
    state_abbr == "MO" ~ 2023L,
    is.na(state_abbr) ~ NA_integer_,
    TRUE ~ 2019L
  )
}

# Construct internal state-pair border lines, excluding point-only contacts.
make_state_pair_borders <- function(states_sf) {
  state_neighbors <- st_touches(states_sf)
  state_pairs <- rbindlist(lapply(seq_along(state_neighbors), function(i) {
    j <- state_neighbors[[i]]
    if (length(j) == 0) return(NULL)
    data.table(i = i, j = j)
  }))
  state_pairs <- state_pairs[i < j]

  border_list <- lapply(seq_len(nrow(state_pairs)), function(k) {
    i <- state_pairs$i[k]
    j <- state_pairs$j[k]
    boundary_i <- st_boundary(states_sf[i, ])
    boundary_j <- st_boundary(states_sf[j, ])

    geom <- suppressWarnings(st_intersection(boundary_i, boundary_j))
    geom_type <- as.character(st_geometry_type(geom))
    if (!any(geom_type %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION"))) {
      return(NULL)
    }
    geom <- suppressWarnings(st_collection_extract(geom, "LINESTRING"))
    if (nrow(geom) == 0) return(NULL)

    border_length <- sum(as.numeric(st_length(geom)))
    if (is.na(border_length) || border_length <= 1) return(NULL)

    state_a <- min(states_sf$STUSPS[i], states_sf$STUSPS[j])
    state_b <- max(states_sf$STUSPS[i], states_sf$STUSPS[j])
    statefp_a <- states_sf$STATEFP[match(state_a, states_sf$STUSPS)]
    statefp_b <- states_sf$STATEFP[match(state_b, states_sf$STUSPS)]

    st_sf(
      state_a = state_a,
      state_b = state_b,
      statefp_a = statefp_a,
      statefp_b = statefp_b,
      state_pair_id = paste(state_a, state_b, sep = "_"),
      border_length_m = border_length,
      geometry = st_sfc(st_union(st_geometry(geom)), crs = st_crs(states_sf))
    )
  })

  do.call(rbind, border_list)
}

# Split border-intersecting grid cells into state-side wedges.
make_wedges <- function(states_sf, state_pair_borders, border_grid) {
  grid_border_matches <- st_intersects(border_grid, state_pair_borders)

  border_attrs <- state_pair_borders |>
    st_drop_geometry() |>
    mutate(border_id = row_number())

  grid_border_pairs <- tibble(
    grid_pos = rep(seq_along(grid_border_matches), lengths(grid_border_matches)),
    border_id = unlist(grid_border_matches)
  ) |>
    mutate(grid_id = border_grid$grid_id[grid_pos]) |>
    left_join(border_attrs, by = "border_id")

  grid_pair_sides <- bind_rows(
    grid_border_pairs |>
      transmute(
        grid_id,
        state_pair_id,
        state_a,
        state_b,
        statefp = statefp_a,
        state_abbr = state_a,
        side = if_else(statefp_a == pmin(statefp_a, statefp_b), 1L, 2L)
      ),
    grid_border_pairs |>
      transmute(
        grid_id,
        state_pair_id,
        state_a,
        state_b,
        statefp = statefp_b,
        state_abbr = state_b,
        side = if_else(statefp_b == pmin(statefp_a, statefp_b), 1L, 2L)
      )
  ) |>
    mutate(wedge_pair_id = paste0(state_pair_id, "_g", grid_id))

  grid_state_pieces <- suppressWarnings(
    st_intersection(
      border_grid |> select(grid_id),
      states_sf |> select(STATEFP, STUSPS)
    )
  )
  grid_state_pieces <- suppressWarnings(st_collection_extract(grid_state_pieces, "POLYGON")) |>
    mutate(
      statefp = STATEFP,
      state_abbr = STUSPS,
      wedge_area_m2 = as.numeric(st_area(geometry))
    ) |>
    filter(wedge_area_m2 > 1) |>
    select(grid_id, statefp, state_abbr, wedge_area_m2)

  wedges_sf <- grid_state_pieces |>
    inner_join(
      grid_pair_sides,
      by = c("grid_id", "statefp", "state_abbr")
    ) |>
    select(
      wedge_pair_id, grid_id, state_pair_id, state_a, state_b,
      statefp, state_abbr, side, wedge_area_m2
    ) |>
    mutate(wedge_id = sprintf("w%06d", row_number())) |>
    relocate(wedge_id)

  wedges_sf
}

# Compute sum(payroll_j / distance_ij^1.5) in chunks to control memory use.
compute_market_potential <- function(focal_points,
                                     mass_points,
                                     payroll_panel,
                                     focal_id_col,
                                     output_mp_col,
                                     exclude_same_zip = FALSE,
                                     exclude_same_wedge = FALSE,
                                     chunk_size = 250,
                                     radius_km = 1000) {
  focal_attr <- st_drop_geometry(focal_points) |>
    mutate(focal_index = row_number())
  mass_attr <- st_drop_geometry(mass_points) |>
    mutate(mass_index = row_number())

  focal_coords <- st_coordinates(focal_points)
  mass_coords <- st_coordinates(mass_points)
  payroll_dt <- as.data.table(payroll_panel)
  mass_zip <- mass_attr$zipcode
  mass_wedge <- if ("assigned_wedge_id" %in% names(mass_attr)) {
    mass_attr$assigned_wedge_id
  } else {
    rep(NA_character_, nrow(mass_attr))
  }

  out <- vector("list", ceiling(nrow(focal_points) / chunk_size))
  chunk_id <- 1L

  starts <- seq(1, nrow(focal_points), by = chunk_size)
  for (start in starts) {
    end <- min(start + chunk_size - 1L, nrow(focal_points))
    message(
      "Computing ", output_mp_col, " for focal rows ",
      start, "-", end, " of ", nrow(focal_points), "..."
    )

    focal_chunk <- focal_points[start:end, ]
    within_list <- st_is_within_distance(
      focal_chunk,
      mass_points,
      dist = units::set_units(radius_km, "km")
    )

    pairs <- rbindlist(lapply(seq_along(within_list), function(i) {
      j <- within_list[[i]]
      if (length(j) == 0) return(NULL)
      focal_index <- start + i - 1L
      data.table(focal_index = focal_index, mass_index = j)
    }))

    if (nrow(pairs) == 0) {
      chunk_ids <- focal_attr[[focal_id_col]][start:end]
      chunk_years <- expand_grid(
        !!focal_id_col := chunk_ids,
        year = years
      )
      chunk_years[[output_mp_col]] <- 0
      out[[chunk_id]] <- chunk_years
      chunk_id <- chunk_id + 1L
      next
    }

    pairs[, focal_id := focal_attr[[focal_id_col]][focal_index]]
    pairs[, focal_zipcode := focal_attr$zipcode[focal_index]]
    if ("wedge_id" %in% names(focal_attr)) {
      pairs[, focal_wedge_id := focal_attr$wedge_id[focal_index]]
    } else {
      pairs[, focal_wedge_id := NA_character_]
    }
    pairs[, mass_zipcode := mass_zip[mass_index]]
    pairs[, mass_wedge_id := mass_wedge[mass_index]]

    dx <- focal_coords[pairs$focal_index, 1] - mass_coords[pairs$mass_index, 1]
    dy <- focal_coords[pairs$focal_index, 2] - mass_coords[pairs$mass_index, 2]
    pairs[, dist_km := sqrt(dx^2 + dy^2) / 1000]

    pairs <- pairs[dist_km > 0]
    if (exclude_same_zip) {
      pairs <- pairs[focal_zipcode != mass_zipcode]
    }
    if (exclude_same_wedge) {
      pairs <- pairs[is.na(mass_wedge_id) | focal_wedge_id != mass_wedge_id]
    }

    year_results <- lapply(years, function(y) {
      payroll_y <- payroll_dt[year == y, .(zipcode, payann)]
      payann_vec <- payroll_y$payann[match(pairs$mass_zipcode, payroll_y$zipcode)]
      contrib <- fifelse(is.na(payann_vec), 0, payann_vec) / (pairs$dist_km^1.5)
      data.table(
        focal_id = pairs$focal_id,
        year = y,
        contrib = contrib
      )[, .(market_potential = sum(contrib, na.rm = TRUE)), by = .(focal_id, year)]
    })

    chunk_dt <- rbindlist(year_results)
    complete_dt <- CJ(
      focal_id = focal_attr[[focal_id_col]][start:end],
      year = years,
      unique = TRUE
    )
    chunk_dt <- merge(complete_dt, chunk_dt, by = c("focal_id", "year"), all.x = TRUE)
    chunk_dt[is.na(market_potential), market_potential := 0]
    setnames(chunk_dt, c("focal_id", "market_potential"), c(focal_id_col, output_mp_col))
    out[[chunk_id]] <- as_tibble(chunk_dt)
    chunk_id <- chunk_id + 1L
  }

  bind_rows(out)
}

# 1. Load lower-48 + DC states and ZCTA polygons.
message("Loading state and ZCTA geometries...")
message("Using ", buffer_miles, "-mile state-border buffer.")
lower48_dc <- c(setdiff(state.abb, c("AK", "HI")), "DC")

states_sf <- states(cb = TRUE, year = 2022) |>
  filter(STUSPS %in% lower48_dc) |>
  st_make_valid() |>
  st_transform(crs_projected) |>
  select(STATEFP, STUSPS, NAME)

state_lookup <- states_sf |>
  st_drop_geometry() |>
  transmute(
    statefp = STATEFP,
    state_abbr = STUSPS,
    state_name = NAME
  )

zcta_sf <- zctas(cb = TRUE, year = 2020) |>
  st_make_valid() |>
  st_transform(crs_projected) |>
  mutate(zipcode = coalesce(
    as.character(.data$ZCTA5CE20),
    as.character(.data$GEOID20)
  )) |>
  select(zipcode)

zcta_points_all <- zcta_sf |>
  st_set_geometry(st_point_on_surface(st_geometry(zcta_sf))) |>
  st_join(states_sf |> select(STATEFP, STUSPS), left = TRUE) |>
  filter(!is.na(STATEFP)) |>
  transmute(
    zipcode,
    statefp = STATEFP,
    state_abbr = STUSPS
  )

# 2. Build internal state borders, 20-mile grid cells, and wedges.
message("Constructing internal state borders, grid cells, and wedges...")
state_pair_borders <- make_state_pair_borders(states_sf)

grid <- st_make_grid(
  states_sf,
  cellsize = c(grid_size_m, grid_size_m),
  square = TRUE
)
grid_sf <- st_sf(grid_id = seq_along(grid), geometry = grid)
border_grid <- grid_sf[lengths(st_intersects(grid_sf, state_pair_borders)) > 0, ]

wedges_sf <- make_wedges(states_sf, state_pair_borders, border_grid)
border_buffer <- st_buffer(state_pair_borders, buffer_m)

# 3. Keep ZCTAs near borders and assign each to one largest-overlap wedge.
message("Assigning near-border ZCTAs to wedges by largest overlap...")
zcta_near_border <- zcta_sf[
  lengths(st_intersects(zcta_sf, border_buffer)) > 0 &
    lengths(st_intersects(zcta_sf, wedges_sf)) > 0,
]

zcta_wedge_intersections <- suppressWarnings(st_intersection(
  zcta_near_border |> select(zipcode),
  wedges_sf |> select(
    wedge_id, wedge_pair_id, grid_id, state_pair_id,
    state_a, state_b, statefp, state_abbr, side
  )
))

zcta_area_lookup <- zcta_near_border |>
  mutate(zcta_area = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  select(zipcode, zcta_area)

zcta_wedge_crosswalk <- zcta_wedge_intersections |>
  mutate(overlap_area = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  left_join(zcta_area_lookup, by = "zipcode") |>
  mutate(
    overlap_share = overlap_area / zcta_area,
    buffer_distance_m = buffer_m,
    neighbor_state = if_else(state_abbr == state_a, state_b, state_a),
    adoption_year_other = adoption_year_from_abbr(neighbor_state)
  ) |>
  group_by(zipcode) |>
  slice_max(order_by = overlap_area, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(zipcode) |>
  select(
    zipcode, wedge_id, wedge_pair_id, grid_id, state_pair_id,
    statefp, state_abbr, neighbor_state, side, state_a, state_b,
    adoption_year_other, overlap_area, overlap_share, buffer_distance_m
  )

if (anyDuplicated(zcta_wedge_crosswalk$zipcode) > 0) {
  stop("ZCTA-wedge crosswalk has duplicate ZIP assignments.")
}

write_csv(
  zcta_wedge_crosswalk,
  temp_csv_path("zcta_wedge_crosswalk", "zcta_wedge_crosswalk_10mile")
)

# 4. Build wedge ZIP clusters and diagnostics for the spatial sample.
zcta_assigned_sf <- zcta_sf |>
  inner_join(zcta_wedge_crosswalk, by = "zipcode")

wedge_zip_clusters <- zcta_assigned_sf |>
  group_by(
    wedge_id, wedge_pair_id, state_pair_id, statefp,
    state_abbr, neighbor_state, side
  ) |>
  summarise(n_zctas = n(), geometry = st_union(geometry), .groups = "drop")

multi_state_corner_wedge_pairs <- wedges_sf |>
  st_drop_geometry() |>
  distinct(grid_id, state_pair_id, wedge_pair_id) |>
  add_count(grid_id, name = "n_state_pairs_in_grid") |>
  filter(n_state_pairs_in_grid > 1) |>
  distinct(wedge_pair_id)

wedge_pair_sides <- zcta_wedge_crosswalk |>
  distinct(wedge_pair_id, side) |>
  count(wedge_pair_id, name = "n_sides")

border_wedge_diagnostics <- tibble(
  buffer_miles = buffer_miles,
  buffer_distance_m = buffer_m,
  number_of_state_pairs = nrow(state_pair_borders),
  number_of_grid_cells_intersecting_borders = nrow(border_grid),
  number_of_wedges = nrow(wedges_sf),
  number_of_wedge_pairs = n_distinct(wedges_sf$wedge_pair_id),
  number_of_zctas_in_buffer_sample = nrow(zcta_near_border),
  number_of_zctas_assigned_to_wedges = nrow(zcta_wedge_crosswalk),
  number_of_wedge_pairs_with_two_sides = sum(wedge_pair_sides$n_sides == 2),
  number_of_wedge_pairs_missing_one_side = sum(wedge_pair_sides$n_sides < 2),
  number_of_multistate_corner_wedge_pairs = nrow(multi_state_corner_wedge_pairs)
)

write_csv(
  border_wedge_diagnostics,
  temp_csv_path("border_wedge_diagnostics")
)

# 5. Clean state tax controls and ZIP-year outcomes/controls.
message("Cleaning tax, CIT, retailer, payroll, and population data...")
sales_tax <- read_xlsx("data/raw/combined sales tax.xlsx") |>
  rename(state_name = GEO_ID) |>
  left_join(state_lookup, by = "state_name") |>
  mutate(across(starts_with("tax_"), ~ suppressWarnings(as.numeric(.x)))) |>
  pivot_longer(
    cols = starts_with("tax_"),
    names_to = "tax_year",
    values_to = "sales_tax"
  ) |>
  mutate(year = as.integer(str_remove(tax_year, "^tax_"))) |>
  select(state_abbr, year, sales_tax)

cit <- read_xlsx("data/raw/us_state_corporate_tax.xlsx") |>
  rename(state_name = state_name, state_abbr = abbrev) |>
  mutate(across(starts_with("corporate_tax_"), ~ suppressWarnings(as.numeric(.x)))) |>
  pivot_longer(
    cols = starts_with("corporate_tax_"),
    names_to = "cit_year",
    values_to = "cit"
  ) |>
  mutate(year = as.integer(str_remove(cit_year, "^corporate_tax_"))) |>
  select(state_abbr, year, cit)

retailer <- read_csv(
  "data/temp/zbp_retailer_2015_2022.csv",
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names() |>
  filter(empszes_label == "All establishments") |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    estab = as.numeric(estab),
    naics = recode(
      naics,
      "online retailer" = "online_estab",
      "local retailer" = "local_estab"
    )
  ) |>
  group_by(zipcode, year, naics) |>
  summarise(estab = sum(estab, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = naics,
    values_from = estab,
    values_fill = 0
  )

payroll <- read_csv(
  "data/temp/zbp_payroll_2015_2022.csv",
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names() |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    payann = as.numeric(payann)
  ) |>
  group_by(zipcode, year) |>
  summarise(payann = sum(payann, na.rm = TRUE), .groups = "drop")

population <- read_csv(
  "data/temp/acs5_zipcode_population_2015_2022.csv",
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names() |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    population = as.numeric(population)
  ) |>
  select(zipcode, year, population)

# 6. Prepare all lower-48 + DC ZIP payroll masses for market potential.
all_mass_zips <- zcta_points_all |>
  st_drop_geometry() |>
  distinct(zipcode)

payroll_panel <- expand_grid(
  zipcode = all_mass_zips$zipcode,
  year = years
) |>
  left_join(payroll, by = c("zipcode", "year")) |>
  mutate(payann = replace_na(payann, 0))

zcta_points_for_mp <- zcta_points_all |>
  left_join(
    zcta_wedge_crosswalk |> select(zipcode, assigned_wedge_id = wedge_id),
    by = "zipcode"
  )

focal_zip_points <- zcta_points_for_mp |>
  semi_join(zcta_wedge_crosswalk, by = "zipcode")

# 7. ZIP MP: focal ZIP to other ZIP payroll masses within 1000 km.
message("Computing ZIP-level market potential...")
zipcode_market_potential <- compute_market_potential(
  focal_points = focal_zip_points,
  mass_points = zcta_points_for_mp,
  payroll_panel = payroll_panel,
  focal_id_col = "zipcode",
  output_mp_col = "zipcode_market_potential",
  exclude_same_zip = TRUE,
  exclude_same_wedge = FALSE,
  chunk_size = 250,
  radius_km = market_radius_km
)

write_csv(
  zipcode_market_potential,
  temp_csv_path("zipcode_market_potential_2015_2022")
)

# 8. Wedge MP: focal wedge point to other ZIP payroll masses within 1000 km.
message("Computing wedge-level market potential from other ZIP payroll masses...")
wedge_points <- wedge_zip_clusters |>
  st_set_geometry(st_point_on_surface(st_geometry(wedge_zip_clusters)))

wedge_market_potential <- compute_market_potential(
  focal_points = wedge_points,
  mass_points = zcta_points_for_mp,
  payroll_panel = payroll_panel,
  focal_id_col = "wedge_id",
  output_mp_col = "wedge_market_potential",
  exclude_same_zip = FALSE,
  exclude_same_wedge = TRUE,
  chunk_size = 250,
  radius_km = market_radius_km
)

write_csv(
  wedge_market_potential,
  temp_csv_path("wedge_market_potential_2015_2022")
)

# 9. Build ZIP-year-wedge panel and fill missing establishment/payroll as zero.
message("Building balanced ZIP-year-wedge data...")
zipcode_year_wedge <- zcta_wedge_crosswalk |>
  crossing(year = years) |>
  left_join(retailer, by = c("zipcode", "year")) |>
  left_join(payroll, by = c("zipcode", "year")) |>
  left_join(population, by = c("zipcode", "year")) |>
  left_join(zipcode_market_potential, by = c("zipcode", "year")) |>
  left_join(sales_tax, by = c("state_abbr", "year")) |>
  left_join(cit, by = c("state_abbr", "year")) |>
  mutate(
    online_estab = replace_na(online_estab, 0),
    local_estab = replace_na(local_estab, 0),
    payann = replace_na(payann, 0),
    post_wayfair_other = as.integer(year >= adoption_year_other)
  )

required_zip_cols <- c(
  "online_estab", "local_estab", "sales_tax", "cit", "population",
  "zipcode_market_potential", "neighbor_state", "post_wayfair_other"
)

# Drop ZIP-year rows missing required controls or market potential.
# Last full run attrition:
#   before this filter: 62,920 ZIP-year rows = 7,865 assigned ZIPs x 8 years
#   after this filter:  62,014 ZIP-year rows
#   removed here:          906 ZIP-year rows with missing required fields
zipcode_year_wedge <- zipcode_year_wedge |>
  filter(if_all(all_of(required_zip_cols), ~ !is.na(.x)))

# Keep only ZIP-wedge units observed in all years.
# Last full run attrition:
#   after required-field filter: 62,014 ZIP-year rows
#   after balance filter:        61,712 ZIP-year rows = 7,714 ZIPs x 8 years
#   removed here:                   302 ZIP-year rows, corresponding to 151 ZIPs
balanced_zip_ids <- zipcode_year_wedge |>
  group_by(zipcode, wedge_id) |>
  summarise(n_years = n_distinct(year), .groups = "drop") |>
  filter(n_years == length(years)) |>
  select(zipcode, wedge_id)

zipcode_year_wedge <- zipcode_year_wedge |>
  semi_join(balanced_zip_ids, by = c("zipcode", "wedge_id")) |>
  arrange(wedge_id, zipcode, year)

if (anyDuplicated(zipcode_year_wedge |> select(zipcode, year, wedge_id)) > 0) {
  stop("ZIP-year-wedge data has duplicate zipcode-year-wedge rows.")
}

zip_balance_check <- zipcode_year_wedge |>
  count(zipcode, wedge_id, name = "n_rows")
if (any(zip_balance_check$n_rows != length(years))) {
  stop("ZIP-year-wedge data is not balanced.")
}

write_csv(
  zipcode_year_wedge,
  temp_csv_path("zipcode_year_wedge_balanced_2015_2022")
)

# 10. Aggregate balanced ZIP data to wedge-year level.
message("Aggregating to balanced wedge-year data...")
wedge_year <- zipcode_year_wedge |>
  group_by(wedge_id, year) |>
  summarise(
    online_estab = sum(online_estab, na.rm = TRUE),
    local_estab = sum(local_estab, na.rm = TRUE),
    payann = sum(payann, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    wedge_pair_id = first_unique(wedge_pair_id),
    grid_id = first_unique(grid_id),
    state_pair_id = first_unique(state_pair_id),
    statefp = first_unique(statefp),
    state_abbr = first_unique(state_abbr),
    neighbor_state = first_unique(neighbor_state),
    side = first_unique(side),
    state_a = first_unique(state_a),
    state_b = first_unique(state_b),
    adoption_year_other = first_unique(adoption_year_other),
    sales_tax = first_unique(sales_tax),
    cit = first_unique(cit),
    post_wayfair_other = first_unique(post_wayfair_other),
    .groups = "drop"
  ) |>
  left_join(wedge_market_potential, by = c("wedge_id", "year")) |>
  filter(!is.na(wedge_market_potential))

# Keep only wedges observed in all years.
# Last full run attrition:
#   possible wedge-year rows from retained ZIP sample: 14,760 = 1,845 wedges x 8 years
#   final wedge-year rows:                            14,760
#   removed here:                                          0 wedge-year rows
balanced_wedge_ids <- wedge_year |>
  group_by(wedge_id) |>
  summarise(n_years = n_distinct(year), .groups = "drop") |>
  filter(n_years == length(years)) |>
  select(wedge_id)

wedge_year <- wedge_year |>
  semi_join(balanced_wedge_ids, by = "wedge_id") |>
  arrange(wedge_id, year)

if (anyDuplicated(wedge_year |> select(wedge_id, year)) > 0) {
  stop("Wedge-year data has duplicate wedge-year rows.")
}

wedge_balance_check <- wedge_year |>
  count(wedge_id, name = "n_rows")
if (any(wedge_balance_check$n_rows != length(years))) {
  stop("Wedge-year data is not balanced.")
}

# Verify wedge totals are exact sums of the retained ZIP rows.
wedge_sum_check <- zipcode_year_wedge |>
  semi_join(wedge_year |> distinct(wedge_id), by = "wedge_id") |>
  group_by(wedge_id, year) |>
  summarise(
    online_estab = sum(online_estab, na.rm = TRUE),
    local_estab = sum(local_estab, na.rm = TRUE),
    payann = sum(payann, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    wedge_year |>
      select(wedge_id, year, online_estab, local_estab, payann, population),
    by = c("wedge_id", "year"),
    suffix = c("_zip_sum", "_wedge")
  )

if (
  any(abs(wedge_sum_check$online_estab_zip_sum - wedge_sum_check$online_estab_wedge) > 1e-8) ||
    any(abs(wedge_sum_check$local_estab_zip_sum - wedge_sum_check$local_estab_wedge) > 1e-8) ||
    any(abs(wedge_sum_check$payann_zip_sum - wedge_sum_check$payann_wedge) > 1e-8) ||
    any(abs(wedge_sum_check$population_zip_sum - wedge_sum_check$population_wedge) > 1e-8)
) {
  stop("Wedge totals do not match ZIP-level sums.")
}

write_csv(
  wedge_year,
  temp_csv_path("wedge_year_balanced_2015_2022")
)

# 11. Report final panel sizes.
message("Done.")
message("Buffer suffix: ", buffer_suffix)
message("ZIP-year-wedge rows: ", nrow(zipcode_year_wedge))
message("Wedge-year rows: ", nrow(wedge_year))
