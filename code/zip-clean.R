library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(sf)
library(tigris)
library(data.table)

options(tigris_use_cache = TRUE)
dir.create("data/temp", recursive = TRUE, showWarnings = FALSE)
dir.create("data/temp/tigris_cache", recursive = TRUE, showWarnings = FALSE)
options(
  tigris_cache_dir = normalizePath("data/temp/tigris_cache", winslash = "/")
)
sf::sf_use_s2(FALSE)

years <- 2015:2022
crs_projected <- 5070
lower48_dc <- c(setdiff(state.abb, c("AK", "HI")), "DC")

clean_names <- function(df) {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))
  df
}

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

employment_midpoints <- tibble(
  empszes = c("210", "212", "220", "230", "241", "242", "251", "252", "254", "260"),
  emp_midpoint = c(2.5, 2.5, 7, 14.5, 34.5, 74.5, 174.5, 374.5, 749.5, 1500)
)

retailer_categories <- c("online retailer", "local retailer", "warehouse")
estab_cols <- c("online_estab", "local_estab", "warehouse_estab")
emp_hat_cols <- c("online_emp_hat", "local_emp_hat", "warehouse_emp_hat")

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

message("Loading state and ZCTA geometries...")
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

zcta_points <- zcta_sf |>
  st_set_geometry(st_point_on_surface(st_geometry(zcta_sf))) |>
  st_join(states_sf |> select(STATEFP, STUSPS), left = TRUE) |>
  filter(!is.na(STATEFP)) |>
  transmute(
    zipcode,
    statefp = STATEFP,
    state_abbr = STUSPS
  )

zcta_sf <- zcta_sf |>
  semi_join(zcta_points |> st_drop_geometry(), by = "zipcode") |>
  left_join(zcta_points |> st_drop_geometry(), by = "zipcode")

message("Constructing internal state borders...")
state_pair_borders <- make_state_pair_borders(states_sf)

message("Identifying ZCTAs that touch internal state borders...")
border_zip_matches <- st_intersects(zcta_sf, state_pair_borders)
border_zip_sf <- zcta_sf[lengths(border_zip_matches) > 0, ]

border_zip_to_state_pair <- tibble(
  zipcode = rep(zcta_sf$zipcode, lengths(border_zip_matches)),
  border_index = unlist(border_zip_matches)
) |>
  mutate(state_pair_id = state_pair_borders$state_pair_id[border_index]) |>
  select(zipcode, state_pair_id) |>
  distinct()

message("Constructing cross-state adjacent ZIP pairs...")
border_touches <- st_touches(border_zip_sf)
zip_touch_pairs <- tibble(
  i = rep(seq_len(nrow(border_zip_sf)), lengths(border_touches)),
  j = unlist(border_touches)
) |>
  filter(i < j) |>
  transmute(
    zipcode_a = border_zip_sf$zipcode[i],
    zipcode_b = border_zip_sf$zipcode[j],
    state_a = border_zip_sf$state_abbr[i],
    state_b = border_zip_sf$state_abbr[j]
  ) |>
  filter(!is.na(state_a), !is.na(state_b), state_a != state_b) |>
  mutate(
    zip_pair_id = paste(pmin(zipcode_a, zipcode_b), pmax(zipcode_a, zipcode_b), sep = "-"),
    state_pair_id = paste(pmin(state_a, state_b), pmax(state_a, state_b), sep = "_")
  ) |>
  distinct(zip_pair_id, .keep_all = TRUE)

zip_pair_crosswalk_matched <- bind_rows(
  zip_touch_pairs |>
    transmute(
      zipcode = zipcode_a,
      zip_pair_id,
      neighbor_zipcode = zipcode_b,
      state_abbr = state_a,
      neighbor_state = state_b,
      state_pair_id
    ),
  zip_touch_pairs |>
    transmute(
      zipcode = zipcode_b,
      zip_pair_id,
      neighbor_zipcode = zipcode_a,
      state_abbr = state_b,
      neighbor_state = state_a,
      state_pair_id
    )
) |>
  arrange(zip_pair_id, zipcode)

unmatched_border_zips <- border_zip_sf |>
  st_drop_geometry() |>
  anti_join(
    zip_pair_crosswalk_matched |> distinct(zipcode),
    by = "zipcode"
  ) |>
  transmute(
    zipcode,
    zip_pair_id = NA_character_,
    neighbor_zipcode = NA_character_,
    state_abbr,
    neighbor_state = NA_character_,
    state_pair_id = NA_character_
  )

zipcode_pair_crosswalk <- bind_rows(
  zip_pair_crosswalk_matched,
  unmatched_border_zips
) |>
  arrange(zipcode, zip_pair_id)

write_csv(
  zipcode_pair_crosswalk,
  "data/temp/zipcode_pair_crosswalk.csv"
)

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
  filter(!is.na(state_abbr)) |>
  select(state_abbr, year, sales_tax) |>
  distinct()

cit <- read_xlsx("data/raw/us_state_corporate_tax.xlsx") |>
  rename(state_name = state_name, state_abbr = abbrev) |>
  mutate(across(starts_with("corporate_tax_"), ~ suppressWarnings(as.numeric(.x)))) |>
  pivot_longer(
    cols = starts_with("corporate_tax_"),
    names_to = "cit_year",
    values_to = "cit"
  ) |>
  mutate(year = as.integer(str_remove(cit_year, "^corporate_tax_"))) |>
  filter(!is.na(state_abbr)) |>
  select(state_abbr, year, cit) |>
  distinct()

retailer_raw <- read_csv(
  "data/temp/zbp_retailer_2015_2022.csv",
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names() |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    estab = as.numeric(estab),
    empszes = as.character(empszes)
  )

payroll_raw <- read_csv(
  "data/temp/zbp_payroll_2015_2022.csv",
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names()

if (!"emp" %in% names(payroll_raw)) {
  stop("zbp_payroll_2015_2022.csv does not include EMP. Re-run code/zip-estab_and_payroll-api.R.")
}

payroll <- payroll_raw |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    payann = as.numeric(payann),
    emp = as.numeric(emp)
  ) |>
  group_by(zipcode, year) |>
  summarise(
    payann = sum(payann, na.rm = TRUE),
    emp = sum(emp, na.rm = TRUE),
    .groups = "drop"
  )

all_sector_size_path <- "data/temp/zbp_all_sector_size_2015_2022.csv"
if (!file.exists(all_sector_size_path)) {
  stop("Missing ", all_sector_size_path, ". Re-run code/zip-estab_and_payroll-api.R.")
}

all_sector_size <- read_csv(
  all_sector_size_path,
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names() |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    estab = as.numeric(estab),
    empszes = as.character(empszes)
  )

missing_retailer_midpoints <- retailer_raw |>
  filter(empszes != "001", !is.na(estab), estab != 0) |>
  anti_join(employment_midpoints, by = "empszes") |>
  distinct(empszes, empszes_label)
if (nrow(missing_retailer_midpoints) > 0) {
  stop("Missing employment midpoint for retailer EMPSZES code(s): ", paste(missing_retailer_midpoints$empszes, collapse = ", "))
}

missing_all_sector_midpoints <- all_sector_size |>
  filter(empszes != "001", !is.na(estab), estab != 0) |>
  anti_join(employment_midpoints, by = "empszes") |>
  distinct(empszes, empszes_label)
if (nrow(missing_all_sector_midpoints) > 0) {
  stop("Missing employment midpoint for all-sector EMPSZES code(s): ", paste(missing_all_sector_midpoints$empszes, collapse = ", "))
}

all_sector_midpoint_emp <- all_sector_size |>
  filter(empszes != "001") |>
  left_join(employment_midpoints, by = "empszes") |>
  mutate(emp_hat_midpoint_total = estab * emp_midpoint) |>
  group_by(zipcode, year) |>
  summarise(emp_hat_midpoint_total = sum(emp_hat_midpoint_total, na.rm = TRUE), .groups = "drop")

employment_calibration <- payroll |>
  select(zipcode, year, emp_true_total = emp) |>
  left_join(all_sector_midpoint_emp, by = c("zipcode", "year")) |>
  mutate(
    emp_hat_midpoint_total = replace_na(emp_hat_midpoint_total, 0),
    emp_calibration_factor = case_when(
      emp_hat_midpoint_total > 0 & !is.na(emp_true_total) ~ emp_true_total / emp_hat_midpoint_total,
      emp_hat_midpoint_total == 0 & replace_na(emp_true_total, 0) == 0 ~ 1,
      TRUE ~ NA_real_
    )
  )

employment_calibration_diagnostics <- employment_calibration |>
  summarise(
    n_zip_years = n(),
    n_missing_factor = sum(is.na(emp_calibration_factor)),
    emp_calibration_factor_min = min(emp_calibration_factor, na.rm = TRUE),
    emp_calibration_factor_p50 = median(emp_calibration_factor, na.rm = TRUE),
    emp_calibration_factor_max = max(emp_calibration_factor, na.rm = TRUE)
  )

write_csv(
  employment_calibration_diagnostics,
  "data/temp/zipcode_emp_calibration_diagnostics_2015_2022.csv"
)

retailer_estab <- retailer_raw |>
  filter(empszes_label == "All establishments", naics %in% retailer_categories) |>
  mutate(
    naics = recode(
      naics,
      "online retailer" = "online_estab",
      "local retailer" = "local_estab",
      "warehouse" = "warehouse_estab"
    )
  ) |>
  group_by(zipcode, year, naics) |>
  summarise(estab = sum(estab, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = naics,
    values_from = estab,
    values_fill = 0
  )

for (estab_col in estab_cols) {
  if (!estab_col %in% names(retailer_estab)) retailer_estab[[estab_col]] <- 0
}

retailer_emp <- retailer_raw |>
  filter(empszes != "001", naics %in% retailer_categories) |>
  left_join(employment_midpoints, by = "empszes") |>
  mutate(
    emp_hat_raw = estab * emp_midpoint,
    naics = recode(
      naics,
      "online retailer" = "online_emp_hat",
      "local retailer" = "local_emp_hat",
      "warehouse" = "warehouse_emp_hat"
    )
  ) |>
  group_by(zipcode, year, naics) |>
  summarise(emp_hat_raw = sum(emp_hat_raw, na.rm = TRUE), .groups = "drop") |>
  left_join(
    employment_calibration |> select(zipcode, year, emp_calibration_factor),
    by = c("zipcode", "year")
  ) |>
  mutate(emp_hat = emp_hat_raw * coalesce(emp_calibration_factor, 1)) |>
  select(zipcode, year, naics, emp_hat) |>
  pivot_wider(
    names_from = naics,
    values_from = emp_hat,
    values_fill = 0
  )

for (emp_col in emp_hat_cols) {
  if (!emp_col %in% names(retailer_emp)) retailer_emp[[emp_col]] <- 0
}

retailer <- retailer_estab |>
  full_join(retailer_emp, by = c("zipcode", "year"))

for (estab_col in estab_cols) {
  if (!estab_col %in% names(retailer)) retailer[[estab_col]] <- 0
}
for (emp_col in emp_hat_cols) {
  if (!emp_col %in% names(retailer)) retailer[[emp_col]] <- 0
}

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

zipcode_market_potential <- read_csv(
  "data/temp/zipcode_market_potential_2015_2022.csv",
  col_types = cols(.default = col_guess(), zipcode = col_character())
) |>
  clean_names() |>
  mutate(
    year = as.integer(year),
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    zipcode_market_potential = as.numeric(zipcode_market_potential)
  ) |>
  select(zipcode, year, zipcode_market_potential)

message("Building ZIP-year-pair panel...")
zipcode_year_pair <- zipcode_pair_crosswalk |>
  crossing(year = years) |>
  left_join(retailer, by = c("zipcode", "year")) |>
  left_join(payroll, by = c("zipcode", "year")) |>
  left_join(population, by = c("zipcode", "year")) |>
  left_join(zipcode_market_potential, by = c("zipcode", "year")) |>
  left_join(sales_tax, by = c("state_abbr", "year")) |>
  left_join(cit, by = c("state_abbr", "year")) |>
  left_join(
    sales_tax |>
      rename(neighbor_state = state_abbr, tax_other = sales_tax),
    by = c("neighbor_state", "year")
  ) |>
  mutate(
    online_estab = replace_na(online_estab, 0),
    local_estab = replace_na(local_estab, 0),
    warehouse_estab = replace_na(warehouse_estab, 0),
    online_emp_hat = replace_na(online_emp_hat, 0),
    local_emp_hat = replace_na(local_emp_hat, 0),
    warehouse_emp_hat = replace_na(warehouse_emp_hat, 0),
    payann = replace_na(payann, 0),
    emp = replace_na(emp, 0),
    tax_diff = sales_tax - tax_other,
    high_tax_dummy = if_else(
      !is.na(sales_tax) & !is.na(tax_other),
      as.integer(sales_tax > tax_other),
      NA_integer_
    ),
    adoption_year_other = adoption_year_from_abbr(neighbor_state),
    post_wayfair_other = if_else(
      !is.na(adoption_year_other),
      as.integer(year >= adoption_year_other),
      NA_integer_
    )
  ) |>
  relocate(
    zipcode, year, zip_pair_id, neighbor_zipcode,
    state_abbr, neighbor_state, state_pair_id
  )

duplicate_keys <- zipcode_year_pair |>
  mutate(zip_pair_key = coalesce(zip_pair_id, "__UNMATCHED__")) |>
  count(zipcode, year, zip_pair_key) |>
  filter(n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("ZIP-year-pair data has duplicate zipcode-year-pair rows.")
}

write_csv(
  zipcode_year_pair,
  "data/temp/zipcode_year_pair_2015_2022.csv"
)

message("Writing diagnostics...")
pair_side_check <- zipcode_pair_crosswalk |>
  filter(!is.na(zip_pair_id)) |>
  count(zip_pair_id, name = "n_sides")

cross_state_violations <- zipcode_pair_crosswalk |>
  filter(!is.na(zip_pair_id), state_abbr == neighbor_state)

matched_zips <- zipcode_pair_crosswalk |>
  filter(!is.na(zip_pair_id)) |>
  distinct(zipcode)

unmatched_zips <- zipcode_pair_crosswalk |>
  filter(is.na(zip_pair_id)) |>
  distinct(zipcode)

zip_pair_multiplicity <- zipcode_pair_crosswalk |>
  filter(!is.na(zip_pair_id)) |>
  count(zipcode, name = "n_pairs")

zipcode_pair_diagnostics <- tibble(
  n_border_zips = n_distinct(border_zip_sf$zipcode),
  n_matched_zips = nrow(matched_zips),
  n_unmatched_zips = nrow(unmatched_zips),
  n_zip_pairs = n_distinct(na.omit(zipcode_pair_crosswalk$zip_pair_id)),
  n_directed_pair_rows = sum(!is.na(zipcode_pair_crosswalk$zip_pair_id)),
  n_pairs_with_exactly_two_sides = sum(pair_side_check$n_sides == 2),
  n_pairs_not_exactly_two_sides = sum(pair_side_check$n_sides != 2),
  n_cross_state_violations = nrow(cross_state_violations),
  max_pairs_per_zip = if_else(nrow(zip_pair_multiplicity) == 0, 0L, max(zip_pair_multiplicity$n_pairs)),
  n_zip_year_pair_rows = nrow(zipcode_year_pair),
  n_years = n_distinct(zipcode_year_pair$year),
  n_zipcode_market_potential_missing = sum(is.na(zipcode_year_pair$zipcode_market_potential)),
  n_high_tax_nonmissing_without_neighbor_tax = sum(
    is.na(zipcode_year_pair$tax_other) & !is.na(zipcode_year_pair$high_tax_dummy)
  )
)

write_csv(
  zipcode_pair_diagnostics,
  "data/temp/zipcode_pair_diagnostics.csv"
)

if (any(pair_side_check$n_sides != 2)) {
  stop("Some non-NA zip_pair_id values do not have exactly two directed rows.")
}
if (nrow(cross_state_violations) > 0) {
  stop("Some ZIP pairs are not cross-state.")
}
if (zipcode_pair_diagnostics$n_border_zips !=
    zipcode_pair_diagnostics$n_matched_zips + zipcode_pair_diagnostics$n_unmatched_zips) {
  stop("Border ZIP diagnostic identity failed.")
}
if (zipcode_pair_diagnostics$n_high_tax_nonmissing_without_neighbor_tax > 0) {
  stop("high_tax_dummy is non-missing where neighbor tax is missing.")
}

message("Done.")
message("Border ZIPs: ", zipcode_pair_diagnostics$n_border_zips)
message("ZIP pairs: ", zipcode_pair_diagnostics$n_zip_pairs)
message("Unmatched border ZIPs: ", zipcode_pair_diagnostics$n_unmatched_zips)
message("ZIP-year-pair rows: ", nrow(zipcode_year_pair))
