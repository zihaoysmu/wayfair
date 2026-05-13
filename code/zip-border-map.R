library(dplyr)
library(readr)
library(sf)
library(tigris)
library(ggplot2)
library(data.table)

options(tigris_use_cache = TRUE)
dir.create("data/temp/tigris_cache", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
options(
  tigris_cache_dir = normalizePath("data/temp/tigris_cache", winslash = "/")
)

sf::sf_use_s2(FALSE)

crs_projected <- 5070
lower48_dc <- c(setdiff(state.abb, c("AK", "HI")), "DC")

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

    st_sf(
      state_a = state_a,
      state_b = state_b,
      state_pair_id = paste(state_a, state_b, sep = "_"),
      geometry = st_sfc(st_union(st_geometry(geom)), crs = st_crs(states_sf))
    )
  })

  do.call(rbind, border_list)
}

states_sf <- states(cb = TRUE, year = 2022) |>
  filter(STUSPS %in% lower48_dc) |>
  st_make_valid() |>
  st_transform(crs_projected)

zcta_sf <- zctas(cb = TRUE, year = 2020) |>
  st_make_valid() |>
  st_transform(crs_projected) |>
  mutate(zipcode = coalesce(as.character(.data$ZCTA5CE20), as.character(.data$GEOID20))) |>
  select(zipcode)

state_pair_borders <- make_state_pair_borders(states_sf)

border_zcta_sf <- zcta_sf[
  lengths(st_intersects(zcta_sf, state_pair_borders)) > 0,
]

border_zipcodes <- border_zcta_sf |>
  st_drop_geometry() |>
  distinct(zipcode)

write_csv(
  border_zipcodes,
  "data/temp/zcta_touching_state_border.csv"
)

border_zip_map <- ggplot() +
  geom_sf(
    data = states_sf,
    fill = "grey96",
    color = "white",
    linewidth = 0.15
  ) +
  geom_sf(
    data = border_zcta_sf,
    fill = "#2c7fb8",
    color = NA,
    alpha = 0.85
  ) +
  geom_sf(
    data = states_sf,
    fill = NA,
    color = "grey35",
    linewidth = 0.18
  ) +
  coord_sf(crs = st_crs(crs_projected), datum = NA) +
  labs(
    title = "ZIP/ZCTA Areas Touching State Borders",
    subtitle = "All ZIP/ZCTA polygons that intersect an internal state border",
    caption = paste0("Number of ZIP/ZCTA areas: ", nrow(border_zipcodes))
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.02),
    plot.subtitle = element_text(size = 10, hjust = 0.02, margin = margin(b = 8)),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0.98),
    plot.margin = margin(10, 12, 10, 12)
  )

ggsave(
  "output/figures/zipcode_touching_state_border_map.png",
  border_zip_map,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "output/figures/zipcode_touching_state_border_map.pdf",
  border_zip_map,
  width = 12,
  height = 8
)

message("Saved output/figures/zipcode_touching_state_border_map.png")
message("Saved output/figures/zipcode_touching_state_border_map.pdf")
message("Saved data/temp/zcta_touching_state_border.csv")
