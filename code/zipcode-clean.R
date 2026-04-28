source("code/setup.R")

zbp_4541_2015_2022 <- readr::read_csv(
  "data/temp/zbp_4541_2015_2022.csv",
  col_types = readr::cols(
    GEO_ID = readr::col_character(),
    YEAR = readr::col_integer(),
    ESTAB = readr::col_double(),
    EMPSZES = readr::col_character(),
    zipcode = readr::col_character()
  )
)

# get the ZCTA data (the polygon of ZIP code)
zcta <- zctas(cb = TRUE, year = 2017) %>%
  st_transform(5070) %>%
  mutate(zipcode = ZCTA5CE10)

# clean zipcode in ZBP
zbp_clean <- zbp_4541_2015_2022 %>%
  mutate(
    zipcode = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    year = as.integer(year),
    estab = as.numeric(estab)
  )

# combine ZCTA polygons with ZBP data
zbp <- zcta %>%
  left_join(zbp_clean, by = "zipcode")

# state and nation boundaries
states_sf <- states(cb = TRUE, year = 2017) %>%
  st_transform(5070)
nation_sf <- nation(year = 2017) %>%
  st_transform(5070)

# plot the ZCTA-level establishments for NAICS 4541 in 2017
ggplot(zbp %>% filter(year == 2017)) +
  geom_sf(aes(fill = log1p(estab)), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = nation_sf, fill = NA, color = "black", size = 0.4) +
  labs(
    title = "ZCTA-level establishments, NAICS 4541, 2017",
    fill = "log(1 + establishments)"
  ) +
  theme_void()

# ---- ZCTAs with establishments that lie on a state border (2017) ----

# interior state borders = all state edges minus the national outline/coast
all_state_edges <- states_sf %>% st_boundary() %>% st_union()
nation_edge     <- nation_sf %>% st_boundary() %>% st_union()
interior_state_borders <- st_difference(all_state_edges, st_buffer(nation_edge, 100))

# all ZCTAs (universe of ZIP polygons)
zcta$on_state_border <- lengths(st_intersects(zcta, interior_state_borders)) > 0

cat("Total ZCTAs:                             ", nrow(zcta), "\n")
cat("  of which on an interior state border: ", sum(zcta$on_state_border), "\n")
cat("  share on state border:                ",
    round(mean(zcta$on_state_border) * 100, 2), "%\n")

# ZIPs in the ZBP file (NAICS 4541, 2017) that have establishments
zbp_2017 <- zbp %>%
  filter(year == 2017, !is.na(estab), estab > 0)

zbp_2017$on_state_border <- lengths(st_intersects(zbp_2017, interior_state_borders)) > 0

cat("2017 ZIPs with >=1 establishment:        ", nrow(zbp_2017), "\n")
cat("  of which on an interior state border: ", sum(zbp_2017$on_state_border), "\n")
cat("  share on state border:                ",
    round(mean(zbp_2017$on_state_border) * 100, 2), "%\n")

# analyze the distribution of ZCTA area
zcta_area <- zcta %>%
mutate(
area_sqkm = as.numeric(st_area(geometry)) / 1e6
)

summary(zcta_area$area_sqkm)

ggplot(zcta_area) +
geom_histogram(aes(x = area_sqkm), bins = 100) +
scale_x_log10() +
labs(
x = "ZCTA area, square kilometers",
y = "Count",
title = "Distribution of ZCTA Area"
) +
theme_minimal()
