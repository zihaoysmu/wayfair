library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(gt)
library(psych)
library(tibble)
library(fixest)
library(sf)
library(tigris)
library(ggplot2)
library(stringr)
library(aod)
library(units)
library(data.table)
library(languageserver)
options(tigris_use_cache = TRUE)

# import raw data ----
raw_cbp_2011 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp11co.txt")
raw_cbp_2012 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp12co.txt")
raw_cbp_2013 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp13co.txt")
raw_cbp_2014 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp14co.txt")
raw_cbp_2015 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2015.CB1500CBP-Data.csv")
raw_cbp_2016 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2016.CB1600CBP-Data.csv")
raw_cbp_2017 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2017.CB1700CBP-Data.csv")
raw_cbp_2018 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2018.CB1800CBP-Data.csv")
raw_cbp_2019 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2019.CB1900CBP-Data.csv")
raw_cbp_2020 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2020.CB2000CBP-Data.csv")
raw_cbp_2021 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2021.CB2100CBP-Data.csv")
raw_cbp_2022 <- read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2022.CB2200CBP-Data.csv")
raw_pop <- read_xlsx("C:/document/SMU PhD/research/Data/Census Population Estimates Program/2010-2025 county pop.xlsx")
raw_gdp <- read.csv("data/raw/county_gdp.csv")
raw_consumption <- read.csv("data/raw/consumption.csv")
raw_IRPD <- read.csv("data/raw/IRPD.csv")
raw_wayfair <- read_xlsx("data/raw/wayfair state implemetion timeline.xlsx")
raw_CFS_2012 <- read.csv("C:/document/SMU PhD/research/sales tax and immigration/Data/CFS/2012/CFSAREA2012.CF1200A30-Data.csv")
raw_tax <- read.csv("data/temp/tax.csv")
raw_tax_full <- read_xlsx("data/raw/combined sales tax.xlsx")

# clean data before 2015 ----
# # 加入更早的数据 does not add new info or conclusion
# name_map2 = raw_cbp_2015 %>%
#   select(GEO_ID, NAME) %>%
#   filter(!is.na(NAME)) %>%
#   distinct() %>% 
#   mutate(GEO_ID = str_sub(GEO_ID, -5, -1))
# name_map2 = name_map2[-1,]
# 
# cbp_2011 = raw_cbp_2011 %>% 
#   mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
#   complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
#   mutate(YEAR = 2011) %>% 
#   filter(naics == "4541//") %>% 
#   rename(NAICS = naics, ESTAB = est) %>% 
#   select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
#   left_join(name_map2, by = "GEO_ID")
# 
# cbp_2012 = raw_cbp_2012 %>% 
#   mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
#   complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
#   mutate(YEAR = 2012) %>% 
#   filter(naics == "4541//") %>% 
#   rename(NAICS = naics, ESTAB = est) %>% 
#   select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
#   left_join(name_map2, by = "GEO_ID")
# 
# cbp_2013 = raw_cbp_2013 %>% 
#   mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
#   complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
#   mutate(YEAR = 2013) %>% 
#   filter(naics == "4541//") %>% 
#   rename(NAICS = naics, ESTAB = est) %>% 
#   select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
#   left_join(name_map2, by = "GEO_ID")
# 
# cbp_2014 = raw_cbp_2014 %>% 
#   mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
#   complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
#   mutate(YEAR = 2014) %>% 
#   filter(naics == "4541//") %>% 
#   rename(NAICS = naics, ESTAB = est) %>% 
#   select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
#   left_join(name_map2, by = "GEO_ID")
# 
# rm(name_map,name_map2)

# construct index ----
name_map <- raw_cbp_2015 %>%
  select(GEO_ID, NAME) %>%
  filter(!is.na(NAME)) %>%
  distinct()

year <- 2015:2022

# clean cbp ----
cbp_list <- list(
  raw_cbp_2015,
  raw_cbp_2016,
  raw_cbp_2017,
  raw_cbp_2018,
  raw_cbp_2019,
  raw_cbp_2020,
  raw_cbp_2021,
  raw_cbp_2022
)


cbp_list <- lapply(cbp_list, function(df) {
  df %>% mutate(ESTAB = as.numeric(ESTAB)) %>% 
    rename_with(~ "NAICS", matches("^NAICS20[0-9]{2}$")) %>%
    rename_with(~ "NAICS_LABEL", matches("^NAICS20[0-9]{2}_LABEL$")) %>% 
    filter(EMPSZES_LABEL == "All establishments") %>% 
    complete(GEO_ID,YEAR, NAICS = c("4541", "4421", "4431", "4451", "4481", "4511", "4512", "4532"), fill = list(ESTAB = 0, EMP = "0")) %>% 
    filter(NAICS %in% c("4541", "4421", "4431", "4451", "4481", "4511", "4512", "4532")) %>% # Add other retail sectors for placebo test
    mutate(NAICS = ifelse(NAICS == "4541", "4541", "0")) %>% 
    select(GEO_ID, NAME, YEAR, NAICS, ESTAB, EMP) %>% 
    left_join(name_map, by = "GEO_ID", suffix = c("", "_map")) %>% 
    mutate(NAME = coalesce(NAME, NAME_map)) %>% 
    select(-NAME_map)
})

cbp_list <- Map(function(df, year) {
  df %>% mutate(YEAR = year)
}, cbp_list, year)

cbp <- bind_rows(cbp_list)
cbp <- na.omit(cbp)

cbp <- cbp %>%
  mutate(
    EMP =
      case_when(
        str_detect(EMP, "employees") ~
          (
            as.numeric(str_extract(EMP, "^\\d+")) +
              as.numeric(str_extract(EMP, "(?<=to )\\d+"))
          ) / 2,
        TRUE ~ as.numeric(EMP)
      )
  )
cbp <- cbp %>% 
  group_by(GEO_ID, YEAR, NAICS) %>% 
  summarise(ESTAB = sum(ESTAB), EMP = sum(EMP), .groups = "drop")
cbp <- cbp %>% 
  mutate(EMP = ifelse(is.na(EMP), median(EMP, na.rm = TRUE), EMP))

rm(cbp_list)

# clean gdp ----
raw_gdp$GeoName = iconv(raw_gdp$GeoName, from = "", to = "UTF-8")
gdp <- raw_gdp %>% 
  filter(LineCode == 1,
         grepl(",", GeoName)) %>% 
  pivot_longer(cols = "X2001":"X2024",
               names_to = "year",
               values_to = "gdp") %>% 
  mutate(year = as.numeric(sub("^X", "", year)),
         GeoFIPS = sub(" ", "", GeoFIPS)) %>% 
  select(GeoFIPS,year,gdp) %>% 
  rename(GEO_ID = GeoFIPS, YEAR = year)

# clean tax_full ----
data(fips_codes)
state_crosswalk <- fips_codes %>%
  distinct(state_name, state_code)

tax_full = raw_tax_full %>% 
  rename(state = GEO_ID) %>% 
  left_join(state_crosswalk, by = c("state" = "state_name")) %>% 
  mutate(across(starts_with("tax"), as.numeric)) %>% 
  pivot_longer(cols = tax_2012:tax_2022, names_to = "year", values_to = "tax") %>%
  mutate(year = sub("tax_", "", year)) %>% 
  drop_na()
  



# clean state consumption + tax + wayfair timeline ----
wayfair <- raw_wayfair %>% 
  mutate(year = strsplit(wayfair_time, "-") %>% sapply(`[`,1),
         month = strsplit(wayfair_time, "-") %>% sapply(`[`,2),
         wyear = as.numeric(year),
         wmonth = as.numeric(month)) %>% 
  select(-wayfair_time, -year, -month)
wayfair <- wayfair[-(47:49),]
df <- data.frame(
  state = c("Alaska", "Delaware", "Montana", "New Hampshire", "Oregon"),
  wayfair_year = c(0,0,0,0,0),
  wyear = c(0,0,0,0,0),
  wmonth = c(0,0,0,0,0)
)
wayfair <- bind_rows(wayfair, df)

consumption <- raw_consumption %>% 
  pivot_longer(cols = c("X2012":"X2024"),
               names_to = "year",
               values_to = "con") %>% 
  select(-GeoFIPS) %>% 
  rename(state = GeoName)
consumption$year = str_remove(consumption$year, "^X")

IRPD <- raw_IRPD %>% 
  pivot_longer(cols = c("X2012":"X2024"),
               names_to = "year",
               values_to = "IRPD") %>% 
  select(-GeoFIPS) %>% 
  rename(state = GeoName)
IRPD$year = str_remove(IRPD$year, "^X")

expo = raw_CFS_2012 %>% 
  filter(VAL != "Value ($ Millions)")
expo$VAL[expo$VAL == "Z"] = 0
expo$VAL[expo$VAL == "S"] = NA
expo$VAL = as.numeric(expo$VAL)
expo = expo %>% 
  filter(str_detect(GEO_ID, "^04"),
         str_detect(DDESTGEO, "^04"),
         DMODE == "001",
         NAICS2007 == "4541") %>% 
  select(DDESTGEO, GEO_ID, NAME, DDESTGEO_LABEL, NAICS2007, NAICS2007_LABEL, VAL) %>%  # lots of NA data for 4541 trade
  mutate(VAL = ifelse(is.na(VAL), 0, VAL)) %>%  # NA 替换为0
  group_by(DDESTGEO_LABEL) %>% 
  summarise(online = sum(VAL)) %>% 
  rename(state = DDESTGEO_LABEL)

tax = raw_tax %>% 
  filter(year == 2017) %>% 
  select(DDESTGEO_LABEL, tax) %>% 
  rename(state = DDESTGEO_LABEL)

main = consumption %>% 
  left_join(IRPD, by = c("state", "year")) %>% 
  left_join(wayfair, by = "state") %>% 
  mutate(rcon = 100*con/IRPD,
         treat = ifelse(wayfair_year > 0, 1, 0),
         year = as.numeric(year),
         time = year - wyear,
         wayfair_year = ifelse(wayfair_year == 0, 0, wayfair_year),
         time = ifelse(time > 100, -9999, time),
         ID = as.numeric(as.factor(state))) %>% 
  drop_na() %>% 
  left_join(tax, by = "state") %>% 
  left_join(expo, by = "state") %>% 
  mutate(ronline = 100*online/IRPD,
         expo = tax*ronline / rcon) %>% 
  group_by(state) %>% 
  mutate(expo = expo[year == 2012]) %>% 
  ungroup() %>% 
  left_join(state_crosswalk, by = c("state" = "state_name"))



# construct county distance ----
sf::sf_use_s2(TRUE)

# 1) county polygon
cty_poly <- tigris::counties(cb = TRUE, year = 2022) %>%
  st_transform(4326) %>%
  select(GEOID)

# 2) 用 polygon 生成点（更稳：point_on_surface）
cty_pt <- cty_poly %>%
  st_transform(5070) %>%                      # NAD83 / Conus Albers（米）
  st_set_geometry(st_point_on_surface(st_geometry(.))) %>%
  st_transform(4326)                          # 如果后面你想用球面距离，可再转回 4326

# 3) 找 1000km 内邻居（返回 index 列表）
within_list <- st_is_within_distance(
  cty_pt, cty_pt,
  dist = units::set_units(1000, "km")
)

# 4) index 长表
pairs_idx <- rbindlist(lapply(seq_along(within_list), function(i) {
  data.table(i = i, j = within_list[[i]])
}))
pairs_idx <- pairs_idx[i != j]

# 5) 只对这些 pairs 算距离（by_element=TRUE）
d_m <- st_distance(cty_pt[pairs_idx$i, ], cty_pt[pairs_idx$j, ], by_element = TRUE)

# unit is in km
pairs <- data.table(
  GEOID_i = cty_pt$GEOID[pairs_idx$i],
  GEOID_j = cty_pt$GEOID[pairs_idx$j],
  dist_km = as.numeric(units::set_units(d_m, "km"))
)

# use the formula in Hanson (2005), alpha_2 = −1
pairs <- pairs %>%
  mutate(
    dist_km_new = exp(dist_km * (-1) * (0.001))
  )
rm(cty_poly, cty_pt, pairs_idx, d_m, within_list)


# construct county market access ----

# note: ma does not include county's own gdp

market = pairs %>%
  mutate(
    state_i = str_sub(GEOID_i, 1, 2),
    state_j = str_sub(GEOID_j, 1, 2),
    same_state = (state_i == state_j)
  ) %>%
  left_join(
    gdp %>%
      pivot_wider(names_from = YEAR, values_from = gdp, names_prefix = "gdp_"),
    by = c("GEOID_j" = "GEO_ID")
  ) %>%
  mutate(across(starts_with("gdp_"), as.numeric)) %>%
  pivot_longer(
    cols = starts_with("gdp_"),
    names_to = "year",
    values_to = "gdp_j"
  ) %>% 
  mutate(year = as.numeric(sub("gdp_", "", year)))
market = market %>% 
  left_join(tax_full %>% 
              mutate(year = as.numeric(year)) %>% 
              select(state_code, tax, year), by = c("state_j" = "state_code", "year" = "year"))

market = market %>%   
  mutate(
    gdp_j = replace_na(gdp_j, 0),
    gdp_j_tax = gdp_j * tax,
    ma_contrib_new = gdp_j * dist_km_new,
    ma_contrib = gdp_j / dist_km,
    ma_tax_contrib_new = ma_contrib_new * tax,
    ma_tax_contrib = ma_contrib * tax
  ) %>% 
  group_by(GEOID_i, year, same_state) %>%
  summarise(
    gdp_sum = sum(gdp_j, na.rm = TRUE),
    gdp_tax_sum = sum(gdp_j_tax, na.rm = TRUE),
    ma_sum_new  = sum(ma_contrib_new, na.rm = TRUE),
    ma_tax_sum_new = sum(ma_tax_contrib_new, na.rm = TRUE),
    ma_sum = sum(ma_contrib, na.rm = TRUE),
    ma_tax_sum = sum(ma_tax_contrib, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type = if_else(same_state, "in", "out")) %>%
  select(GEOID_i, year, type, gdp_sum, gdp_tax_sum, ma_sum_new, ma_tax_sum_new, ma_sum, ma_tax_sum)

market = market %>% 
  pivot_wider(
    names_from = type,
    values_from = c(gdp_sum, gdp_tax_sum, ma_sum_new, ma_tax_sum_new, ma_sum, ma_tax_sum),
    values_fill = 0
  ) %>%
  rename(
    gdp_in  = gdp_sum_in,
    gdp_out = gdp_sum_out,
    gdp_tax_in = gdp_tax_sum_in,
    gdp_tax_out = gdp_tax_sum_out,
    ma_in_new   = ma_sum_new_in,
    ma_out_new  = ma_sum_new_out,
    ma_tax_in_new = ma_tax_sum_new_in,
    ma_tax_out_new = ma_tax_sum_new_out,
    ma_in   = ma_sum_in,
    ma_out = ma_sum_out,
    ma_tax_in = ma_tax_sum_in,
    ma_tax_out = ma_tax_sum_out
  ) %>% 
  filter((ma_in + ma_out) != 0) 

cbp = cbp %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1))

# output ----
write.csv(cbp, "data/temp/cbp_temp.csv", row.names = FALSE)
write.csv(gdp, "data/temp/gdp_temp.csv", row.names = FALSE)
write.csv(market, "data/temp/market_temp.csv", row.names = FALSE)
write.csv(main, "data/temp/state_con_tax.csv", row.names = FALSE)

# pairs オブジェクトが残っていれば
pairs %>%
    filter(dist_km < 1) %>%
    arrange(dist_km)
