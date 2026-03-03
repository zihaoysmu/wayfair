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
options(tigris_use_cache = TRUE)

# import raw data ----
raw_cbp_2011 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp11co.txt")
raw_cbp_2012 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp12co.txt")
raw_cbp_2013 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp13co.txt")
raw_cbp_2014 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/cbp14co.txt")
raw_cbp_2015 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2015.CB1500CBP-Data.csv")
raw_cbp_2016 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2016.CB1600CBP-Data.csv")
raw_cbp_2017 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2017.CB1700CBP-Data.csv")
raw_cbp_2018 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2018.CB1800CBP-Data.csv")
raw_cbp_2019 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2019.CB1900CBP-Data.csv")
raw_cbp_2020 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2020.CB2000CBP-Data.csv")
raw_cbp_2021 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2021.CB2100CBP-Data.csv")
raw_cbp_2022 = read.csv("C:/document/SMU PhD/research/Data/County Business Pattern/CBP2022.CB2200CBP-Data.csv")
raw_pop = read_xlsx("C:/document/SMU PhD/research/Data/Census Population Estimates Program/co-est2020int-pop.xlsx")
raw_gdp = read.csv("../data/raw/county_gdp.csv")
raw_consumption = read.csv("../data/raw/consumption.csv")
raw_IRPD = read.csv("../data/raw/IRPD.csv")
raw_wayfair = read_xlsx("../data/raw/wayfair state implemetion timeline.xlsx")
raw_CFS_2012 = read.csv("C:/document/SMU PhD/research/sales tax and immigration/Data/CFS/2012/CFSAREA2012.CF1200A30-Data.csv")
raw_tax = read.csv("../data/temp/tax.csv")

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
name_map = raw_cbp_2015 %>%
  select(GEO_ID, NAME) %>%
  filter(!is.na(NAME)) %>%
  distinct()

year = 2015:2022

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
    complete(GEO_ID,YEAR, NAICS = "4541", fill = list(ESTAB = 0, EMP = "0")) %>% 
    filter(NAICS == "4541") %>% 
    select(GEO_ID, NAME, YEAR, NAICS, ESTAB, EMP) %>% 
    left_join(name_map, by = "GEO_ID", suffix = c("", "_map")) %>% 
    mutate(NAME = coalesce(NAME, NAME_map)) %>% 
    select(-NAME_map)
})

cbp_list <- Map(function(df, year) {
  df %>% mutate(YEAR = year)
}, cbp_list, year)

cbp = bind_rows(cbp_list)
cbp = na.omit(cbp)


cbp = cbp %>%
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
cbp = cbp %>% 
  mutate(EMP = ifelse(is.na(EMP), median(EMP, na.rm = TRUE), EMP))

rm(cbp_list)

# clean gdp ----
raw_gdp$GeoName = iconv(raw_gdp$GeoName, from = "", to = "UTF-8")
gdp = raw_gdp %>% 
  filter(LineCode == 1,
         grepl(",", GeoName)) %>% 
  pivot_longer(cols = "X2001":"X2024",
               names_to = "year",
               values_to = "gdp") %>% 
  mutate(year = as.numeric(sub("^X", "", year)),
         GeoFIPS = sub(" ", "", GeoFIPS)) %>% 
  select(GeoFIPS,year,gdp) %>% 
  rename(GEO_ID = GeoFIPS, YEAR = year)

# county distance ----
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

# 3) 找 600km 内邻居（返回 index 列表）
within_list <- st_is_within_distance(
  cty_pt, cty_pt,
  dist = units::set_units(600, "km")
)

# 4) index 长表
pairs_idx <- rbindlist(lapply(seq_along(within_list), function(i) {
  data.table(i = i, j = within_list[[i]])
}))
pairs_idx <- pairs_idx[i != j]

# 5) 只对这些 pairs 算距离（by_element=TRUE）
d_m <- st_distance(cty_pt[pairs_idx$i, ], cty_pt[pairs_idx$j, ], by_element = TRUE)

pairs <- data.table(
  GEOID_i = cty_pt$GEOID[pairs_idx$i],
  GEOID_j = cty_pt$GEOID[pairs_idx$j],
  dist_km = as.numeric(units::set_units(d_m, "km"))
)
rm(cty_poly, cty_pt, pairs_idx, d_m,within_list)


# county market access ----
market = pairs %>% 
  left_join(gdp %>% 
              pivot_wider(names_from = YEAR, values_from = gdp, names_prefix = "gdp_"), 
            by = c("GEOID_j" = "GEO_ID")) %>% 
  mutate(across(starts_with("gdp_"), ~ as.numeric(.x)),
         across(starts_with("gdp_"), ~ .x / dist_km, .names = "gdp_div_dist_{.col}")) %>% 
  select(GEOID_i,GEOID_j,gdp_div_dist_gdp_2015:gdp_div_dist_gdp_2022) %>% 
  group_by(GEOID_i) %>% 
  summarise(across(starts_with("gdp_div_dist_gdp_"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>% 
  filter(gdp_div_dist_gdp_2015 != 0) %>% 
  rename_with(~ paste0("ma_", sub("gdp_div_dist_gdp_", "", .x)),
              starts_with("gdp_div_dist_gdp_")) %>% 
  pivot_longer(cols = ma_2015:ma_2022,
               names_to = "year",
               values_to = "ma") %>% 
  mutate(year = sub("ma_", "", year),
         year = as.numeric(year))

cbp = cbp %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1))







# clean state consumption + tax + wayfair timeline ----
wayfair = raw_wayfair %>% 
  mutate(year = strsplit(wayfair_time, "-") %>% sapply(`[`,1),
         month = strsplit(wayfair_time, "-") %>% sapply(`[`,2),
         wyear = as.numeric(year),
         wmonth = as.numeric(month)) %>% 
  select(-wayfair_time, -year, -month)
wayfair = wayfair[-(47:49),]
df = data.frame(
  state = c("Alaska", "Delaware", "Montana", "New Hampshire", "Oregon"),
  wayfair_year = c(0,0,0,0,0),
  wyear = c(0,0,0,0,0),
  wmonth = c(0,0,0,0,0)
)
wayfair = bind_rows(wayfair, df)

consumption = raw_consumption %>% 
  pivot_longer(cols = c("X2012":"X2024"),
               names_to = "year",
               values_to = "con") %>% 
  select(-GeoFIPS) %>% 
  rename(state = GeoName)
consumption$year = str_remove(consumption$year, "^X")

IRPD = raw_IRPD %>% 
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
  ungroup()


# output ----
write.csv(cbp, "../data/temp/cbp_temp.csv", row.names = FALSE)
write.csv(gdp, "../data/temp/gdp_temp.csv", row.names = FALSE)
write.csv(market, "../data/temp/market_temp.csv", row.names = FALSE)
write.csv(main, "../data/temp/state_con_tax.csv", row.names = FALSE)
