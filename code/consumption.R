# library ----
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
library(contdid)


# import raw ----
raw_consumption = read.csv("../data/raw/consumption.csv")
raw_IRPD = read.csv("../data/raw/IRPD.csv")
raw_wayfair = read_xlsx("../data/raw/wayfair state implemetion timeline.xlsx")
raw_CFS_2012 = read.csv("C:/document/SMU PhD/research/sales tax and immigration/Data/CFS/2012/CFSAREA2012.CF1200A30-Data.csv")
raw_tax = read.csv("../data/temp/tax.csv")

# clean ----
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

# Staggered DiD ----
did = feols(
  rcon ~ sunab(wayfair_year, year, ref.p = -1) | state + year,
  data = main,
  cluster = ~state
)
iplot(did)



# Staggered Continuous DiD ----
cd_res <- cont_did(
  yname = "rcon",
  tname = "year",
  idname = "ID",
  dname = "expo",
  data = main,２
  gname = "wayfair_year",
  target_parameter = "slope",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 100,
  cband = TRUE,
  num_knots = 0,
  degree = 1,
)
ggcont_did(cd_res)
