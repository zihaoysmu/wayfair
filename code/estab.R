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

# clean data before 2015 ----
name_map2 = raw_cbp_2015 %>%
  select(GEO_ID, NAME) %>%
  filter(!is.na(NAME)) %>%
  distinct() %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1))
name_map2 = name_map2[-1,]

cbp_2011 = raw_cbp_2011 %>% 
  mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
  complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
  mutate(YEAR = 2011) %>% 
  filter(naics == "4541//") %>% 
  rename(NAICS = naics, ESTAB = est) %>% 
  select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
  left_join(name_map2, by = "GEO_ID")

cbp_2012 = raw_cbp_2012 %>% 
  mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
  complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
  mutate(YEAR = 2012) %>% 
  filter(naics == "4541//") %>% 
  rename(NAICS = naics, ESTAB = est) %>% 
  select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
  left_join(name_map2, by = "GEO_ID")

cbp_2013 = raw_cbp_2013 %>% 
  mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
  complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
  mutate(YEAR = 2013) %>% 
  filter(naics == "4541//") %>% 
  rename(NAICS = naics, ESTAB = est) %>% 
  select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
  left_join(name_map2, by = "GEO_ID")

cbp_2014 = raw_cbp_2014 %>% 
  mutate(GEO_ID = sprintf("%02d%03d", fipstate, fipscty)) %>% 
  complete(GEO_ID, naics = "4541//", fill = list(est = 0)) %>% 
  mutate(YEAR = 2014) %>% 
  filter(naics == "4541//") %>% 
  rename(NAICS = naics, ESTAB = est) %>% 
  select(NAICS, YEAR, ESTAB, GEO_ID) %>% 
  left_join(name_map2, by = "GEO_ID")



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
  complete(GEO_ID,YEAR, NAICS = "4541", fill = list(ESTAB = 0)) %>% 
  filter(NAICS == "4541") %>% 
  select(GEO_ID, NAME, YEAR, NAICS, ESTAB) %>% 
  left_join(name_map, by = "GEO_ID", suffix = c("", "_map")) %>% 
  mutate(NAME = coalesce(NAME, NAME_map)) %>% 
  select(-NAME_map)
})

cbp_list <- Map(function(df, year) {
  df %>% mutate(YEAR = year)
}, cbp_list, year)

cbp = bind_rows(cbp_list)
cbp = na.omit(cbp)

# mark border county ----
options(tigris_use_cache = TRUE)

# 1) 读 county 边界（建议用 cartographic boundary 更轻）
cty <- counties(cb = TRUE, year = 2022, class = "sf") %>%
  st_transform(5070) %>%  # US Albers，适合邻接/距离判断
  select(GEOID, STATEFP, NAME)

# 2) 建立邻接关系（touches: 共享边或点）
nb <- st_touches(cty)  # list: 每个 county 的邻居 index

# 3) 判断是否存在“跨州邻居”
boundary <- vapply(seq_len(nrow(cty)), function(i) {
  nbr <- nb[[i]]
  if (length(nbr) == 0) return(FALSE)
  any(cty$STATEFP[nbr] != cty$STATEFP[i])
}, logical(1))

cty$is_boundary_county = boundary

# border county graph
# ggplot(cty) +
#   geom_sf(aes(fill = is_boundary_county), color = NA) +
#   scale_fill_manual(
#     values = c("FALSE" = "grey85", "TRUE" = "red"),
#     labels = c("Interior county", "Boundary county"),
#     name = ""
#   ) +
#   theme_void() +
#   labs(title = "Boundary Counties in the United States")


cbp_border = cbp %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1)) %>% 
  left_join(cty %>% select(GEOID,is_boundary_county, STATEFP), by = c("GEO_ID" = "GEOID")) %>% 
  filter(STATEFP != 72) %>% 
  mutate(lest = asinh(ESTAB))



# DiD ----

cbp_border = cbp_border %>% 
  mutate(time = YEAR-2019,
         treat = as.integer(is_boundary_county))

did = feols(
   ESTAB~ i(time, treat, ref = -1) | GEO_ID + YEAR,
  data = cbp_border,
  cluster = ~STATEFP
)

# plot ----
# plot = cbp_border %>% 
#   filter(is_boundary_county == TRUE) %>% 
#   group_by(YEAR) %>% 
#   summarize(sum = sum(ESTAB))
# 
# ggplot(data = plot, aes(x = YEAR, y = sum))+
#   geom_point()
# 
# nevada = cbp_border %>% 
#   filter(STATEFP == 32, is_boundary_county == 1) %>% 
#   group_by(YEAR) %>% 
#   summarise(sum = sum(ESTAB))
# 
# ggplot(data = nevada, aes(x = YEAR, y = sum))+
#   geom_point()

# combine population ----
pop = raw_pop
pop$NAME = str_remove(pop$NAME, "^\\.")

cbp_border = cbp_border %>% 
  left_join(pop %>% select(NAME, `2018pop`), by = "NAME") %>% 
  drop_na() %>% 
  mutate(small = ifelse(`2018pop` < quantile(`2018pop`,0.9), 1, 0),
         small2 = ifelse(`2018pop` < quantile(`2018pop`,0.5), 1, 0),
         small3 = ifelse(`2018pop` < 50000, 1, 0),
         treat_pop = treat*small,
         treat_pop2 = treat*small2,
         treat_pop3 = treat*small3,
         post = ifelse(time>=0, 1, 0),
         time2 = time+5)

test = cbp_border %>%
  select(GEO_ID, NAME, YEAR, ESTAB, treat, small, treat_pop) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = ESTAB,
    names_prefix = "ESTAB_"
  ) %>% 
  mutate(dif = ESTAB_2020 - ESTAB_2017) %>% 
  mutate(negative_change = ifelse(dif<0,1,0))
summary(test$negative_change)
summary(test %>% filter(treat_pop == 1) %>% pull(negative_change))
summary(test %>% filter(treat_pop == 0) %>% pull(negative_change))

did_pop = feols(
  ESTAB~ i(time, treat_pop, ref = -1)| GEO_ID + YEAR,
  data = cbp_border,
  cluster = ~STATEFP
)

iplot(did_pop)

did_trend = feols(
  ESTAB ~ treat_pop:time2+ treat_pop:time2^2 + treat_pop:post:time2
  | GEO_ID + YEAR,
  cluster = ~STATEFP,
  data = test
)
did_trend

# when post is >=0, time trend dif at t=1 is -2.2+2*0.2 = -1.8, at t=2 is -1.4, at t=3 is -1, at t=4 is -0.6, 
# at t=5 is -2.2+1.16-0.035*2*5=-1.39, t = 6 is -1.49

# export did parallel trend graph ----
png("../output/did_pop_iplot.png", width = 800, height = 600, res = 150)
iplot(did_pop, main = "Effect on Establishment Number\nTreatment = 1 (border county) * 1 (pop < 10th_pop)")
dev.off()

png("../output/did_iplot.png", width = 800, height = 600, res = 150)
iplot(did, main = "Effect on Establishment Number\nTreatment = 1 (border county)")
dev.off()

png("../output/did_pop_trend_iplot.png", width = 800, height = 600, res = 150)
iplot(did_pop_trend, main = "Effect on the Trend of Establishment Number\nTreatment = 1 (border county) * 1 (pop < 10th_pop)")
dev.off()

etable(did_trend, file = "../output/did_trend.tex")

# if using asinh, the coefficient is between 0.1 - 0.2. Most counties 

# establishement graph ----
options(tigris_use_cache = TRUE)

counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>% 
  rename(GEO_ID = GEOID)
states_sf   <- states(cb = TRUE, resolution = "20m", year = 2020)

exclude <- c("02", "15", "60", "66", "69", "72", "78")

counties_sf <- counties_sf %>%
  filter(!STATEFP %in% exclude)

states_sf <- states_sf %>%
  filter(!STATEFP %in% exclude)

map_df_16 <- counties_sf %>%
  left_join(cbp_border, by = "GEO_ID") %>% 
  mutate(group = cut(
    ESTAB,
    breaks = c(0, 10, 100, Inf),
    labels = c("1–10", "10–100", "100+")
  )) %>% 
  filter(YEAR == 2016,small == 1)

map_df_17 <- counties_sf %>%
  left_join(cbp_border, by = "GEO_ID") %>% 
  mutate(group = cut(
    ESTAB,
    breaks = c(0, 10, 100, Inf),
    labels = c("1–10", "10–100", "100+")
  )) %>% 
  filter(YEAR == 2017,small == 1)

map_df_22 <- counties_sf %>%
  left_join(cbp_border, by = "GEO_ID") %>% 
  mutate(group = cut(
    ESTAB,
    breaks = c(0, 10, 100, Inf),
    labels = c("1–10", "10–100", "100+")
  )) %>% 
  filter(YEAR == 2022, small == 1)

ggplot() +
  geom_sf(data = map_df_16,
          aes(fill = group),
          color = "white",
          size = 0.05) +
  geom_sf(data = states_sf,
          fill = NA,
          color = "black",
          size = 0.5) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Number of Establishements in Online Shopping Industry")+
  theme_void()

ggplot() +
  geom_sf(data = map_df_17,
          aes(fill = group),
          color = "white",
          size = 0.05) +
  geom_sf(data = states_sf,
          fill = NA,
          color = "black",
          size = 0.5) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Number of Establishements in Online Shopping Industry, 2017")+
  theme_void()

ggsave("../output/estab_17.png")

ggplot() +
  geom_sf(data = map_df_22,
          aes(fill = group),
          color = "white",
          size = 0.05) +
  
  geom_sf(data = states_sf,
          fill = NA,
          color = "black",
          size = 0.5) +
  
  scale_fill_brewer(palette = "YlOrRd") +
  
  theme_void()
ggsave("../output/estab_22.png")
