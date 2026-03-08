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
library(units)
library(data.table)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggrepel)
options(tigris_use_cache = TRUE)



# import raw data ----
gdp = read.csv("../data/temp/gdp_temp.csv", colClasses = c(GEO_ID = "character"))
cbp = read.csv("../data/temp/cbp_temp.csv", colClasses = c(GEO_ID = "character"))
market = read.csv("../data/temp/market_temp.csv", colClasses = c(GEOID_i = "character"))
state = read.csv("../data/temp/state_con_tax.csv")
raw_pop = read_xlsx("C:/document/SMU PhD/research/Data/Census Population Estimates Program/co-est2020int-pop.xlsx")
tax = read_xlsx("../data/raw/combined sales tax.xlsx")


# mark border county ----
# 1)读county边界
cty <- counties(cb = TRUE, year = 2022, class = "sf") %>%
  st_transform(5070) %>%  # 把坐标系换为美国专用的投影坐标系（单位：米）
  select(GEOID, STATEFP, NAME)


# 2)建立邻接关系（touches:共享边或点）
nb <- st_touches(cty)  # list: 每个county的邻居index

# 3)判断是否存在“跨州邻居”
boundary <- vapply(seq_len(nrow(cty)), function(i) {
  nbr <- nb[[i]]
  if (length(nbr) == 0) return(FALSE)
  any(cty$STATEFP[nbr] != cty$STATEFP[i])
}, logical(1))

cty$is_boundary_county = boundary

# 4)costalline
coastline <- ne_download(
  scale = "medium",
  type = "coastline",
  category = "physical",
  returnclass = "sf"
)
coastline <- st_transform(coastline, st_crs(cty))

# 5)international boundary
countries <- ne_countries(scale = "medium", returnclass = "sf")

usa <- countries %>%
  filter(admin == "United States of America")

neighbors <- countries %>%
  filter(admin %in% c("Canada", "Mexico"))

# border line = intersection boundary
border_line <- st_intersection(
  st_boundary(usa),
  st_boundary(neighbors)
) 
border_line = st_transform(border_line, st_crs(cty))

# 6)costal indicator
cty$coastal <- as.integer(
  lengths(st_intersects(cty, coastline)) > 0
)

# 7)international indicator 
cty$border <- as.integer(
  lengths(st_intersects(cty, border_line)) > 0
)


# Establishment graph preparation ----

# Generate counties and states
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>% 
  rename(GEO_ID = GEOID)
states_sf   <- states(cb = TRUE, resolution = "20m", year = 2020)

exclude <- c("02", "15", "60", "66", "69", "72", "78")

counties_sf <- counties_sf %>%
  filter(!STATEFP %in% exclude)

states_sf <- states_sf %>%
  filter(!STATEFP %in% exclude)

# Top 50 big cities in the US
cities50 <- maps::us.cities %>%
  as_tibble() %>%
  arrange(desc(pop)) %>%
  slice(1:50) %>%
  transmute(city = name, pop, lon = long, lat = lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

cities50 <- st_transform(cities50, st_crs(cty))
xy <- st_coordinates(cities50)
cities50_df <- cities50 %>%
  st_drop_geometry() %>%
  mutate(x = xy[,1], y = xy[,2])

# Generate distance to border and which border the county belongs to
states_pre <- states_sf %>% 
  select(STUSPS, STATEFP, NAME) %>% 
  st_transform(5070)
state_border <- st_intersection(states_pre, states_pre) %>% 
  filter(STUSPS != STUSPS.1) %>% 
  mutate(
    border_name = paste0(
      pmin(STUSPS, STUSPS.1),
      "-",
      pmax(STUSPS, STUSPS.1)
    )
  ) %>% 
  group_by(border_name) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")

counties_sf  <- st_transform(counties_sf, 5070)
state_border <- st_transform(state_border, 5070)

county_cent <- st_centroid(counties_sf)
nearest_id <- st_nearest_feature(county_cent, state_border)

nearest_pts <- st_nearest_points(
  county_cent,
  state_border[nearest_id, ],
  pairwise = TRUE
)

counties_sf$dist_to_border <- as.numeric(st_length(nearest_pts))
counties_sf$nearest_border <- state_border$border_name[nearest_id]

rm(coastline, countries, border_line, nb, neighbors, usa, states_pre)


# combine main ----
pop = raw_pop
pop$NAME = str_remove(pop$NAME, "^\\.")

#add back county's own gdp to gdp_in and ma_in
main = cbp %>% 
  left_join(cty %>% select(GEOID,is_boundary_county, STATEFP, coastal, border), by = c("GEO_ID" = "GEOID")) %>% 
  filter(STATEFP != 72) %>% 
  left_join(gdp, by = c("YEAR" = "YEAR", "GEO_ID" = "GEO_ID")) %>% 
  left_join(market, by = c("YEAR" = "year", "GEO_ID" = "GEOID_i")) %>% 
  mutate(lest = asinh(ESTAB),
         gdp = as.numeric(gdp),
         ma_in = ma_in + gdp,
         gdp_in = gdp + gdp_in,
         lma_in = log(ma_in),
         lma_out = asinh(ma_out),
         lemp = asinh(EMP),
         lgdp_tax_in = asinh(gdp_tax_in),
         lgdp_tax_out = asinh(gdp_tax_out)) %>% 
  left_join(pop %>% select(NAME, `2018pop`), by = "NAME") %>% 
  mutate(state = sub(".*,", "", NAME),
         state = sub("^ ", "", state)) %>% 
  left_join(state %>% select(state, year, expo, tax), by = c("state" = "state", "YEAR" = "year"))

# drop county with only one year obs and na
main = main %>% 
  group_by(GEO_ID) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  drop_na()


# est graph after controlling market access ----
resid_graph = main %>% 
  filter(YEAR == 2017) %>% 
  mutate(have_est = ifelse(ESTAB>0, 1, 0))

est_emp = feols(lemp ~ lma_in + lma_out + coastal + border, data = resid_graph)
est_est = feols(lest ~ lma_in + lma_out + coastal + border, data = resid_graph)

resid_graph = resid_graph %>% 
  mutate(resid_est = residuals(est_est),
         resid_emp = residuals(est_emp))

q_est <- quantile(resid_graph$resid_est, probs = c(.01, .99), na.rm = TRUE)
q_emp = quantile(resid_graph$resid_emp, probs = c(.01, .99), na.rm = TRUE)

resid_graph = resid_graph %>% 
  mutate(resid_clip_est = pmin(pmax(resid_est, q_est[1]), q_est[2]),
         resid_clip_emp = pmin(pmax(resid_emp, q_emp[1]), q_emp[2]),
         top_est = ifelse(resid_clip_est > quantile(resid_clip_est, 0.9, na.rm = TRUE), 1, 0),
         top_emp = ifelse(resid_clip_emp > quantile(resid_clip_emp, 0.9, na.rm = TRUE), 1, 0))

map_resid = counties_sf %>% 
  left_join(resid_graph, by = "GEO_ID")

ggplot(map_resid) +
  geom_sf(aes(fill = top_emp), color = NA) +
  ggtitle("Top 10th EMP Resid County and Top 50 Big Cities, 2017")+
  geom_sf(data = states_sf,
          fill = NA,
          color = "black",
          size = 0.5)+
  geom_sf(data = cities50, size = 0.5, color = "red") +
  theme_void()
ggsave("../output/emp_resid_17.png")

rm(cities50, cities50_df)

#kansus city 地跨两州，但是税率高的county反而有更多的est
#考虑港口、国外市场



# regression ----

# market + foreign state counties gdp x tax +foreign state counties gdp x tax x post 
# + home state counties gdp x tax +home state counties gdp x tax x post
# 分离在本州和外州的gdp sum √
# 收集tax数据，构建每个county x tax的数据
# 扩大market radius
# 写个模型 (见note) √
reg = feols(
  lemp ~ 
  lma_in + lma_out + lgdp_tax_in + lgdp_tax_in * I(YEAR >= 2019) + lgdp_tax_out + lgdp_tax_out * I(YEAR >= 2019) |YEAR + GEO_ID ,
  data = main,
  cluster = ~STATEFP
)



# border density graph ----
# hard to decide which county belongs to which border

# clean tax data
tax17 <- tax %>% 
  select(tax_2017, GEO_ID)
tax17$state_abbr <- state.abb[match(tax17$GEO_ID, state.name)]

# combine density df
density <- resid_graph %>% 
  select(GEO_ID, NAME, YEAR, resid_clip_emp, state) %>% 
  left_join(counties_sf %>% select(GEO_ID, dist_to_border, nearest_border), by = "GEO_ID") %>% 
  drop_na() %>% # exclude alaska and hawaii
  separate(nearest_border, into = c("state1", "state2"), sep = "-")
density$state_home <- state.abb[match(density$state, state.name)]
density <- density %>% 
  mutate(state_other = ifelse(state1 == state_home, state2, state1)) %>% 
  select(-state1, -state2) %>% 
  left_join(tax17 %>% select(-GEO_ID), by = c("state_home" = "state_abbr")) %>% 
  rename(tax_home = tax_2017) %>%
  left_join(tax17 %>% select(-GEO_ID), by = c("state_other" = "state_abbr")) %>% 
  rename(tax_other = tax_2017) %>% 
  mutate(high_side = ifelse(tax_home > tax_other, 1, -1),
         dist_to_border_adj = dist_to_border * high_side)

ggplot(density, aes(x = dist_to_border_adj, y = resid_clip_emp)) +
  geom_point(alpha = 0.3) +
  geom_smooth()




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
