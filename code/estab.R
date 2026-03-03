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


# establishment graph pre ----
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>% 
  rename(GEO_ID = GEOID)
states_sf   <- states(cb = TRUE, resolution = "20m", year = 2020)

exclude <- c("02", "15", "60", "66", "69", "72", "78")

counties_sf <- counties_sf %>%
  filter(!STATEFP %in% exclude)

states_sf <- states_sf %>%
  filter(!STATEFP %in% exclude)

# 美国前五十大城市
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

rm(coastline, countries, border_line, nb, neighbors, usa)


# combine main ----
main = cbp %>% 
  left_join(cty %>% select(GEOID,is_boundary_county, STATEFP, coastal, border), by = c("GEO_ID" = "GEOID")) %>% 
  filter(STATEFP != 72) %>% 
  left_join(gdp, by = c("YEAR" = "YEAR", "GEO_ID" = "GEO_ID")) %>% 
  left_join(market, by = c("YEAR" = "year", "GEO_ID" = "GEOID_i")) %>% 
  mutate(lest = asinh(ESTAB),
         lma = log(ma),
         lemp = asinh(EMP))

# drop county with only one year obs
main = main %>% 
  group_by(GEO_ID) %>% 
  filter(n() > 1) %>% 
  ungroup()

  
# combine population and state consumption + tax ----
pop = raw_pop
pop$NAME = str_remove(pop$NAME, "^\\.")

main = main %>% 
  left_join(pop %>% select(NAME, `2018pop`), by = "NAME") %>% 
  mutate(state = sub(".*,", "", NAME),
         state = sub("^ ", "", state)) %>% 
  left_join(state %>% select(state, year, expo, tax), by = c("state" = "state", "YEAR" = "year"))%>% 
  drop_na() 


# est graph after controlling market access ----
resid_graph = main %>% 
  filter(YEAR == 2017) %>% 
  mutate(have_est = ifelse(ESTAB>0, 1, 0))

est_emp = feols(lemp ~ lma + `2018pop` + coastal + border + is_boundary_county, data = resid_graph)
est_est = feols(lest ~ lma + `2018pop` + coastal + border + is_boundary_county, data = resid_graph)

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

med_est = median(map_resid$resid_est, na.rm = TRUE)
med_emp = median(map_resid$resid_emp, na.rm = TRUE)
med2 = median(map_resid$lest, na.rm = TRUE)



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


#kansus city 地跨两州，但是税率高的county反而有更多的est
#考虑港口、国外市场



# regression ----

# market + foreign state counties gdp x tax +foreign state counties gdp x tax x post 
# + home state counties gdp x tax +home state counties gdp x tax x post
# 分离在本州和外州的market
# 扩大market radius
# 写个模型 (见note)
reg = feols(
  lemp ~ coastal + border + lma + `2018pop` + tax+is_boundary_county|YEAR,
  data = main,
  cluster = ~STATEFP
)
summary(reg)


# DiD ----

main = main %>% 
  mutate(time = YEAR-2019,
         treat = as.integer(is_boundary_county))

did = feols(
   ESTAB~ i(time, treat, ref = -1) | GEO_ID + YEAR,
  data = main,
  cluster = ~STATEFP
)

did_pop = feols(
  ESTAB~ i(time, treat_pop, ref = -1)| GEO_ID + YEAR,
  data = main,
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

# plot ----
# plot = main %>% 
#   filter(is_boundary_county == TRUE) %>% 
#   group_by(YEAR) %>% 
#   summarize(sum = sum(ESTAB))
# 
# ggplot(data = plot, aes(x = YEAR, y = sum))+
#   geom_point()
# 
# nevada = main %>% 
#   filter(STATEFP == 32, is_boundary_county == 1) %>% 
#   group_by(YEAR) %>% 
#   summarise(sum = sum(ESTAB))
# 
# ggplot(data = nevada, aes(x = YEAR, y = sum))+
#   geom_point()


# establishement graph ----


map_df_22 <- counties_sf %>%
  left_join(main, by = "GEO_ID") %>% 
  mutate(group = cut(
    ESTAB,
    breaks = c(0, 10, 100, Inf),
    labels = c("1–10", "10–100", "100+")
  )) %>% 
  filter(YEAR == 2022, small == 1)

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
