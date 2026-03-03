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
main = read.csv("../data/temp/state_con_tax.csv")



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
  data = main,
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
