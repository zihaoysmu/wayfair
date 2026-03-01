library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(gt)
library(psych)

raw_CFS_2012 = read.csv("C:/document/SMU PhD/research/sales tax and immigration/Data/CFS/2012/CFSAREA2012.CF1200A30-Data.csv")

data = raw_CFS_2012
data$VAL[data$VAL == "Z"] = 0
data$VAL[data$VAL == "S"] = NA
data$VAL = as.numeric(data$VAL)

test = data %>% 
  filter(str_detect(GEO_ID, "^04"),
         str_detect(DDESTGEO, "^04"),
         DMODE == "001") %>% 
  select(DDESTGEO, GEO_ID, NAME, DDESTGEO_LABEL, NAICS2007, NAICS2007_LABEL, VAL)
  summarise(trade = sum(VAL, na.rm = TRUE))

#After summation, there's no "S" kind data
any(is.na(data$trade))

