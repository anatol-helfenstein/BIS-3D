#-------------------------------------------------------------------------------
# Name:     16_soil_BIS_remove_Ohorizon_outliers.R
#
# Content:  - based on findings in exploratory analysis of BIS data (scripts
#             "15_soil_BIS_expl_analysis_[].R"), we remove any response data we
#             do not want to include in BIS-3D models:
#               - e.g. O horizon ?humus layer above mineral soil observations
#               - any observations where we are certain that they are outliers
#
# Inputs:   - prepared tibble of all BIS soil point data (tbl_BIS.Rds)
#
# Output:   - BIS master tibble: out/data/soil/tbl_BIS.Rds
#
# Project   BIS+ 
# Author:   Anatol Helfenstein
# Date:     February 2021
#-------------------------------------------------------------------------------



### Load soil data, remove data and save updated version to disk ---------------

library(tidyverse)

tbl_BIS <- read_rds("out/data/soil/01_tbl_BIS.Rds")

# remove O horizon observations
tbl_BIS_no_Ohor <- tbl_BIS %>% 
  filter(d_lower > 0)

# remove "outliers"
system.time(
  tbl_BIS_no_Ohor_outliers <- tbl_BIS_no_Ohor %>% 
  # in order to locate "outliers", first unnest soil target variables
  unnest(soil_target) %>% 
  # based on exploratory analysis, remove pH value of 0.2 from PFB site 474
  # any other values to remove? currently not (status: 2021-02-23)
  mutate(pH_KCl = na_if(pH_KCl, 0.2)) %>% 
  # nest soil target information again
  nest(soil_target = c(SOM:grain_size_m)) %>% 
  # reorder cols in same order as original BIS master tibble
  select(BIS_tbl:d_lower, soil_target, soil_chem:unknown)
)
# time elapse: 3.3 min

# save tbl to disk
write_rds(tbl_BIS_no_Ohor_outliers, "out/data/soil/02_tbl_BIS.Rds")


