#------------------------------------------------------------------------------
# Name:     12_soil_BIS_master_tibble.R
#
# Content:  - combine all BIS soil information:
#           - BPK, CC-NL, LSK and PFB lab and field tibbles into one master tibble
#
# Inputs:   - prepared soil tibbles in out/data/soil/:
#               - tbl_BPK.Rds
#               - tbl_CCNL.Rds
#               - tbl_LSK_field.Rds
#               - tbl_LSK_lab.Rds
#               - tbl_PFB_field.Rds
#               - tbl_PFB_lab.Rds
#
# Output:   - BIS master tibble: out/data/soil/tbl_BIS.Rds
#
# Project   BIS+ 
# Author:   Anatol Helfenstein
# Date:     November 2020



### install / load required packages ---------------------------------------

library(tidyverse)



### Load soil data ----------------------------------------------------------

tbl_BPK <- read_rds("out/data/soil/tbl_BPK.Rds")
tbl_CCNL <- read_rds("out/data/soil/tbl_CCNL.Rds")
tbl_LSK_field <- read_rds("out/data/soil/tbl_LSK_field.Rds")
tbl_LSK_lab <- read_rds("out/data/soil/tbl_LSK_lab.Rds")
tbl_PFB_field <- read_rds("out/data/soil/tbl_PFB_field.Rds")
tbl_PFB_lab <- read_rds("out/data/soil/tbl_PFB_lab.Rds")

# prepare binding of soil information by adding type of BIS data
tbl_BPK <- tbl_BPK %>% 
  add_column(BIS_type = "field", .before = .$site_id) %>% 
  add_column(BIS_tbl = "BPK", .before = .$site_id)

tbl_CCNL <- tbl_CCNL %>% 
  add_column(BIS_type = "lab", .before = .$site_id) %>% 
  add_column(BIS_tbl = "CCNL", .before = .$site_id)

tbl_LSK_field <- tbl_LSK_field %>% 
  add_column(BIS_type = "field", .before = .$site_id) %>% 
  add_column(BIS_tbl = "LSK", .before = .$site_id)

tbl_LSK_lab <- tbl_LSK_lab %>% 
  add_column(BIS_type = "lab", .before = .$site_id) %>% 
  add_column(BIS_tbl = "LSK", .before = .$site_id)

tbl_PFB_field <- tbl_PFB_field %>% 
  add_column(BIS_type = "field", .before = .$site_id) %>% 
  add_column(BIS_tbl = "PFB", .before = .$site_id)

tbl_PFB_lab <- tbl_PFB_lab %>% 
  add_column(BIS_type = "lab", .before = .$site_id) %>% 
  add_column(BIS_tbl = "PFB", .before = .$site_id)



### Combine soil data into BIS master tibble and save to disk ------------------

tbl_BIS <- bind_rows(tbl_BPK,
                     tbl_CCNL,
                     tbl_LSK_field, tbl_LSK_lab,
                     tbl_PFB_field, tbl_PFB_lab)

# remove observations without values for any of the target soil properties (approx. 15K)
system.time(
  tbl_BIS <- tbl_BIS %>%
    unnest(soil_target) %>%
    filter_at(vars(SOM:grain_size_m), any_vars(!is.na(.))) %>% 
    nest(soil_target = SOM:grain_size_m)
) # time elapse: 4 min

# reorder by column of interest
tbl_BIS <- tbl_BIS %>% 
  select(BIS_tbl, BIS_type, sample_id, site_id:d_lower,
         soil_target, soil_chem, soil_phys, soil_other,
         soil_profile, env_fact, metadata, unknown)

# save tbl to disk
write_rds(tbl_BIS, "out/data/soil/01_tbl_BIS.Rds")


