#------------------------------------------------------------------------------
# Name:     30_regression_matrix.R
#
# Content:  - read in prepared covariate stack and soil point data with coordinates
#           - overlay rasters and points and prepare regression matrix (extract
#             covariate values from sampled locations)
#           
# Inputs:   - prepared covariates (out/data/covariates/final_stack/)
#           - soil point data
#
# Output:   - regression matrix in "out/data/model/"
#           - e.g. "out/data/model/tbl_regmat_PFB_lab.Rds"
#
# Runtime:  - approx. 40 min (entire script)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  June 2021
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "sf", "rgdal", "raster", "foreach") 
          # "snowfall", "terra", "exactextractr")
lapply(pkgs, library, character.only = TRUE)



### Read in data; convert soil data into spatial points dataframes (spdf) ------

# read in all BIS soil point data
system.time(
  tbl_BIS <- read_rds("out/data/soil/02_tbl_BIS.Rds")
) # time elapse:  min

# locate rasters for stack
v_cov_names <- dir("out/data/covariates/final_stack",
                   pattern = "\\.grd$", recursive = TRUE)

# read in covariates and make stack
ls_r_cov <- foreach(cov = 1:length(v_cov_names)) %do%
  raster(paste0("out/data/covariates/final_stack/", v_cov_names[[cov]]))

r_stack_cov <- stack(ls_r_cov)

# read in border of Netherlands shapefile
# sf_NL_borders <- st_read("data/other/NL_borders.shp")
# spdf_NL_borders <- readOGR("data/other/NL_borders.shp")

# convert tbl to spatial points dataframe (spdf)
# spdf_BIS <- tbl_BIS # rename, since we are about to change object class
# coordinates(spdf_BIS) <- ~X+Y
# proj4string(spdf_BIS) <- crs(r_stack_cov)

# convert tbl to sf object
sf_BIS <- tbl_BIS %>% 
  st_as_sf(., coords = c("X", "Y"), crs = crs(r_stack_cov))

# For exploratory analysis of soil point data and covariates over space,
# see 35_model_data_expl_analysis.Rmd



### Extract covariate values at soil sampling locations ---------------------

# split BIS into chunks to extract covariate values in parallel
# n = 20
# v_rows = 1:nrow(spdf_BIS)
# ls_chunks <- split(v_rows, sort(v_rows %% n))

# create R cluster using snowfall package
# automatically adjusts # of cores to available memory (see sfCluster in "snowfall" pkg)
# therefore all cores (-1) registered when using extract() but only few are used
# sfInit(parallel = TRUE, cpus = parallel::detectCores() - 1)

# raster::extract does not read rasters into memory
# system.time(
#   mat_cov_BIS_1 <- sfSapply(ls_r_cov, raster::extract, y = spdf_BIS[ls_chunks[[1]],])
# )
# time elapsed: 5.5 min
# for entire BIS spdf only 1-3 cores are used!
# sfStop() # removes working memory (RAM)

# too large to run in parallel
system.time(
  tbl_cov_BIS <- raster::extract(r_stack_cov, sf_BIS)
)
# time elapsed sequential: 27 min

# make into tibble and rename
tbl_cov_BIS <- tbl_cov_BIS %>% 
  as_tibble() %>% 
  rename_all(~names(r_stack_cov))



### Combine point info of covariates & soils into regression matrices (regmat) ----

tbl_regmat_BIS <- bind_cols(tbl_BIS, tbl_cov_BIS)

# add midpoint of each depth increment;
# this variable "depth" will be used as predictor in a 3D RF
tbl_regmat_BIS <- tbl_regmat_BIS %>% 
  mutate(d_mid = (d_lower - d_upper)/2 + d_upper, .after = d_lower)

# first and last covariate to select predictors in tibble
cov_1 <- names(r_stack_cov[[1]])
cov_last <- names(r_stack_cov[[length(ls_r_cov)]])

# remove observations where predictors/covariates = NA
tbl_regmat_BIS_noNA <- tbl_regmat_BIS %>% 
  filter_at(vars(all_of(cov_1):all_of(cov_last)), all_vars(!is.na(.)))
# would be interesting to find why there are 24K NA values...?!
  
# nest covariates and re-order cols
system.time(
  tbl_regmat_BIS_noNA <- tbl_regmat_BIS_noNA %>%
    nest(cov = c(colnames(tbl_cov_BIS[1]):tail(colnames(tbl_cov_BIS),
                                               n = 1))) %>%
    dplyr::select(BIS_tbl:soil_target,
                  cov,
                  soil_chem:unknown)
) # time elapse: 3.5 min

# save regression matrices to disk
system.time(
  write_rds(tbl_regmat_BIS_noNA, "out/data/model/tbl_regmat_BIS.Rds")
) # time elapse: 4 min


