#------------------------------------------------------------------------------
# Name:     extract_from_raster_comparison.R
#
# Content:  - read in prepared covariate stack and soil point data with coordinates
#           - reduce size of soil data to rapidly benchmark
#           - benchmark & compare extract approaches
#
# Refs:     - https://tmieno2.github.io/R-as-GIS-for-Economists/extraction-speed-comparison.html
#           
# Inputs:   - prepared covariates (out/data/covariates/final_stack/)
#           - soil point data (out/data/soil/tbl_BIS.Rds)
#
# Output:   - 
#
# Runtime:  - approx.  (entire script)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  December 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "rgdal", "raster", "terra", "sf", "exactextractr",
          "tictoc", "foreach")
lapply(pkgs, library, character.only = TRUE)
# could also check out "velox package": https://github.com/hunzikp/velox
# unable to download using devtools::install_github("hunzikp/velox") ???



### Read in data; convert soil data into spatial points dataframes (spdf) ------

# read in all BIS soil point data
system.time(
  tbl_BIS <- read_rds("out/data/soil/tbl_BIS.Rds")
) # time elapse:  min

# decrease size of datasets for rapid testing/benchmarking
n = 1e3 # set number of observations to benchmark with
tbl_BIS_reduced <- tbl_BIS %>% 
  tail(n)

# locate, read in, and stack covariates to predict over
v_cov_names <- dir("out/data/covariates/final_stack",
                   pattern = "\\.grd$", recursive = TRUE)

ls_r_cov <- foreach(cov = 1:length(v_cov_names)) %do%
  raster(paste0("out/data/covariates/final_stack/", v_cov_names[[cov]]))

r_stack_cov <- stack(ls_r_cov)

# convert tbl to spatial points dataframe (spdf)
spdf_BIS_reduced <- tbl_BIS_reduced # rename, since we are about to change object class
coordinates(spdf_BIS_reduced) <- ~X+Y
proj4string(spdf_BIS_reduced) <- crs(r_stack_cov)

# convert tbl to sf object
sf_BIS_reduced <- tbl_BIS_reduced %>% 
  st_as_sf(., coords = c("X", "Y"), crs = crs(r_stack_cov))



### Benchmark different ways to extract -------------------------------------

# using "exactextractr" pkg

# benchmark, start timing
tic()

mat_cov_BIS_exact <- exactextractr::exact_extract(x = r_stack_cov,
                                                  y = sf_BIS_reduced,
                                                  progress = TRUE)

# benchmark, stop timing
toc()
# time elapse: 1.9 min (1000 obs); 14 sec (100 obs)


# using "raster" pkg
tic()
mat_cov_BIS_raster <- raster::extract(r_stack_cov, spdf_BIS_reduced)
toc()
# time elapse: 15 sec (100 obs)


# using "terra" pkg with sf object
tic()
mat_cov_BIS_terra_sf <- terra::extract(x = r_stack_cov, y = sf_BIS_reduced)
toc()
# time elapse: 17 sec (100 obs)

# using "terra" pkg with spatial points datafram
tic()
mat_cov_BIS_terra_spdf <- terra::extract(x = r_stack_cov, y = spdf_BIS_reduced)
toc()
# time elapse: 28 sec (100 obs)


