#------------------------------------------------------------------------------
# Name:     target_prediction_depth_GSM.R
#
# Content:  - create target GSM prediction depths
#           
# Inputs:   - prepared covariates (out/data/covariates/final_stack/)
#           - raster mask (out/data/covariates/mask_continuous/ahn2_nodata_mask.tif)
#
# Output:   - Masked GSM target prediction depths as rasters:
#             out/data/covariates/target_GSM_depths/
#
# Refs:     - e.g. Arrouays et al. 2015 (GlobalSoilMap project specifications)
#
# Runtime:  - approx. 15 min (entire script)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  December 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "foreach", "doParallel")

lapply(pkgs, library, character.only = TRUE)



# locate, read in and stack covariates to predict over
v_cov_names <- dir("out/data/covariates/final_stack",
                   pattern = "\\.grd$", recursive = TRUE)

ls_r_cov <- foreach(cov = 1:length(v_cov_names)) %do%
  raster(paste0("out/data/covariates/final_stack/", v_cov_names[[cov]]))

r_stack_cov <- stack(ls_r_cov)

# create tbl of coordinates and depth increments and midpoints
tbl_d_GSM <- coordinates(r_stack_cov) %>% 
  as_tibble() %>% 
  add_column(d_0 = 0,
             d_0_5_mid = 2.5,
             d_5 = 5,
             d_5_15_mid = 10,
             d_15 = 15,
             d_15_30_mid = 22.5,
             d_30 = 30,
             d_30_60_mid = 45,
             d_60 = 60,
             d_60_100_mid = 80,
             d_100 = 100,
             d_100_200_mid = 150,
             d_200 = 200)

# make target depth prediction rasters (sequential; not enough RAM for parallel)
system.time(
  ls_r_depth_GSM <- foreach(d = 1:(length(tbl_d_GSM)-2)) %do%
  rasterFromXYZ(xyz = tbl_d_GSM %>% 
                  dplyr::select(x, y, colnames(tbl_d_GSM)[d+2]),
                res = res(r_stack_cov),
                crs = crs(r_stack_cov))
) # time elapse: 6.3 min (sequential)

# mask target depth prediction rasters
r_mask <- raster("out/data/covariates/mask_continuous/ahn2_nodata_mask.tif")

# mask depth rasters (sequential)
system.time(
  ls_r_depth_GSM <- foreach(d = 1:length(ls_r_depth_GSM)) %do% {
    raster::mask(x = ls_r_depth_GSM[[d]],
                 mask = r_mask,
                 maskvalue = 0) 
  }
) # time elapse: 2 min (sequential)

# combine into stack
r_stack_depth_GSM <- stack(ls_r_depth_GSM)

# plot target prediction depths
spplot(r_stack_depth_GSM)

# make table of datatypes to save decimals as floats, others as integers
tbl_datatype <- tibble(names = names(r_stack_depth_GSM),
                       d_cm = c(0, 2.5, 5, 10, 15, 22.5, 30, 45, 60, 80, 100, 150, 200),
                       datatype = c("INT1U", "FLT4S", "INT1U", "INT1U", "INT1U", "FLT4S",
                                    "INT1U", "INT1U", "INT1U", "INT1U", "INT1U", "INT1U",
                                    "INT1U"))

# save to disk (not enough RAM to do in parallel)
system.time(
  foreach(rr = 1:length(ls_r_depth_GSM)) %do%
    raster::writeRaster(r_stack_depth_GSM[[rr]],
                        paste0("out/data/covariates/target_GSM_depths/",
                               names(r_stack_depth_GSM[[rr]]), ".tif"),
                        datatype = tbl_datatype$datatype[rr],
                        overwrite = TRUE)
)
# time elapsed: 3 min


