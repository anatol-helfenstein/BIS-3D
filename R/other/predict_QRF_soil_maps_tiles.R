#------------------------------------------------------------------------------
# Name:     60_predict_QRF_soil_maps_tiles.R
#           (QRF = quantile regression forest)
#
# Content:  - read in fitted quantile regression forest (QRF) model
#           - read in tiles covariate stack (identical to stack using for model fitting)
#           - predict soil properties using new data for each tile
#           - mosaic tiles back together to get 1 map of entire NL
#
# Refs:     - see R/tiling_rasters.R
#           
# Inputs:   - Fitted QRF: out/data/model/qrf_fit_[soil_property]_obs[]_p[].Rds
#           - Tiled stack of rasters used for model fitting:
#             out/data/covariates/tiles/[]x[]grid/
#
# Output:   - soil property prediction maps (tiles)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  December 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "quantregForest", "foreach", "doParallel")
lapply(pkgs, library, character.only = TRUE)



### Read in fitted QRF model and tiled prediction raster stack -----------------

# CHOOSE TILING GRID nxn from out/data/covariates/tiles: (e.g. 2x2, 4x4, 10x10)
n = 10

# set up parallel backend to use multiple cores
cores <- detectCores()
cl <- makeCluster(cores) # use all available cores
registerDoParallel(cl)

# locate, read in and stack tiled covariates to predict over
ls_v_dir_tiles <- foreach(t = 1:n^2) %dopar%
  dir(paste0("out/data/covariates/tiles/", n, "x", n, "grid"),
      pattern = paste0("tile", t, "_"), recursive = TRUE)

system.time(
  ls_r_tiles <- foreach(t = 1:n^2) %do% {
  foreach(r = 1:length(ls_v_dir_tiles[[1]])) %dopar% {
    raster::raster(paste0("out/data/covariates/tiles/", n, "x", n, "grid/",
                          ls_v_dir_tiles[[t]][r]))
    }
  }
) # time elapse parallel: 5 min (10x10 grid; 48 cores)

ls_r_stack_fit <- foreach(t = 1:n^2) %dopar%
  raster::stack(ls_r_tiles[[t]])

# change raster names to those used for fitting QRF
foreach(t = 1:n^2) %do% {
  names(ls_r_stack_fit[[t]]) <- names(ls_r_stack_fit[[t]]) %>%
    stringr::str_replace(., paste0("tile", t, "_"), "")
  }

# read in fitted QRF model (ADJUST AS NECESSARY!)
qrf_fit <- read_rds("out/data/model/qrf_fit_pH_KCl_obs21523_p181_nofactor.Rds")

# summary of calibrated model (model fit)
qrf_fit

# stop parallel backend
stopCluster(cl)



### Predict target soil property using new data (entire NL) --------------------

# at the moment, raster stack still contains all possible GSM depth increments 
v_depth <- names(ls_r_stack_fit[[1]]) %>% 
  stringr::str_detect(., "^d_")

v_depth <- names(ls_r_stack_fit[[1]])[v_depth]

# ! CHOOSE TARGET PREDICTION DEPTH !
ls_r_stack_fit[[50]]$d_0
ls_r_stack_fit[[50]]$d_0_5_mid
ls_r_stack_fit[[50]]$d_5

# remove the upper, midpoint and lower depth boundary from layers that will be dropped
v_depth_drop <- v_depth[!v_depth %in% c("d_0", "d_0_5_mid", "d_5")]

# drop other depth layers
ls_r_stack_fit_d <- foreach(t = 1:n^2) %do%
  raster::dropLayer(ls_r_stack_fit[[t]], v_depth_drop)

# rename to same names as used for QRF model calibration
foreach(t = 1:n^2) %do% {
  names(ls_r_stack_fit_d[[t]]) <- names(ls_r_stack_fit_d[[t]]) %>%
    stringr::str_replace(., "d_0$", "d_upper") %>% 
    stringr::str_replace(., "d_0_5_mid$", "d_mid") %>%
    stringr::str_replace(., "d_5$", "d_lower")
}

# set up parallel backend to use multiple cores
cl <- makeCluster(10) # to not overload memory
registerDoParallel(cl)

# convert stack of raster tiles into dataframe (col = covariate, row = cell/pixel)
# use .packages arg to allow each cluster to use pipe operator (from magrittr pkg)
system.time(
  ls_tbl_fit_d <- foreach(t = 1:(n^2), .packages = c("magrittr", "dplyr")) %dopar% {
    raster::as.data.frame(ls_r_stack_fit_d[[t]], xy = TRUE) %>% 
                          # na.rm = TRUE) %>% # slower than filter_all
      as_tibble() %>% 
      filter_all(all_vars(!is.na(.))) # remove NAs
  }
) # time elapse parallel: 16 min (10x10 grid; 8 cores; 106GB RAM)
# time elapse parallel: 14 min (10x10 grid; 10 cores; 106GB RAM)

# remove tbls without new data
not_na <- map_lgl(ls_tbl_fit_d, ~nrow(.x) > 0)
ls_tbl_fit_d <- ls_tbl_fit_d[not_na]

# designate categorical variables as factors; first read in covariate metadata
tbl_cov_meta <- read_csv("data/covariates/covariates_metadata.csv") %>% 
  # only interested in covariates we use in model
  filter(name %in% colnames(ls_tbl_fit_d[[1]]))

# vector of categorical covariate names
v_cat <- tbl_cov_meta %>% 
  filter(values_type %in% "categorical") %>% 
  .$name %>% 
  sort()

# OPTIONAL: depending on whether model was fitted with categorical covariates as factors
# designate all categorical variables as factors
# system.time(
#   tbl_fit_t2_0_5 <- tbl_fit_t2_0_5 %>%
#     mutate_if(., colnames(.) %in% v_cat, as.factor)
# ) # time elapse sequential: 9 min

# stop parallel backend
stopCluster(cl)

# Per default only one observation per node is used for prediction. This can be
# set with the input argument all with default all = FALSE (one observation per 
# node used) and when setting all = TRUE, all observation per node are used. The
# use of only one observation per node is of advantage especially when working
# with large datasets since the algorithm can be very slow otherwise. Numerical
# experiments suggest that the performance remains good.
# In cases with big datasets & few new sample points as input for newdata,
# setting all = FALSE may be significantly slower than choosing all = TRUE.
# When newdata (test set) is very large, use all = FALSE (default) since much faster.

# ERROR: New factor levels not present in the training data!
# -> For now, calibrate model without designating factors
system.time(
  ls_qrf_predict <- foreach(t = (1:length(ls_tbl_fit_d))[1:10]) %do% {
    predict(qrf_fit,
            newdata = ls_tbl_fit_d[[t]] %>%
              dplyr::select(-c(x, y)),
            what = c(0.05, 0.5, 0.95),
            all = FALSE,
            obs = 1)
  }
) # time elapse sequential: 26 min (10 tiles from 10x10 grid; GB RAM)

system.time(
  qrf_predict_t1 <- predict(qrf_fit,
                            newdata = ls_tbl_fit_d[[55]] %>%
                              dplyr::select(-c(x, y)),
                            what = c(0.05, 0.5, 0.95),
                            all = FALSE,
                            obs = 1)
) # time elapse sequential: 8 min (1.3e6 x 181 DF)

# convert to tibble and add coordinates
qrf_predict_t1 <- qrf_predict_t1 %>% 
  as_tibble() %>% 
  add_column(ls_tbl_fit_d[[55]]$x, ls_tbl_fit_d[[55]]$y,
             .before = "quantile= 0.1") %>% 
  rename(x = `ls_tbl_fit_d[[55]]$x`,
         y = `ls_tbl_fit_d[[55]]$y`,
         pred_10 = `quantile= 0.1`,
         pred_50 = `quantile= 0.5`,
         pred_90 = `quantile= 0.9`)

# convert to spatial raster
r_qrf_predict_t1 <- rasterFromXYZ(qrf_predict_t1,
                                  res = c(25, 25),
                                  crs = crs(ls_r_stack_fit_d[[1]]))

# save preliminary tile of target variable map
writeRaster(r_qrf_predict_t1$pred_10,
            "out/maps/target/pH_KCl/pH_KCl_0_5_qrf_t55_pred10.tif")

writeRaster(r_qrf_predict_t1$pred_50,
            "out/maps/target/pH_KCl/pH_KCl_0_5_qrf_t55_pred50.tif")

writeRaster(r_qrf_predict_t1$pred_90,
            "out/maps/target/pH_KCl/pH_KCl_0_5_qrf_t55_pred90.tif")




