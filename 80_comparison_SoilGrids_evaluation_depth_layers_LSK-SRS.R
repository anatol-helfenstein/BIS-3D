#------------------------------------------------------------------------------
# Name:     80_comparison_SoilGrids_evaluation_depth_layers_LSK-SRS.R
#
# Content:  - Map accuracy of SoilGrids for NL using LSK probability sample as a
#             comparison to BIS+ maps
#           
# Refs:     - https://soilgrids.org/
#           - https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/webdav_from_R.md
#           - "vsicurl_streaming" is a type of GDAL virtual file system (VFS):
#             https://gdal.org/user/virtual_file_systems.html
#
# Inputs:   - target soil property regression matrix
#           - LSK strata shapefile
#           - NL borders shapefile
#           - SoilGrid maps (all depths, mean and quantiles) cropped for NL using
#             ISRIC GDAL VFS
#
# Output:   - GeoTIFFs of SoilGrid maps (all depths, mean and quantiles):
#             "out/maps/other/SoilGrids_v2.0/"
#           - map accuracy metrics using design-based inference (LSK-SRS) of
#             SoilGrid maps:
#             "out/maps/other/SoilGrids_v2.0/SoilGrids_phh2o_model_evaluation_LSK_SRS_d.csv"
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  August 2021
#------------------------------------------------------------------------------



### Empty memory and workspace; load required packages -------------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "gdalUtils", "rgdal", "raster", "sf", "foreach",
          "doParallel", "cowplot", "ggspatial", "mapview", "RColorBrewer",
          "scales", "surveyplanning", "boot")
lapply(pkgs, library, character.only = TRUE)
# make sure 'mapview' pkg installed from github:
# remotes::install_github("r-spatial/mapview")
mapviewOptions(fgb = FALSE) # to visualize points on map



### Designate script parameters, load modelling data, GDAL connection & ISRIC VFS ----

# 1) Specify DSM target soil property (response) of both BIS and ISRIC VFS:
TARGET = "pH_KCl"
TARGET_ISRIC = "phh2o"
# expression of TARGET for model evaluation plots
TARGET_EXP = "pH [KCl]" # observed (BIS)
TARGET_PRED_ISRIC = expression(paste(hat(pH), " [H2O]")) # predicted (SoilGrids)

# 2) Regression matrix data containing calibration and validation data
tbl_regmat_target <- read_rds(paste0("out/data/model/tbl_regmat_",
                                     TARGET,".Rds"))

# Separate tables for calibration and validation data
tbl_regmat_target_cal <- tbl_regmat_target %>% 
  filter(split %in% "train")
tbl_regmat_target_val <- tbl_regmat_target %>% 
  filter(split %in% "test")

# convert validation data to spatial sf (to incorporate sampling design later..)
sf_regmat_target_val <- tbl_regmat_target_val %>% 
  st_as_sf(., coords = c("X", "Y"), crs = "EPSG:28992")

# read in shapefile of LSK/CCNL (validation set) strata
sf_LSK_strata <- st_read("data/other/strata_ccnl.shp")
# read in NL border shapefile for mapping background
sf_NL_borders <- st_read("data/other/NL_borders.shp")

# 3) Specify number of cores to use for parallel computation
THREADS = makeCluster(parallel::detectCores() - 2) # to not overload memory
registerDoParallel(THREADS)

# 4) Set predicted vs. observed plotting X and Y axis min, max, range and breaks
XY_MIN_VAL = min(tbl_regmat_target_val[TARGET])
XY_MAX_VAL = max(tbl_regmat_target_val[TARGET])
XY_RANGE_VAL = diff(range(XY_MIN_VAL, XY_MAX_VAL))
XY_BREAKS_VAL = unique(round(seq(XY_MIN_VAL, XY_MAX_VAL, XY_RANGE_VAL/10)))

# retrieve help files of basic GDAL commands to check if GDAL is connected to R
if(.Platform$OS.type == "windows"){
  gdal.dir <- shortPathName("C:/Program Files/GDAL")
  gdal_translate <- paste0(gdal.dir, "/gdal_translate.exe")
  gdalwarp <- paste0(gdal.dir, "/gdalwarp.exe")
  gdal_calc.py <- paste0(gdal.dir, "/gdalcalc.py.exe")
} else {
  gdal_translate = "gdal_translate"
  gdalwarp = "gdalwarp"
  gdal_calc.py = "gdal_calc.py"
}
system(paste(gdalwarp, "--help"))

# "vsicurl_streaming" is a type of GDAL virtual file system (VFS)
sg_url = "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

# proj string for Homolosine projection
igh = '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'



### Retrieve TARGET SoilGrids maps for NL --------------------------------------

# transform to homosoline projection to get bbox for NL in this crs
bb_NL_igh <- st_transform(sf_NL_borders, crs = "+proj=igh") %>% 
  st_bbox() %>% 
  unname()

# gdal_translate requires different order of bbox limits: <ulx> <uly> <lrx> <lry>
bb_NL_igh <- c(bb_NL_igh[1], bb_NL_igh[4], bb_NL_igh[3], bb_NL_igh[2])

# define vector of depth layer [cm] and type of prediction strings (from ISRIC VFS)
v_d <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
v_pred <- c("mean", "Q0.05", "Q0.5", "Q0.95", "uncertainty")

# Retrieve SoilGrid v2.0 maps of TARGET soil property for:
# - all depth layers
# - mean, 0.05, 0.50 and 0.95 quantile (Q) and uncertainty (Q0.95 - Q0.05 = PI90)
foreach(d = 1:length(v_d), .packages = c("gdalUtils", "foreach")) %dopar% {
  foreach(p = 1:length(v_pred), .packages = "gdalUtils") %dopar% {
    # read in map from ISRIC VFS
    gdal_translate(paste0(sg_url, TARGET_ISRIC, '/', TARGET_ISRIC, '_',
                          v_d[d], 'cm_', v_pred[p], '.vrt'),
                   paste0('./out/maps/other/SoilGrids_v2.0/', TARGET_ISRIC, '_',
                          v_d[d], 'cm_', v_pred[p], '_NL.vrt'),
                   of = "VRT", tr = c(250,250),
                   projwin = bb_NL_igh,
                   projwin_srs = igh,
                   verbose = TRUE)
    
    # change to RD New (Dutch CRS)
    gdalwarp(paste0('./out/maps/other/SoilGrids_v2.0/', TARGET_ISRIC, '_',
                    v_d[d], 'cm_', v_pred[p], '_NL.vrt'),
             paste0('./out/maps/other/SoilGrids_v2.0/', TARGET_ISRIC, '_',
                    v_d[d], 'cm_', v_pred[p], '_NL_epsg28992.vrt'), 
             s_src = igh, 
             t_srs = "EPSG:28992", 
             of = "VRT",
             overwrite = TRUE)
    
    # Convert to GeoTIFF
    gdal_translate(paste0('./out/maps/other/SoilGrids_v2.0/', TARGET_ISRIC, '_',
                          v_d[d], 'cm_', v_pred[p], '_NL.vrt'),  
                   paste0('./out/maps/other/SoilGrids_v2.0/', TARGET_ISRIC, '_',
                          v_d[d], 'cm_', v_pred[p], '_NL.tif'), 
                   co = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2",
                          "BIGTIFF=YES"))
  }
}

# remove temporary / intermediary GDAL VRT files on disk (clean up)
unlink(dir("out/maps/other/SoilGrids_v2.0", pattern = "\\.vrt$", full.names = TRUE))
  
# read in raster and plot map as an example
r_mean_0_5 <- raster(paste0('./out/maps/other/SoilGrids_v2.0/', TARGET_ISRIC,
                            '_0-5cm_mean_NL.tif'))
spplot(r_mean_0_5)

# locate, read in and stack rasters of response soil properties
# prediction mean and quantiles (not uncertainty (PI90 b/c they are not pH x10 units))
v_response_names_pred <- dir("out/maps/other/SoilGrids_v2.0", pattern = "[meanQ0.59]_NL.tif$")

ls_r_response_pred <- foreach(r = 1:length(v_response_names_pred)) %do%
  raster(paste0("out/maps/other/SoilGrids_v2.0/", v_response_names_pred[[r]]))

# to save memory SoilGrids use units of pH x 10, so convert to same units as in BIS
r_stack_response_pred <- (stack(ls_r_response_pred))/10



### Make predicted (SoilGrids) vs. observed (BIS) tables for each depth layer ----

# extract predictions at validation locations
tbl_target_val <- raster::extract(r_stack_response_pred, sf_regmat_target_val) %>% 
  as_tibble()


sf_predobs_val <- bind_cols(sf_regmat_target_val %>% 
                              dplyr::select(split:all_of(TARGET)),
                            tbl_target_val) %>% 
  # rename cols to match names in script 51
  rename(obs = all_of(TARGET)) %>% 
  # SoilGrids have more NA locations because use a coarser mask
  filter_at(vars(contains(TARGET_ISRIC)), all_vars(!is.na(.)))

# join points and polygons sf features (overlay) to get LSK strata info
sf_predobs_val <- st_join(sf_predobs_val, sf_LSK_strata) %>% 
  rename(str_id = OBJECTID,
         str_code = stratum_cu,
         str_leng = Shape_Leng,
         str_area_m2 = Shape_Area,
         str_area_ha = opp_ha)

# area of all strata (should be approx land area of NL excluding cities & water)
area_tot_m2 = as.numeric(sum(st_area(sf_LSK_strata))) # in m^2

# split data into designated GSM depth increments
# 0 to 5 cm
sf_predobs_val_0_5 <- sf_predobs_val %>% 
  filter(between(d_mid, 0, 4.9)) %>% 
  dplyr::select(c(split:obs,
                  str_id:IW_5080,
                  contains(paste0(TARGET_ISRIC, "_0.5cm")))) %>% 
  # rename cols to match names in script 51
  rename_at(vars(contains(TARGET_ISRIC)),
            ~ c("pred_mean", "quant_5", "quant_50", "quant_95")) %>% 
  # add PI90
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95, TRUE, FALSE))

# 5 to 15 cm
sf_predobs_val_5_15 <- sf_predobs_val %>% 
  filter(between(d_mid, 5, 14.9)) %>% 
  dplyr::select(c(split:obs,
                  str_id:IW_5080,
                  contains(paste0(TARGET_ISRIC, "_5.15cm")))) %>% 
  # rename cols to match names in script 51
  rename_at(vars(contains(TARGET_ISRIC)),
            ~ c("pred_mean", "quant_5", "quant_50", "quant_95")) %>% 
  # add PI90
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95, TRUE, FALSE))

# 15 to 30 cm
sf_predobs_val_15_30 <- sf_predobs_val %>% 
  filter(between(d_mid, 15, 29.9)) %>% 
  dplyr::select(c(split:obs,
                  str_id:IW_5080,
                  contains(paste0(TARGET_ISRIC, "_15.30cm")))) %>% 
  # rename cols to match names in script 51
  rename_at(vars(contains(TARGET_ISRIC)),
            ~ c("pred_mean", "quant_5", "quant_50", "quant_95")) %>% 
  # add PI90
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95, TRUE, FALSE))

# 30 to 60 cm
sf_predobs_val_30_60 <- sf_predobs_val %>% 
  filter(between(d_mid, 30, 59.9)) %>% 
  dplyr::select(c(split:obs,
                  str_id:IW_5080,
                  contains(paste0(TARGET_ISRIC, "_30.60cm")))) %>% 
  # rename cols to match names in script 51
  rename_at(vars(contains(TARGET_ISRIC)),
            ~ c("pred_mean", "quant_5", "quant_50", "quant_95")) %>% 
  # add PI90
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95, TRUE, FALSE))

# 60 to 100 cm
sf_predobs_val_60_100 <- sf_predobs_val %>% 
  filter(between(d_mid, 60, 99.9)) %>% 
  dplyr::select(c(split:obs,
                  str_id:IW_5080,
                  contains(paste0(TARGET_ISRIC, "_60.100cm")))) %>% 
  # rename cols to match names in script 51
  rename_at(vars(contains(TARGET_ISRIC)),
            ~ c("pred_mean", "quant_5", "quant_50", "quant_95")) %>% 
  # add PI90
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95, TRUE, FALSE))

# 100 to 200 cm
sf_predobs_val_100_200 <- sf_predobs_val %>% 
  filter(between(d_mid, 100, 200)) %>% 
  dplyr::select(c(split:obs,
                  str_id:IW_5080,
                  contains(paste0(TARGET_ISRIC, "_100.200cm")))) %>% 
  # rename cols to match names in script 51
  rename_at(vars(contains(TARGET_ISRIC)),
            ~ c("pred_mean", "quant_5", "quant_50", "quant_95")) %>% 
  # add PI90
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95, TRUE, FALSE))

# number of samples below 2m
n_below_2m_val <- sf_predobs_val %>% 
  filter(d_mid > 200) %>% 
  nrow()

# CHECK: If no samples were forgotten, this should be TRUE:
nrow(sf_predobs_val_0_5) + nrow(sf_predobs_val_5_15) +
  nrow(sf_predobs_val_15_30) + nrow(sf_predobs_val_30_60) +
  nrow(sf_predobs_val_60_100) + nrow(sf_predobs_val_100_200) +
  n_below_2m_val == nrow(sf_predobs_val)



### 0-5cm: Accuracy plots and metrics ------------------------------------------

# NO accuracy plot or metrics for validation data for 0-5cm because only 1 obs
sf_predobs_val_0_5



### 5-15cm: Accuracy plots and metrics -----------------------------------------

# LSK-SRS (ANALYSIS PREPARATION STARTS HERE)
# vector of locations where there are multiple observations with the same
# distance to the midpoint of the respective GSM depth layer
v_d_dist_mid_eq_5_15 <- sf_predobs_val_5_15 %>% 
  mutate(d_dist_mid = abs(10 - d_mid)) %>% # MIDPOINT OF GSM DEPTH LAYER = 10
  group_by(site_id, d_dist_mid) %>% 
  tally() %>% 
  filter(n >= 2) %>% 
  .$site_id

# For SRS sampling design, can only have 1 observation per location and specified
# GSM depth layer. Therefore remove observations farther away from midpoint of 
# GSM depth layer, or average (mean) if distances are the same
if (any(duplicated(sf_predobs_val_5_15$site_id))) {
  sf_predobs_val_5_15 <- sf_predobs_val_5_15 %>% 
    mutate(d_dist_mid = abs(10 - d_mid)) %>% 
    group_by(site_id) %>% 
    # average observations and predictions at same location where both observations
    # have equal distance to GSM depth layer (see defined vector above)
    mutate(across(c(obs, pred_mean, quant_5, quant_50, quant_95),
                  ~ ifelse(site_id %in% v_d_dist_mid_eq_5_15, mean(.x), .x))) %>% 
    # select observation that is closest to midpoint of GSM depth layer
    slice(which.min(d_dist_mid)) %>% 
    # remove temporary grouping variable
    dplyr::select(-d_dist_mid) %>%
    # calculate new PI90 because may change slightly after averaging...
    mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                                 TRUE, FALSE))
} else {
  sf_predobs_val_5_15 <- sf_predobs_val_5_15
}

# vector of strata codes that only have one observation
v_str_code_1obs_5_15 <- sf_predobs_val_5_15 %>% 
  group_by(str_code) %>% 
  tally() %>% 
  filter(n == 1) %>% 
  .$str_code

# for upcoming calculations, we need to calculate residuals, stratum (str) weights
# and number of samples per str; we leave tibble grouped by str
sf_predobs_val_5_15 <- sf_predobs_val_5_15 %>% 
  mutate(res = obs - pred_mean, # residuals
         res_median = obs - quant_50, # residuals when using median prediction
         str_weight = str_area_m2/area_tot_m2) %>% # str weights based on area
  group_by(str_code) %>% 
  mutate(str_n = n()) # number of samples per str

# for str with only 1 observation, we estimate the within-str variance by taking
# the average of all within-str variances of all str with 2 or more observations;
# this mean variance has to be calculated for each metric separately
# Mean of all within str variances of Mean Error (ME = bias)
var_me_mean <- sf_predobs_val_5_15 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res - mean(res))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_me_median_mean <- sf_predobs_val_5_15 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res_median - mean(res_median))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of Mean Squared Error (MSE)
var_mse_mean <- sf_predobs_val_5_15 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res^2 - mean(res^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_mse_median_mean <- sf_predobs_val_5_15 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res_median^2 - mean(res_median^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of observations (for Var Y and MEC)
var_Y_mean <- sf_predobs_val_5_15 %>% 
  mutate(str_var_Y = str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2)) %>%
  slice(1L) %>% 
  pull(str_var_Y) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# calculate model metrics of probability sampling design
sf_predobs_val_5_15 <- sf_predobs_val_5_15 %>% 
  # weighted ME per str
  mutate(str_me = str_weight*mean(res),
         # within str variances of ME
         str_me_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_mean,
                             str_weight^2*(1/(str_n*(str_n-1)))*sum((res - mean(res))^2)),
         # weighted MSE per str
         str_mse = str_weight*mean(res^2),
         # within str variances of MSE
         str_mse_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((res^2 - mean(res^2))^2)),
         # same as above but now using median instead of mean predictions
         str_me_median = str_weight*mean(res_median),
         str_me_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_median_mean,
                                    str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median - mean(res_median))^2)),
         str_mse_median = str_weight*mean(res_median^2),
         str_mse_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_median_mean,
                                     str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median^2 - mean(res_median^2))^2)),
         # for MEC, first need unbiased estimator of spatial variance (var(Y);
         # see eqn. 7.16 in De Gruijter et al. 2006), where var(Y) is composed
         # of the following terms (without yet taking sum of all str; this is done in
         # "tbl_sum_stats_val" object below...):
         # 1) Y^2st 
         str_Y2st = str_weight*(sum((obs)^2)/str_n),
         # 2) Yst^2
         str_Yst2 = str_weight*mean(obs),
         # 3) v(Yst)
         str_var_Yst = ifelse(str_n == 1, var_Y_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2))
  )

# table of accuracy metrics for each str
tbl_accuracy_metrics_LSK_SRS_str_5_15 <- sf_predobs_val_5_15 %>% 
  dplyr::select(str_id:str_var_Yst) %>% 
  slice(1L) %>% # works since tibble is still grouped
  as_tibble()

# degrees of freedom = sample size minus # of str
df = nrow(sf_predobs_val_5_15) - length(unique(sf_predobs_val_5_15$str_code))

# validation accuracy metrics for 5-15 cm GSM depth layer over all strata
tbl_accuracy_metrics_LSK_SRS_5_15 <- tibble(
  n = length(sf_predobs_val_5_15$obs), # number of samples in this depth layer
  # ME metrics (ME, ME variance, 95% confidence intervals (CI95) of ME)
  me = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_me),
  me_var = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_me_var),
  me_ci_low = me - qt(0.975, df)*sqrt(me_var),
  me_ci_up = me + qt(0.975, df)*sqrt(me_var),
  # ME metrics when using median instead of mean predictions
  me_median = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_me_median),
  me_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_me_median_var),
  me_median_ci_low = me_median - qt(0.975, df)*sqrt(me_median_var),
  me_median_ci_up = me_median + qt(0.975, df)*sqrt(me_median_var),
  # MSE metrics
  mse = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_mse),
  mse_var = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_mse_var),
  mse_ci_low = mse - qt(0.975, df)*sqrt(mse_var),
  mse_ci_up = mse + qt(0.975, df)*sqrt(mse_var),
  # MSE metrics when using median instead of mean predictions
  mse_median = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_mse_median),
  mse_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_mse_median_var),
  mse_median_ci_low = mse_median - qt(0.975, df)*sqrt(mse_median_var),
  mse_median_ci_up = mse_median + qt(0.975, df)*sqrt(mse_median_var),
  # RMSE metrics
  rmse = sqrt(mse),
  rmse_ci_low = sqrt(mse_ci_low),
  rmse_ci_up = sqrt(mse_ci_up),
  # RMSE metrics when using median instead of mean predictions
  rmse_median = sqrt(mse_median),
  rmse_median_ci_low = sqrt(mse_median_ci_low),
  rmse_median_ci_up = sqrt(mse_median_ci_up),
  # for MEC, first need unbiased estimator of spatial variance (var(Y);
  # see eqn. 7.16 in De Gruijter et al. 2006)
  var_Y = sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_Y2st) -
    ((sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_Yst2))^2) +
    sum(tbl_accuracy_metrics_LSK_SRS_str_5_15$str_var_Yst),
  # using function of package "surveyplanning": result is different!?
  var_Y_pkg = s2(sf_predobs_val_5_15$obs, sf_predobs_val_5_15$str_weight),
  # MEC
  mec = 1 - mse/var_Y,
  # MEC when using median instead of mean predictions
  mec_median = 1 - mse_median/var_Y,
  # obs within PI90
  n_within_PI90 = nrow(filter(sf_predobs_val_5_15, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_5_15, within_PI90 %in% FALSE))/n)

# function to calculate MEC of a stratified random sampling dataset
fnc_MEC_SRS <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  # calculations need to be done for each strata separately
  d <- group_by(d, str_code)
  # one of the terms in MEC equation is MSE
  mse <- d %>% 
    mutate(str_mse = str_weight*mean(res^2)) %>% 
    slice(1L) %>% 
    pull(str_mse) %>% 
    sum()
  # 1st term in Var Y equation (Y^2_st)
  Y2st <- d %>% 
    mutate(str_Y2st = str_weight*(sum((obs)^2)/str_n)) %>% 
    slice(1L) %>% 
    pull(str_Y2st) %>% 
    sum()
  # 2nd term in Var Y equation ((Y_st)^2)
  Yst2 <- d %>% 
    mutate(str_Yst2 = str_weight*mean(obs)) %>% 
    slice(1L) %>% 
    pull(str_Yst2) %>% 
    sum()
  # 3rd term in Var Y equation (V(Y_st))
  var_Yst <- d %>% 
    mutate(var_Yst = ifelse(str_n == 1, var_Y_mean,
                            str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2))) %>% 
    slice(1L) %>% 
    pull(var_Yst) %>% 
    sum()
  # from these 3 terms we calculate Var(Y)
  var_Y <- Y2st - (Yst2^2) + var_Yst
  # calculate MEC using MSE and Var(Y)
  return(mec = 1 - mse/var_Y)
}

# control randomness of bootstrapping for reproducibility
set.seed(2021)

# get bootstrap of MEC values
MEC_SRS_bootstrap_5_15 <- boot(data = sf_predobs_val_5_15,
                               statistic = fnc_MEC_SRS,
                               R = 1000,
                               strata = sf_predobs_val_5_15$str_code,
                               parallel = "multicore",
                               ncpus = parallel::detectCores())

# get CI95
MEC_SRS_90CI_5_15 <- boot.ci(boot.out = MEC_SRS_bootstrap_5_15,
                             conf = 0.95,
                             type = "perc")

# add CI95 of MEC to accuracy metrics tibble
tbl_accuracy_metrics_LSK_SRS_5_15 <- tbl_accuracy_metrics_LSK_SRS_5_15 %>% 
  add_column(mec_ci_low = MEC_SRS_90CI_5_15$percent[,4],
             mec_ci_up = MEC_SRS_90CI_5_15$percent[,5])

# Modify columns for plot annotation
tbl_accuracy_metrics_LSK_SRS_5_15_annotate <- tbl_accuracy_metrics_LSK_SRS_5_15 %>%
  add_column(one_one = "1:1") %>% 
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me_ci = as.character(as.expression(paste0(
           "ME = ", round(me, 2), " [", round(me_ci_low, 2),", ",
           round(me_ci_up, 2), "]"))),
         rmse_ci = as.character(as.expression(paste0(
           "RMSE = ", round(rmse, 2), " [", round(rmse_ci_low, 2),", ",
           round(rmse_ci_up, 2), "]"))),
         mec_ci = as.character(as.expression(paste0(
           "MEC = ", round(mec, 2), " [", round(mec_ci_low, 2),", ",
           round(mec_ci_up, 2), "]"))))

# validation accuracy plot for 5-15 cm GSM depth layer
p_pred_obs_val_5_15 <- sf_predobs_val_5_15 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_SRS_5_15_annotate$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_SRS_5_15_annotate$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_5_15_annotate,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_5_15_annotate,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_5_15_annotate,
            aes(x = Inf, y = -Inf, label = mec_ci), size = 3,
            hjust = 1.1, vjust = -5.75, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_5_15_annotate,
            aes(x = Inf, y = -Inf, label = rmse_ci), size = 3,
            hjust = 1.05, vjust = -3.5, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_5_15_annotate,
            aes(x = Inf, y = -Inf, label = me_ci), size = 3,
            hjust = 1.05, vjust = -1.4, parse = FALSE) +
  ylab(as.expression(TARGET_EXP)) +
  xlab(TARGET_PRED_ISRIC) +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# report which strata were excluded in spatial validation for this depth layer
(v_strata_excluded_5_15 = setdiff(unique(sf_predobs_val$str_code),
                                  unique(sf_predobs_val_5_15$str_code)))

# area excluded in spatial validation for this depth layer in m^2
area_excluded_5_15_PFB_CV = sf_predobs_val %>% 
  filter(str_code %in% v_strata_excluded_5_15) %>% 
  group_by(str_code) %>% 
  slice(1L) %>% 
  pull(str_area_m2) %>% 
  sum()

# area included as a percentage of total area [%]
(area_included_5_15_per = (area_tot_m2 - area_excluded_5_15_PFB_CV)/area_tot_m2 *100)



### 15-30cm: Accuracy plots and metrics ----------------------------------------

# LSK-SRS (ANALYSIS PREPARATION STARTS HERE)
# vector of locations where there are multiple observations with the same
# distance to the midpoint of the respective GSM depth layer
v_d_dist_mid_eq_15_30 <- sf_predobs_val_15_30 %>% 
  mutate(d_dist_mid = abs(22.5 - d_mid)) %>% # MIDPOINT OF GSM DEPTH LAYER = 22.5
  group_by(site_id, d_dist_mid) %>% 
  tally() %>% 
  filter(n >= 2) %>% 
  .$site_id

# For SRS sampling design, can only have 1 observation per location and specified
# GSM depth layer. Therefore remove observations farther away from midpoint of 
# GSM depth layer, or average (mean) if distances are the same
if (any(duplicated(sf_predobs_val_15_30$site_id))) {
  sf_predobs_val_15_30 <- sf_predobs_val_15_30 %>% 
    mutate(d_dist_mid = abs(10 - d_mid)) %>% 
    group_by(site_id) %>% 
    # average observations and predictions at same location where both observations
    # have equal distance to GSM depth layer (see defined vector above)
    mutate(across(c(obs, pred_mean, quant_5, quant_50, quant_95),
                  ~ ifelse(site_id %in% v_d_dist_mid_eq_15_30, mean(.x), .x))) %>% 
    # select observation that is closest to midpoint of GSM depth layer
    slice(which.min(d_dist_mid)) %>% 
    # remove temporary grouping variable
    dplyr::select(-d_dist_mid) %>%
    # calculate new PI90 because may change slightly after averaging...
    mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                                 TRUE, FALSE))
} else {
  sf_predobs_val_15_30 <- sf_predobs_val_15_30
}

# vector of strata codes that only have one observation
v_str_code_1obs_15_30 <- sf_predobs_val_15_30 %>% 
  group_by(str_code) %>% 
  tally() %>% 
  filter(n == 1) %>% 
  .$str_code

# for upcoming calculations, we need to calculate residuals, stratum (str) weights
# and number of samples per str; we leave tibble grouped by str
sf_predobs_val_15_30 <- sf_predobs_val_15_30 %>% 
  mutate(res = obs - pred_mean, # residuals
         res_median = obs - quant_50, # residuals when using median prediction
         str_weight = str_area_m2/area_tot_m2) %>% # str weights based on area
  group_by(str_code) %>% 
  mutate(str_n = n()) # number of samples per str

# for str with only 1 observation, we estimate the within-str variance by taking
# the average of all within-str variances of all str with 2 or more observations;
# this mean variance has to be calculated for each metric separately
# Mean of all within str variances of Mean Error (ME = bias)
var_me_mean <- sf_predobs_val_15_30 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res - mean(res))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_me_median_mean <- sf_predobs_val_15_30 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res_median - mean(res_median))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of Mean Squared Error (MSE)
var_mse_mean <- sf_predobs_val_15_30 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res^2 - mean(res^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_mse_median_mean <- sf_predobs_val_15_30 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res_median^2 - mean(res_median^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of observations (for Var Y and MEC)
var_Y_mean <- sf_predobs_val_15_30 %>% 
  mutate(str_var_Y = str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2)) %>%
  slice(1L) %>% 
  pull(str_var_Y) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# calculate model metrics of probability sampling design
sf_predobs_val_15_30 <- sf_predobs_val_15_30 %>% 
  # weighted ME per str
  mutate(str_me = str_weight*mean(res),
         # within str variances of ME
         str_me_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_mean,
                             str_weight^2*(1/(str_n*(str_n-1)))*sum((res - mean(res))^2)),
         # weighted MSE per str
         str_mse = str_weight*mean(res^2),
         # within str variances of MSE
         str_mse_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((res^2 - mean(res^2))^2)),
         # same as above but now using median instead of mean predictions
         str_me_median = str_weight*mean(res_median),
         str_me_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_median_mean,
                                    str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median - mean(res_median))^2)),
         str_mse_median = str_weight*mean(res_median^2),
         str_mse_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_median_mean,
                                     str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median^2 - mean(res_median^2))^2)),
         # for MEC, first need unbiased estimator of spatial variance (var(Y);
         # see eqn. 7.16 in De Gruijter et al. 2006), where var(Y) is composed
         # of the following terms (without yet taking sum of all str; this is done in
         # "tbl_sum_stats_val" object below...):
         # 1) Y^2st 
         str_Y2st = str_weight*(sum((obs)^2)/str_n),
         # 2) Yst^2
         str_Yst2 = str_weight*mean(obs),
         # 3) v(Yst)
         str_var_Yst = ifelse(str_n == 1, var_Y_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2))
  )

# table of accuracy metrics for each str
tbl_accuracy_metrics_LSK_SRS_str_15_30 <- sf_predobs_val_15_30 %>% 
  dplyr::select(str_id:str_var_Yst) %>% 
  slice(1L) %>% # works since tibble is still grouped
  as_tibble()

# degrees of freedom = sample size minus # of str
df = nrow(sf_predobs_val_15_30) - length(unique(sf_predobs_val_15_30$str_code))

# validation accuracy metrics for 15-30 cm GSM depth layer over all strata
tbl_accuracy_metrics_LSK_SRS_15_30 <- tibble(
  n = length(sf_predobs_val_15_30$obs), # number of samples in this depth layer
  # ME metrics (ME, ME variance, 95% confidence intervals (CI95) of ME)
  me = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_me),
  me_var = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_me_var),
  me_ci_low = me - qt(0.975, df)*sqrt(me_var),
  me_ci_up = me + qt(0.975, df)*sqrt(me_var),
  # ME metrics when using median instead of mean predictions
  me_median = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_me_median),
  me_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_me_median_var),
  me_median_ci_low = me_median - qt(0.975, df)*sqrt(me_median_var),
  me_median_ci_up = me_median + qt(0.975, df)*sqrt(me_median_var),
  # MSE metrics
  mse = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_mse),
  mse_var = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_mse_var),
  mse_ci_low = mse - qt(0.975, df)*sqrt(mse_var),
  mse_ci_up = mse + qt(0.975, df)*sqrt(mse_var),
  # MSE metrics when using median instead of mean predictions
  mse_median = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_mse_median),
  mse_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_mse_median_var),
  mse_median_ci_low = mse_median - qt(0.975, df)*sqrt(mse_median_var),
  mse_median_ci_up = mse_median + qt(0.975, df)*sqrt(mse_median_var),
  # RMSE metrics
  rmse = sqrt(mse),
  rmse_ci_low = sqrt(mse_ci_low),
  rmse_ci_up = sqrt(mse_ci_up),
  # RMSE metrics when using median instead of mean predictions
  rmse_median = sqrt(mse_median),
  rmse_median_ci_low = sqrt(mse_median_ci_low),
  rmse_median_ci_up = sqrt(mse_median_ci_up),
  # for MEC, first need unbiased estimator of spatial variance (var(Y);
  # see eqn. 7.16 in De Gruijter et al. 2006)
  var_Y = sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_Y2st) -
    ((sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_Yst2))^2) +
    sum(tbl_accuracy_metrics_LSK_SRS_str_15_30$str_var_Yst),
  # using function of package "surveyplanning": result is different!?
  var_Y_pkg = s2(sf_predobs_val_15_30$obs, sf_predobs_val_15_30$str_weight),
  # MEC
  mec = 1 - mse/var_Y,
  # MEC when using median instead of mean predictions
  mec_median = 1 - mse_median/var_Y,
  # obs within PI90
  n_within_PI90 = nrow(filter(sf_predobs_val_15_30, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_15_30, within_PI90 %in% FALSE))/n)

# control randomness of bootstrapping for reproducibility
set.seed(2021)

# get bootstrap of MEC values
MEC_SRS_bootstrap_15_30 <- boot(data = sf_predobs_val_15_30,
                                statistic = fnc_MEC_SRS,
                                R = 1000,
                                strata = sf_predobs_val_15_30$str_code,
                                parallel = "multicore",
                                ncpus = parallel::detectCores())

# get CI95
MEC_SRS_90CI_15_30 <- boot.ci(boot.out = MEC_SRS_bootstrap_15_30,
                              conf = 0.95,
                              type = "perc")

# add CI95 of MEC to accuracy metrics tibble
tbl_accuracy_metrics_LSK_SRS_15_30 <- tbl_accuracy_metrics_LSK_SRS_15_30 %>% 
  add_column(mec_ci_low = MEC_SRS_90CI_15_30$percent[,4],
             mec_ci_up = MEC_SRS_90CI_15_30$percent[,5])

# Modify columns for plot annotation
tbl_accuracy_metrics_LSK_SRS_15_30_annotate <- tbl_accuracy_metrics_LSK_SRS_15_30 %>%
  add_column(one_one = "1:1") %>% 
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me_ci = as.character(as.expression(paste0(
           "ME = ", round(me, 2), " [", round(me_ci_low, 2),", ",
           round(me_ci_up, 2), "]"))),
         rmse_ci = as.character(as.expression(paste0(
           "RMSE = ", round(rmse, 2), " [", round(rmse_ci_low, 2),", ",
           round(rmse_ci_up, 2), "]"))),
         mec_ci = as.character(as.expression(paste0(
           "MEC = ", round(mec, 2), " [", round(mec_ci_low, 2),", ",
           round(mec_ci_up, 2), "]"))))

# validation accuracy plot for 15-30 cm GSM depth layer
p_pred_obs_val_15_30 <- sf_predobs_val_15_30 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_SRS_15_30_annotate$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_SRS_15_30_annotate$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_15_30_annotate,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_15_30_annotate,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_15_30_annotate,
            aes(x = Inf, y = -Inf, label = mec_ci), size = 3,
            hjust = 1.1, vjust = -5.75, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_15_30_annotate,
            aes(x = Inf, y = -Inf, label = rmse_ci), size = 3,
            hjust = 1.05, vjust = -3.5, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_15_30_annotate,
            aes(x = Inf, y = -Inf, label = me_ci), size = 3,
            hjust = 1.05, vjust = -1.4, parse = FALSE) +
  ylab(as.expression(TARGET_EXP)) +
  xlab(TARGET_PRED_ISRIC) +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# report which strata were excluded in spatial validation for this depth layer
(v_strata_excluded_15_30 = setdiff(unique(sf_predobs_val$str_code),
                                   unique(sf_predobs_val_15_30$str_code)))

# area excluded in spatial validation for this depth layer in m^2
area_excluded_15_30_PFB_CV = sf_predobs_val %>% 
  filter(str_code %in% v_strata_excluded_15_30) %>% 
  group_by(str_code) %>% 
  slice(1L) %>% 
  pull(str_area_m2) %>% 
  sum()

# area included as a percentage of total area [%]
(area_included_15_30_per = (area_tot_m2 - area_excluded_15_30_PFB_CV)/area_tot_m2 *100)



### 30-60cm: Accuracy plots and metrics ----------------------------------------

# LSK-SRS (ANALYSIS PREPARATION STARTS HERE)
# vector of locations where there are multiple observations with the same
# distance to the midpoint of the respective GSM depth layer
v_d_dist_mid_eq_30_60 <- sf_predobs_val_30_60 %>% 
  mutate(d_dist_mid = abs(45 - d_mid)) %>% # MIDPOINT OF GSM DEPTH LAYER = 45
  group_by(site_id, d_dist_mid) %>% 
  tally() %>% 
  filter(n >= 2) %>% 
  .$site_id

# For SRS sampling design, can only have 1 observation per location and specified
# GSM depth layer. Therefore remove observations farther away from midpoint of 
# GSM depth layer, or average (mean) if distances are the same
if (any(duplicated(sf_predobs_val_30_60$site_id))) {
  sf_predobs_val_30_60 <- sf_predobs_val_30_60 %>% 
    mutate(d_dist_mid = abs(10 - d_mid)) %>% 
    group_by(site_id) %>% 
    # average observations and predictions at same location where both observations
    # have equal distance to GSM depth layer (see defined vector above)
    mutate(across(c(obs, pred_mean, quant_5, quant_50, quant_95),
                  ~ ifelse(site_id %in% v_d_dist_mid_eq_30_60, mean(.x), .x))) %>% 
    # select observation that is closest to midpoint of GSM depth layer
    slice(which.min(d_dist_mid)) %>% 
    # remove temporary grouping variable
    dplyr::select(-d_dist_mid) %>%
    # calculate new PI90 because may change slightly after averaging...
    mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                                 TRUE, FALSE))
} else {
  sf_predobs_val_30_60 <- sf_predobs_val_30_60
}

# vector of strata codes that only have one observation
v_str_code_1obs_30_60 <- sf_predobs_val_30_60 %>% 
  group_by(str_code) %>% 
  tally() %>% 
  filter(n == 1) %>% 
  .$str_code

# for upcoming calculations, we need to calculate residuals, stratum (str) weights
# and number of samples per str; we leave tibble grouped by str
sf_predobs_val_30_60 <- sf_predobs_val_30_60 %>% 
  mutate(res = obs - pred_mean, # residuals
         res_median = obs - quant_50, # residuals when using median prediction
         str_weight = str_area_m2/area_tot_m2) %>% # str weights based on area
  group_by(str_code) %>% 
  mutate(str_n = n()) # number of samples per str

# for str with only 1 observation, we estimate the within-str variance by taking
# the average of all within-str variances of all str with 2 or more observations;
# this mean variance has to be calculated for each metric separately
# Mean of all within str variances of Mean Error (ME = bias)
var_me_mean <- sf_predobs_val_30_60 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res - mean(res))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_me_median_mean <- sf_predobs_val_30_60 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res_median - mean(res_median))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of Mean Squared Error (MSE)
var_mse_mean <- sf_predobs_val_30_60 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res^2 - mean(res^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_mse_median_mean <- sf_predobs_val_30_60 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res_median^2 - mean(res_median^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of observations (for Var Y and MEC)
var_Y_mean <- sf_predobs_val_30_60 %>% 
  mutate(str_var_Y = str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2)) %>%
  slice(1L) %>% 
  pull(str_var_Y) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# calculate model metrics of probability sampling design
sf_predobs_val_30_60 <- sf_predobs_val_30_60 %>% 
  # weighted ME per str
  mutate(str_me = str_weight*mean(res),
         # within str variances of ME
         str_me_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_mean,
                             str_weight^2*(1/(str_n*(str_n-1)))*sum((res - mean(res))^2)),
         # weighted MSE per str
         str_mse = str_weight*mean(res^2),
         # within str variances of MSE
         str_mse_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((res^2 - mean(res^2))^2)),
         # same as above but now using median instead of mean predictions
         str_me_median = str_weight*mean(res_median),
         str_me_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_median_mean,
                                    str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median - mean(res_median))^2)),
         str_mse_median = str_weight*mean(res_median^2),
         str_mse_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_median_mean,
                                     str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median^2 - mean(res_median^2))^2)),
         # for MEC, first need unbiased estimator of spatial variance (var(Y);
         # see eqn. 7.16 in De Gruijter et al. 2006), where var(Y) is composed
         # of the following terms (without yet taking sum of all str; this is done in
         # "tbl_sum_stats_val" object below...):
         # 1) Y^2st 
         str_Y2st = str_weight*(sum((obs)^2)/str_n),
         # 2) Yst^2
         str_Yst2 = str_weight*mean(obs),
         # 3) v(Yst)
         str_var_Yst = ifelse(str_n == 1, var_Y_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2))
  )

# table of accuracy metrics for each str
tbl_accuracy_metrics_LSK_SRS_str_30_60 <- sf_predobs_val_30_60 %>% 
  dplyr::select(str_id:str_var_Yst) %>% 
  slice(1L) %>% # works since tibble is still grouped
  as_tibble()

# degrees of freedom = sample size minus # of str
df = nrow(sf_predobs_val_30_60) - length(unique(sf_predobs_val_30_60$str_code))

# validation accuracy metrics for 30-60 cm GSM depth layer over all strata
tbl_accuracy_metrics_LSK_SRS_30_60 <- tibble(
  n = length(sf_predobs_val_30_60$obs), # number of samples in this depth layer
  # ME metrics (ME, ME variance, 95% confidence intervals (CI95) of ME)
  me = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_me),
  me_var = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_me_var),
  me_ci_low = me - qt(0.975, df)*sqrt(me_var),
  me_ci_up = me + qt(0.975, df)*sqrt(me_var),
  # ME metrics when using median instead of mean predictions
  me_median = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_me_median),
  me_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_me_median_var),
  me_median_ci_low = me_median - qt(0.975, df)*sqrt(me_median_var),
  me_median_ci_up = me_median + qt(0.975, df)*sqrt(me_median_var),
  # MSE metrics
  mse = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_mse),
  mse_var = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_mse_var),
  mse_ci_low = mse - qt(0.975, df)*sqrt(mse_var),
  mse_ci_up = mse + qt(0.975, df)*sqrt(mse_var),
  # MSE metrics when using median instead of mean predictions
  mse_median = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_mse_median),
  mse_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_mse_median_var),
  mse_median_ci_low = mse_median - qt(0.975, df)*sqrt(mse_median_var),
  mse_median_ci_up = mse_median + qt(0.975, df)*sqrt(mse_median_var),
  # RMSE metrics
  rmse = sqrt(mse),
  rmse_ci_low = sqrt(mse_ci_low),
  rmse_ci_up = sqrt(mse_ci_up),
  # RMSE metrics when using median instead of mean predictions
  rmse_median = sqrt(mse_median),
  rmse_median_ci_low = sqrt(mse_median_ci_low),
  rmse_median_ci_up = sqrt(mse_median_ci_up),
  # for MEC, first need unbiased estimator of spatial variance (var(Y);
  # see eqn. 7.16 in De Gruijter et al. 2006)
  var_Y = sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_Y2st) -
    ((sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_Yst2))^2) +
    sum(tbl_accuracy_metrics_LSK_SRS_str_30_60$str_var_Yst),
  # using function of package "surveyplanning": result is different!?
  var_Y_pkg = s2(sf_predobs_val_30_60$obs, sf_predobs_val_30_60$str_weight),
  # MEC
  mec = 1 - mse/var_Y,
  # MEC when using median instead of mean predictions
  mec_median = 1 - mse_median/var_Y,
  # obs within PI90
  n_within_PI90 = nrow(filter(sf_predobs_val_30_60, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_30_60, within_PI90 %in% FALSE))/n)

# control randomness of bootstrapping for reproducibility
set.seed(2021)

# get bootstrap of MEC values
MEC_SRS_bootstrap_30_60 <- boot(data = sf_predobs_val_30_60,
                                statistic = fnc_MEC_SRS,
                                R = 1000,
                                strata = sf_predobs_val_30_60$str_code,
                                parallel = "multicore",
                                ncpus = parallel::detectCores())

# get CI95
MEC_SRS_90CI_30_60 <- boot.ci(boot.out = MEC_SRS_bootstrap_30_60,
                              conf = 0.95,
                              type = "perc")

# add CI95 of MEC to accuracy metrics tibble
tbl_accuracy_metrics_LSK_SRS_30_60 <- tbl_accuracy_metrics_LSK_SRS_30_60 %>% 
  add_column(mec_ci_low = MEC_SRS_90CI_30_60$percent[,4],
             mec_ci_up = MEC_SRS_90CI_30_60$percent[,5])

# Modify columns for plot annotation
tbl_accuracy_metrics_LSK_SRS_30_60_annotate <- tbl_accuracy_metrics_LSK_SRS_30_60 %>%
  add_column(one_one = "1:1") %>% 
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me_ci = as.character(as.expression(paste0(
           "ME = ", round(me, 2), " [", round(me_ci_low, 2),", ",
           round(me_ci_up, 2), "]"))),
         rmse_ci = as.character(as.expression(paste0(
           "RMSE = ", round(rmse, 2), " [", round(rmse_ci_low, 2),", ",
           round(rmse_ci_up, 2), "]"))),
         mec_ci = as.character(as.expression(paste0(
           "MEC = ", round(mec, 2), " [", round(mec_ci_low, 2),", ",
           round(mec_ci_up, 2), "]"))))

# validation accuracy plot for 30-60 cm GSM depth layer
p_pred_obs_val_30_60 <- sf_predobs_val_30_60 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_SRS_30_60_annotate$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_SRS_30_60_annotate$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_30_60_annotate,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_30_60_annotate,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_30_60_annotate,
            aes(x = Inf, y = -Inf, label = mec_ci), size = 3,
            hjust = 1.1, vjust = -5.75, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_30_60_annotate,
            aes(x = Inf, y = -Inf, label = rmse_ci), size = 3,
            hjust = 1.05, vjust = -3.5, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_30_60_annotate,
            aes(x = Inf, y = -Inf, label = me_ci), size = 3,
            hjust = 1.05, vjust = -1.4, parse = FALSE) +
  ylab(as.expression(TARGET_EXP)) +
  xlab(TARGET_PRED_ISRIC) +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# report which strata were excluded in spatial validation for this depth layer
(v_strata_excluded_30_60 = setdiff(unique(sf_predobs_val$str_code),
                                   unique(sf_predobs_val_30_60$str_code)))

# area excluded in spatial validation for this depth layer in m^2
area_excluded_30_60_PFB_CV = sf_predobs_val %>% 
  filter(str_code %in% v_strata_excluded_30_60) %>% 
  group_by(str_code) %>% 
  slice(1L) %>% 
  pull(str_area_m2) %>% 
  sum()

# area included as a percentage of total area [%]
(area_included_30_60_per = (area_tot_m2 - area_excluded_30_60_PFB_CV)/area_tot_m2 *100)



### 60-100cm: Accuracy plots and metrics ---------------------------------------

# LSK-SRS (ANALYSIS PREPARATION STARTS HERE)
# vector of locations where there are multiple observations with the same
# distance to the midpoint of the respective GSM depth layer
v_d_dist_mid_eq_60_100 <- sf_predobs_val_60_100 %>% 
  mutate(d_dist_mid = abs(80 - d_mid)) %>% # MIDPOINT OF GSM DEPTH LAYER = 80
  group_by(site_id, d_dist_mid) %>% 
  tally() %>% 
  filter(n >= 2) %>% 
  .$site_id

# For SRS sampling design, can only have 1 observation per location and specified
# GSM depth layer. Therefore remove observations farther away from midpoint of 
# GSM depth layer, or average (mean) if distances are the same
if (any(duplicated(sf_predobs_val_60_100$site_id))) {
  sf_predobs_val_60_100 <- sf_predobs_val_60_100 %>% 
    mutate(d_dist_mid = abs(10 - d_mid)) %>% 
    group_by(site_id) %>% 
    # average observations and predictions at same location where both observations
    # have equal distance to GSM depth layer (see defined vector above)
    mutate(across(c(obs, pred_mean, quant_5, quant_50, quant_95),
                  ~ ifelse(site_id %in% v_d_dist_mid_eq_60_100, mean(.x), .x))) %>% 
    # select observation that is closest to midpoint of GSM depth layer
    slice(which.min(d_dist_mid)) %>% 
    # remove temporary grouping variable
    dplyr::select(-d_dist_mid) %>%
    # calculate new PI90 because may change slightly after averaging...
    mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                                 TRUE, FALSE))
} else {
  sf_predobs_val_60_100 <- sf_predobs_val_60_100
}

# vector of strata codes that only have one observation
v_str_code_1obs_60_100 <- sf_predobs_val_60_100 %>% 
  group_by(str_code) %>% 
  tally() %>% 
  filter(n == 1) %>% 
  .$str_code

# for upcoming calculations, we need to calculate residuals, stratum (str) weights
# and number of samples per str; we leave tibble grouped by str
sf_predobs_val_60_100 <- sf_predobs_val_60_100 %>% 
  mutate(res = obs - pred_mean, # residuals
         res_median = obs - quant_50, # residuals when using median prediction
         str_weight = str_area_m2/area_tot_m2) %>% # str weights based on area
  group_by(str_code) %>% 
  mutate(str_n = n()) # number of samples per str

# for str with only 1 observation, we estimate the within-str variance by taking
# the average of all within-str variances of all str with 2 or more observations;
# this mean variance has to be calculated for each metric separately
# Mean of all within str variances of Mean Error (ME = bias)
var_me_mean <- sf_predobs_val_60_100 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res - mean(res))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_me_median_mean <- sf_predobs_val_60_100 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res_median - mean(res_median))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of Mean Squared Error (MSE)
var_mse_mean <- sf_predobs_val_60_100 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res^2 - mean(res^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_mse_median_mean <- sf_predobs_val_60_100 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res_median^2 - mean(res_median^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of observations (for Var Y and MEC)
var_Y_mean <- sf_predobs_val_60_100 %>% 
  mutate(str_var_Y = str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2)) %>%
  slice(1L) %>% 
  pull(str_var_Y) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# calculate model metrics of probability sampling design
sf_predobs_val_60_100 <- sf_predobs_val_60_100 %>% 
  # weighted ME per str
  mutate(str_me = str_weight*mean(res),
         # within str variances of ME
         str_me_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_mean,
                             str_weight^2*(1/(str_n*(str_n-1)))*sum((res - mean(res))^2)),
         # weighted MSE per str
         str_mse = str_weight*mean(res^2),
         # within str variances of MSE
         str_mse_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((res^2 - mean(res^2))^2)),
         # same as above but now using median instead of mean predictions
         str_me_median = str_weight*mean(res_median),
         str_me_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_median_mean,
                                    str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median - mean(res_median))^2)),
         str_mse_median = str_weight*mean(res_median^2),
         str_mse_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_median_mean,
                                     str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median^2 - mean(res_median^2))^2)),
         # for MEC, first need unbiased estimator of spatial variance (var(Y);
         # see eqn. 7.16 in De Gruijter et al. 2006), where var(Y) is composed
         # of the following terms (without yet taking sum of all str; this is done in
         # "tbl_sum_stats_val" object below...):
         # 1) Y^2st 
         str_Y2st = str_weight*(sum((obs)^2)/str_n),
         # 2) Yst^2
         str_Yst2 = str_weight*mean(obs),
         # 3) v(Yst)
         str_var_Yst = ifelse(str_n == 1, var_Y_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2))
  )

# table of accuracy metrics for each str
tbl_accuracy_metrics_LSK_SRS_str_60_100 <- sf_predobs_val_60_100 %>% 
  dplyr::select(str_id:str_var_Yst) %>% 
  slice(1L) %>% # works since tibble is still grouped
  as_tibble()

# degrees of freedom = sample size minus # of str
df = nrow(sf_predobs_val_60_100) - length(unique(sf_predobs_val_60_100$str_code))

# validation accuracy metrics for 60-100 cm GSM depth layer over all strata
tbl_accuracy_metrics_LSK_SRS_60_100 <- tibble(
  n = length(sf_predobs_val_60_100$obs), # number of samples in this depth layer
  # ME metrics (ME, ME variance, 95% confidence intervals (CI95) of ME)
  me = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_me),
  me_var = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_me_var),
  me_ci_low = me - qt(0.975, df)*sqrt(me_var),
  me_ci_up = me + qt(0.975, df)*sqrt(me_var),
  # ME metrics when using median instead of mean predictions
  me_median = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_me_median),
  me_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_me_median_var),
  me_median_ci_low = me_median - qt(0.975, df)*sqrt(me_median_var),
  me_median_ci_up = me_median + qt(0.975, df)*sqrt(me_median_var),
  # MSE metrics
  mse = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_mse),
  mse_var = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_mse_var),
  mse_ci_low = mse - qt(0.975, df)*sqrt(mse_var),
  mse_ci_up = mse + qt(0.975, df)*sqrt(mse_var),
  # MSE metrics when using median instead of mean predictions
  mse_median = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_mse_median),
  mse_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_mse_median_var),
  mse_median_ci_low = mse_median - qt(0.975, df)*sqrt(mse_median_var),
  mse_median_ci_up = mse_median + qt(0.975, df)*sqrt(mse_median_var),
  # RMSE metrics
  rmse = sqrt(mse),
  rmse_ci_low = sqrt(mse_ci_low),
  rmse_ci_up = sqrt(mse_ci_up),
  # RMSE metrics when using median instead of mean predictions
  rmse_median = sqrt(mse_median),
  rmse_median_ci_low = sqrt(mse_median_ci_low),
  rmse_median_ci_up = sqrt(mse_median_ci_up),
  # for MEC, first need unbiased estimator of spatial variance (var(Y);
  # see eqn. 7.16 in De Gruijter et al. 2006)
  var_Y = sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_Y2st) -
    ((sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_Yst2))^2) +
    sum(tbl_accuracy_metrics_LSK_SRS_str_60_100$str_var_Yst),
  # using function of package "surveyplanning": result is different!?
  var_Y_pkg = s2(sf_predobs_val_60_100$obs, sf_predobs_val_60_100$str_weight),
  # MEC
  mec = 1 - mse/var_Y,
  # MEC when using median instead of mean predictions
  mec_median = 1 - mse_median/var_Y,
  # obs within PI90
  n_within_PI90 = nrow(filter(sf_predobs_val_60_100, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_60_100, within_PI90 %in% FALSE))/n)

# control randomness of bootstrapping for reproducibility
set.seed(2021)

# get bootstrap of MEC values
MEC_SRS_bootstrap_60_100 <- boot(data = sf_predobs_val_60_100,
                                 statistic = fnc_MEC_SRS,
                                 R = 1000,
                                 strata = sf_predobs_val_60_100$str_code,
                                 parallel = "multicore",
                                 ncpus = parallel::detectCores())

# get CI95
MEC_SRS_90CI_60_100 <- boot.ci(boot.out = MEC_SRS_bootstrap_60_100,
                               conf = 0.95,
                               type = "perc")

# add CI95 of MEC to accuracy metrics tibble
tbl_accuracy_metrics_LSK_SRS_60_100 <- tbl_accuracy_metrics_LSK_SRS_60_100 %>% 
  add_column(mec_ci_low = MEC_SRS_90CI_60_100$percent[,4],
             mec_ci_up = MEC_SRS_90CI_60_100$percent[,5])

# Modify columns for plot annotation
tbl_accuracy_metrics_LSK_SRS_60_100_annotate <- tbl_accuracy_metrics_LSK_SRS_60_100 %>%
  add_column(one_one = "1:1") %>% 
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me_ci = as.character(as.expression(paste0(
           "ME = ", round(me, 2), " [", round(me_ci_low, 2),", ",
           round(me_ci_up, 2), "]"))),
         rmse_ci = as.character(as.expression(paste0(
           "RMSE = ", round(rmse, 2), " [", round(rmse_ci_low, 2),", ",
           round(rmse_ci_up, 2), "]"))),
         mec_ci = as.character(as.expression(paste0(
           "MEC = ", round(mec, 2), " [", round(mec_ci_low, 2),", ",
           round(mec_ci_up, 2), "]"))))

# validation accuracy plot for 60-100 cm GSM depth layer
p_pred_obs_val_60_100 <- sf_predobs_val_60_100 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_SRS_60_100_annotate$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_SRS_60_100_annotate$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_60_100_annotate,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_60_100_annotate,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_60_100_annotate,
            aes(x = Inf, y = -Inf, label = mec_ci), size = 3,
            hjust = 1.1, vjust = -5.75, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_60_100_annotate,
            aes(x = Inf, y = -Inf, label = rmse_ci), size = 3,
            hjust = 1.05, vjust = -3.5, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_60_100_annotate,
            aes(x = Inf, y = -Inf, label = me_ci), size = 3,
            hjust = 1.05, vjust = -1.4, parse = FALSE) +
  ylab(as.expression(TARGET_EXP)) +
  xlab(TARGET_PRED_ISRIC) +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# report which strata were excluded in spatial validation for this depth layer
(v_strata_excluded_60_100 = setdiff(unique(sf_predobs_val$str_code),
                                    unique(sf_predobs_val_60_100$str_code)))

# area excluded in spatial validation for this depth layer in m^2
area_excluded_60_100_PFB_CV = sf_predobs_val %>% 
  filter(str_code %in% v_strata_excluded_60_100) %>% 
  group_by(str_code) %>% 
  slice(1L) %>% 
  pull(str_area_m2) %>% 
  sum()

# area included as a percentage of total area [%]
(area_included_60_100_per = (area_tot_m2 - area_excluded_60_100_PFB_CV)/area_tot_m2 *100)



### 100-200cm: Accuracy plots and metrics --------------------------------------

# LSK-SRS (ANALYSIS PREPARATION STARTS HERE)
# vector of locations where there are multiple observations with the same
# distance to the midpoint of the respective GSM depth layer
v_d_dist_mid_eq_100_200 <- sf_predobs_val_100_200 %>% 
  mutate(d_dist_mid = abs(150 - d_mid)) %>% # MIDPOINT OF GSM DEPTH LAYER = 150
  group_by(site_id, d_dist_mid) %>% 
  tally() %>% 
  filter(n >= 2) %>% 
  .$site_id

# For SRS sampling design, can only have 1 observation per location and specified
# GSM depth layer. Therefore remove observations farther away from midpoint of 
# GSM depth layer, or average (mean) if distances are the same
if (any(duplicated(sf_predobs_val_100_200$site_id))) {
  sf_predobs_val_100_200 <- sf_predobs_val_100_200 %>% 
    mutate(d_dist_mid = abs(10 - d_mid)) %>% 
    group_by(site_id) %>% 
    # average observations and predictions at same location where both observations
    # have equal distance to GSM depth layer (see defined vector above)
    mutate(across(c(obs, pred_mean, quant_5, quant_50, quant_95),
                  ~ ifelse(site_id %in% v_d_dist_mid_eq_100_200, mean(.x), .x))) %>% 
    # select observation that is closest to midpoint of GSM depth layer
    slice(which.min(d_dist_mid)) %>% 
    # remove temporary grouping variable
    dplyr::select(-d_dist_mid) %>%
    # calculate new PI90 because may change slightly after averaging...
    mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                                 TRUE, FALSE))
} else {
  sf_predobs_val_100_200 <- sf_predobs_val_100_200
}

# vector of strata codes that only have one observation
v_str_code_1obs_100_200 <- sf_predobs_val_100_200 %>% 
  group_by(str_code) %>% 
  tally() %>% 
  filter(n == 1) %>% 
  .$str_code

# for upcoming calculations, we need to calculate residuals, stratum (str) weights
# and number of samples per str; we leave tibble grouped by str
sf_predobs_val_100_200 <- sf_predobs_val_100_200 %>% 
  mutate(res = obs - pred_mean, # residuals
         res_median = obs - quant_50, # residuals when using median prediction
         str_weight = str_area_m2/area_tot_m2) %>% # str weights based on area
  group_by(str_code) %>% 
  mutate(str_n = n()) # number of samples per str

# for str with only 1 observation, we estimate the within-str variance by taking
# the average of all within-str variances of all str with 2 or more observations;
# this mean variance has to be calculated for each metric separately
# Mean of all within str variances of Mean Error (ME = bias)
var_me_mean <- sf_predobs_val_100_200 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res - mean(res))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_me_median_mean <- sf_predobs_val_100_200 %>% 
  mutate(str_var_me = 1/(str_n-1)*sum((res_median - mean(res_median))^2)) %>%
  slice(1L) %>% 
  pull(str_var_me) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of Mean Squared Error (MSE)
var_mse_mean <- sf_predobs_val_100_200 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res^2 - mean(res^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation
# when using median instead of mean predictions
var_mse_median_mean <- sf_predobs_val_100_200 %>% 
  mutate(str_var_mse = 1/(str_n-1)*sum((res_median^2 - mean(res_median^2))^2)) %>%
  slice(1L) %>% 
  pull(str_var_mse) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# Mean of all within str variances of observations (for Var Y and MEC)
var_Y_mean <- sf_predobs_val_100_200 %>% 
  mutate(str_var_Y = str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2)) %>%
  slice(1L) %>% 
  pull(str_var_Y) %>% 
  mean(na.rm = TRUE) # NAs are from str with only 1 observation

# calculate model metrics of probability sampling design
sf_predobs_val_100_200 <- sf_predobs_val_100_200 %>% 
  # weighted ME per str
  mutate(str_me = str_weight*mean(res),
         # within str variances of ME
         str_me_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_mean,
                             str_weight^2*(1/(str_n*(str_n-1)))*sum((res - mean(res))^2)),
         # weighted MSE per str
         str_mse = str_weight*mean(res^2),
         # within str variances of MSE
         str_mse_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((res^2 - mean(res^2))^2)),
         # same as above but now using median instead of mean predictions
         str_me_median = str_weight*mean(res_median),
         str_me_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_me_median_mean,
                                    str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median - mean(res_median))^2)),
         str_mse_median = str_weight*mean(res_median^2),
         str_mse_median_var = ifelse(str_n == 1, str_weight^2*(1/str_n)*var_mse_median_mean,
                                     str_weight^2*(1/(str_n*(str_n-1)))*sum((res_median^2 - mean(res_median^2))^2)),
         # for MEC, first need unbiased estimator of spatial variance (var(Y);
         # see eqn. 7.16 in De Gruijter et al. 2006), where var(Y) is composed
         # of the following terms (without yet taking sum of all str; this is done in
         # "tbl_sum_stats_val" object below...):
         # 1) Y^2st 
         str_Y2st = str_weight*(sum((obs)^2)/str_n),
         # 2) Yst^2
         str_Yst2 = str_weight*mean(obs),
         # 3) v(Yst)
         str_var_Yst = ifelse(str_n == 1, var_Y_mean,
                              str_weight^2*(1/(str_n*(str_n-1)))*sum((obs - mean(obs))^2))
  )

# table of accuracy metrics for each str
tbl_accuracy_metrics_LSK_SRS_str_100_200 <- sf_predobs_val_100_200 %>% 
  dplyr::select(str_id:str_var_Yst) %>% 
  slice(1L) %>% # works since tibble is still grouped
  as_tibble()

# degrees of freedom = sample size minus # of str
df = nrow(sf_predobs_val_100_200) - length(unique(sf_predobs_val_100_200$str_code))

# validation accuracy metrics for 100-200 cm GSM depth layer over all strata
tbl_accuracy_metrics_LSK_SRS_100_200 <- tibble(
  n = length(sf_predobs_val_100_200$obs), # number of samples in this depth layer
  # ME metrics (ME, ME variance, 95% confidence intervals (CI95) of ME)
  me = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_me),
  me_var = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_me_var),
  me_ci_low = me - qt(0.975, df)*sqrt(me_var),
  me_ci_up = me + qt(0.975, df)*sqrt(me_var),
  # ME metrics when using median instead of mean predictions
  me_median = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_me_median),
  me_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_me_median_var),
  me_median_ci_low = me_median - qt(0.975, df)*sqrt(me_median_var),
  me_median_ci_up = me_median + qt(0.975, df)*sqrt(me_median_var),
  # MSE metrics
  mse = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_mse),
  mse_var = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_mse_var),
  mse_ci_low = mse - qt(0.975, df)*sqrt(mse_var),
  mse_ci_up = mse + qt(0.975, df)*sqrt(mse_var),
  # MSE metrics when using median instead of mean predictions
  mse_median = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_mse_median),
  mse_median_var = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_mse_median_var),
  mse_median_ci_low = mse_median - qt(0.975, df)*sqrt(mse_median_var),
  mse_median_ci_up = mse_median + qt(0.975, df)*sqrt(mse_median_var),
  # RMSE metrics
  rmse = sqrt(mse),
  rmse_ci_low = sqrt(mse_ci_low),
  rmse_ci_up = sqrt(mse_ci_up),
  # RMSE metrics when using median instead of mean predictions
  rmse_median = sqrt(mse_median),
  rmse_median_ci_low = sqrt(mse_median_ci_low),
  rmse_median_ci_up = sqrt(mse_median_ci_up),
  # for MEC, first need unbiased estimator of spatial variance (var(Y);
  # see eqn. 7.16 in De Gruijter et al. 2006)
  var_Y = sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_Y2st) -
    ((sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_Yst2))^2) +
    sum(tbl_accuracy_metrics_LSK_SRS_str_100_200$str_var_Yst),
  # using function of package "surveyplanning": result is different!?
  var_Y_pkg = s2(sf_predobs_val_100_200$obs, sf_predobs_val_100_200$str_weight),
  # MEC
  mec = 1 - mse/var_Y,
  # MEC when using median instead of mean predictions
  mec_median = 1 - mse_median/var_Y,
  # obs within PI90
  n_within_PI90 = nrow(filter(sf_predobs_val_100_200, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_100_200, within_PI90 %in% FALSE))/n)

# control randomness of bootstrapping for reproducibility
set.seed(2021)

# get bootstrap of MEC values
MEC_SRS_bootstrap_100_200 <- boot(data = sf_predobs_val_100_200,
                                  statistic = fnc_MEC_SRS,
                                  R = 1000,
                                  strata = sf_predobs_val_100_200$str_code,
                                  parallel = "multicore",
                                  ncpus = parallel::detectCores())

# get CI95
MEC_SRS_90CI_100_200 <- boot.ci(boot.out = MEC_SRS_bootstrap_100_200,
                                conf = 0.95,
                                type = "perc")

# add CI95 of MEC to accuracy metrics tibble
tbl_accuracy_metrics_LSK_SRS_100_200 <- tbl_accuracy_metrics_LSK_SRS_100_200 %>% 
  add_column(mec_ci_low = MEC_SRS_90CI_100_200$percent[,4],
             mec_ci_up = MEC_SRS_90CI_100_200$percent[,5])

# Modify columns for plot annotation
tbl_accuracy_metrics_LSK_SRS_100_200_annotate <- tbl_accuracy_metrics_LSK_SRS_100_200 %>%
  add_column(one_one = "1:1") %>% 
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me_ci = as.character(as.expression(paste0(
           "ME = ", round(me, 2), " [", round(me_ci_low, 2),", ",
           round(me_ci_up, 2), "]"))),
         rmse_ci = as.character(as.expression(paste0(
           "RMSE = ", round(rmse, 2), " [", round(rmse_ci_low, 2),", ",
           round(rmse_ci_up, 2), "]"))),
         mec_ci = as.character(as.expression(paste0(
           "MEC = ", round(mec, 2), " [", round(mec_ci_low, 2),", ",
           round(mec_ci_up, 2), "]"))))

# validation accuracy plot for 100-200 cm GSM depth layer
p_pred_obs_val_100_200 <- sf_predobs_val_100_200 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_SRS_100_200_annotate$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_SRS_100_200_annotate$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_100_200_annotate,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_100_200_annotate,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_100_200_annotate,
            aes(x = Inf, y = -Inf, label = mec_ci), size = 3,
            hjust = 1.1, vjust = -5.75, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_100_200_annotate,
            aes(x = Inf, y = -Inf, label = rmse_ci), size = 3,
            hjust = 1.05, vjust = -3.5, parse = FALSE) +
  geom_text(data = tbl_accuracy_metrics_LSK_SRS_100_200_annotate,
            aes(x = Inf, y = -Inf, label = me_ci), size = 3,
            hjust = 1.05, vjust = -1.4, parse = FALSE) +
  ylab(as.expression(TARGET_EXP)) +
  xlab(TARGET_PRED_ISRIC) +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# report which strata were excluded in spatial validation for this depth layer
(v_strata_excluded_100_200 = setdiff(unique(sf_predobs_val$str_code),
                                     unique(sf_predobs_val_100_200$str_code)))

# area excluded in spatial validation for this depth layer in m^2
area_excluded_100_200_PFB_CV = sf_predobs_val %>% 
  filter(str_code %in% v_strata_excluded_100_200) %>% 
  group_by(str_code) %>% 
  slice(1L) %>% 
  pull(str_area_m2) %>% 
  sum()

# area included as a percentage of total area [%]
(area_included_100_200_per = (area_tot_m2 - area_excluded_100_200_PFB_CV)/area_tot_m2 *100)



### Accuracy metrics over depth LSK-SRS summary table --------------------------

# rowbind cols of sum stats of all GSM depth increments
# because 0-5cm is empty, designate first row as NA
tbl_accuracy_metrics_LSK_SRS_d <- bind_rows(tbl_accuracy_metrics_LSK_SRS_5_15 %>% 
                                              mutate_all(~na_if(., .)),
                                            tbl_accuracy_metrics_LSK_SRS_5_15,
                                            tbl_accuracy_metrics_LSK_SRS_15_30,
                                            tbl_accuracy_metrics_LSK_SRS_30_60,
                                            tbl_accuracy_metrics_LSK_SRS_60_100,
                                            tbl_accuracy_metrics_LSK_SRS_100_200) %>%
  add_column(d_increment = c(as.character("0 cm to 5 cm"),
                             as.character("5 cm to 15 cm"),
                             as.character("15 cm to 30 cm"),
                             as.character("30 cm to 60 cm"),
                             as.character("60 cm to 100 cm"),
                             as.character("100 cm to 200 cm")),
             .before = "n")

# save LSK-SRS per GSM depth layer as table (.csv)
write_csv(tbl_accuracy_metrics_LSK_SRS_d %>%
            mutate_at(vars(contains("_var")), ~round(., 5)) %>%
            mutate_at(vars(-contains("_var"), -c(d_increment:n)), ~round(., 2)),
          paste0("out/maps/other/SoilGrids_v2.0/SoilGrids_", TARGET_ISRIC, "_model_evaluation_LSK_SRS_d.csv"))



### Evaluate validation model results spatially (2D) ---------------------------

# skip 0-5cm because only 1 observation in LSK

# calculate residuals
sf_predobs_val_5_15 <- sf_predobs_val_5_15 %>% 
  mutate(res = obs - pred_mean,
         # add proportion values of PICP of PI90 to PI90 cols
         within_PI90 = case_when(
           within_PI90 %in% TRUE ~ paste0(
             'TRUE (',round(tbl_accuracy_metrics_LSK_SRS_5_15$n_within_PI90, 3), ')'),
           within_PI90 %in% FALSE ~ paste0(
             'FALSE (', round(tbl_accuracy_metrics_LSK_SRS_5_15$n_within_PI90_not, 3), ')')))

# assign levels to logical col so that "FALSE" colors are easier to see on plots
logical_order <- unique(sf_predobs_val_5_15$within_PI90)
sf_predobs_val_5_15$within_PI90 <- factor(
  x = sf_predobs_val_5_15$within_PI90,
  levels = logical_order)

# calculate residuals
sf_predobs_val_15_30 <- sf_predobs_val_15_30 %>% 
  mutate(res = obs - pred_mean,
         # add proportion values of PICP of PI90 to PI90 cols
         within_PI90 = case_when(
           within_PI90 %in% TRUE ~ paste0(
             'TRUE (',round(tbl_accuracy_metrics_LSK_SRS_15_30$n_within_PI90, 3), ')'),
           within_PI90 %in% FALSE ~ paste0(
             'FALSE (', round(tbl_accuracy_metrics_LSK_SRS_15_30$n_within_PI90_not, 3), ')')))

# assign levels to logical col so that "FALSE" colors are easier to see on plots
logical_order <- unique(sf_predobs_val_15_30$within_PI90)
sf_predobs_val_15_30$within_PI90 <- factor(
  x = sf_predobs_val_15_30$within_PI90,
  levels = logical_order)

# calculate residuals
sf_predobs_val_30_60 <- sf_predobs_val_30_60 %>% 
  mutate(res = obs - pred_mean,
         # add proportion values of PICP of PI90 to PI90 cols
         within_PI90 = case_when(
           within_PI90 %in% TRUE ~ paste0(
             'TRUE (',round(tbl_accuracy_metrics_LSK_SRS_30_60$n_within_PI90, 3), ')'),
           within_PI90 %in% FALSE ~ paste0(
             'FALSE (', round(tbl_accuracy_metrics_LSK_SRS_30_60$n_within_PI90_not, 3), ')')))

# assign levels to logical col so that "FALSE" colors are easier to see on plots
logical_order <- unique(sf_predobs_val_30_60$within_PI90)
sf_predobs_val_30_60$within_PI90 <- factor(
  x = sf_predobs_val_30_60$within_PI90,
  levels = logical_order)

# calculate residuals
sf_predobs_val_60_100 <- sf_predobs_val_60_100 %>% 
  mutate(res = obs - pred_mean,
         # add proportion values of PICP of PI90 to PI90 cols
         within_PI90 = case_when(
           within_PI90 %in% TRUE ~ paste0(
             'TRUE (',round(tbl_accuracy_metrics_LSK_SRS_60_100$n_within_PI90, 3), ')'),
           within_PI90 %in% FALSE ~ paste0(
             'FALSE (', round(tbl_accuracy_metrics_LSK_SRS_60_100$n_within_PI90_not, 3), ')')))

# assign levels to logical col so that "FALSE" colors are easier to see on plots
logical_order <- unique(sf_predobs_val_60_100$within_PI90)
sf_predobs_val_60_100$within_PI90 <- factor(
  x = sf_predobs_val_60_100$within_PI90,
  levels = logical_order)

# calculate residuals
sf_predobs_val_100_200 <- sf_predobs_val_100_200 %>% 
  mutate(res = obs - pred_mean,
         # add proportion values of PICP of PI90 to PI90 cols
         within_PI90 = case_when(
           within_PI90 %in% TRUE ~ paste0(
             'TRUE (',round(tbl_accuracy_metrics_LSK_SRS_100_200$n_within_PI90, 3), ')'),
           within_PI90 %in% FALSE ~ paste0(
             'FALSE (', round(tbl_accuracy_metrics_LSK_SRS_100_200$n_within_PI90_not, 3), ')')))

# assign levels to logical col so that "FALSE" colors are easier to see on plots
logical_order <- unique(sf_predobs_val_100_200$within_PI90)
sf_predobs_val_100_200$within_PI90 <- factor(
  x = sf_predobs_val_100_200$within_PI90,
  levels = logical_order)


## 5-15cm: Map residuals & PI90 ------------------------------------------------

# set min and max centered around 0
limits = max(abs(sf_predobs_val_5_15$res)) * c(-1, 1)

# residuals
m_5_15_res <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_5_15, aes(fill = res),
          pch = 21, colour = "black") +
  geom_text(aes(x = Inf, y = -Inf, label = tbl_accuracy_metrics_LSK_SRS_5_15_annotate$n),
            size = 3, hjust = 8.5, vjust = -28, parse = TRUE) +
  theme(legend.position = c(0.1, 0.85),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Residuals") +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = limits) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
# within PI90
m_5_15_PI <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_5_15,
          aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_shape_manual(values = c(21, 4)) +
  theme(legend.position = c(0.1, 0.91),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# add the title
title_5_15 <- ggdraw() + 
  draw_label(paste0(TARGET_EXP, ": 5 cm to 15 cm depth"),
             fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
  # add margin on the left of the drawing canvas,
  # so title is aligned with left edge of first plot
  theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_5_15 <- plot_grid(title_5_15,
                    plot_grid(m_5_15_res,
                              m_5_15_PI,
                              align = "hv", nrow = 1, ncol = 2),
                    nrow = 2,
                    rel_heights = c(0.1, 1))

# save plot to disk
# ggsave(filename = paste0("m_", TARGET, "_val_res_PI90_5_15.pdf"),
#        plot = m_5_15,
#        path = paste0("out/figs/models/", TARGET),
#        width = 10, height = 5)

# interactive mapview maps
# residuals
mapview(sf_predobs_val_5_15,
        zcol = "res",
        layer.name = paste0(TARGET_EXP, " residuals: 5 cm to 15 cm"),
        col.regions = brewer.pal(11, "RdBu"),
        legend = TRUE,
        viewer.suppress = FALSE)
# within PI90
mapview(sf_predobs_val_5_15 %>% 
          mutate(within_PI90 = as.character(within_PI90)),
        zcol = "within_PI90",
        layer.name = paste0(TARGET_EXP, " within PI90: 5 cm to 15 cm"),
        col.regions = c("#ef8a62", "#67a9cf"),
        legend = TRUE,
        viewer.suppress = FALSE)


## 15-30cm: Map residuals & PI90 -----------------------------------------------

# set min and max centered around 0
limits = max(abs(sf_predobs_val_15_30$res)) * c(-1, 1)

# residuals
m_15_30_res <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_15_30, aes(fill = res),
          pch = 21, colour = "black") +
  geom_text(aes(x = Inf, y = -Inf, label = tbl_accuracy_metrics_LSK_SRS_15_30_annotate$n),
            size = 3, hjust = 8.5, vjust = -28, parse = TRUE) +
  theme(legend.position = c(0.1, 0.85),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Residuals") +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = limits) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
# within PI90
m_15_30_PI <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_15_30,
          aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_shape_manual(values = c(21, 4)) +
  theme(legend.position = c(0.1, 0.91),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# add the title
title_15_30 <- ggdraw() + 
  draw_label(paste0(TARGET_EXP, ": 15 to 30 cm depth"),
             fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
  # add margin on the left of the drawing canvas,
  # so title is aligned with left edge of first plot
  theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_15_30 <- plot_grid(title_15_30,
                     plot_grid(m_15_30_res,
                               m_15_30_PI,
                               align = "hv", nrow = 1, ncol = 2),
                     nrow = 2,
                     rel_heights = c(0.1, 1))

# save plot to disk
# ggsave(filename = paste0("m_", TARGET, "_val_res_PI90_15_30.pdf"),
#        plot = m_15_30,
#        path = paste0("out/figs/models/", TARGET),
#        width = 10, height = 5)

# interactive mapview maps
# residuals
mapview(sf_predobs_val_15_30,
        zcol = "res",
        layer.name = paste0(TARGET_EXP, " residuals: 15 to 30 cm"),
        col.regions = brewer.pal(11, "RdBu"),
        legend = TRUE,
        viewer.suppress = FALSE)
# within PI90
mapview(sf_predobs_val_15_30 %>% 
          mutate(within_PI90 = as.character(within_PI90)),
        zcol = "within_PI90",
        layer.name = paste0(TARGET_EXP, " within PI90: 15 to 30 cm"),
        col.regions = c("#ef8a62", "#67a9cf"),
        legend = TRUE,
        viewer.suppress = FALSE)


## 30-60cm: Map residuals & PI90 -----------------------------------------------

# set min and max centered around 0
limits = max(abs(sf_predobs_val_30_60$res)) * c(-1, 1)

# residuals
m_30_60_res <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_30_60, aes(fill = res),
          pch = 21, colour = "black") +
  geom_text(aes(x = Inf, y = -Inf, label = tbl_accuracy_metrics_LSK_SRS_30_60_annotate$n),
            size = 3, hjust = 7.5, vjust = -28, parse = TRUE) +
  theme(legend.position = c(0.1, 0.85),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Residuals") +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = limits) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
# within PI90
m_30_60_PI <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_30_60,
          aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_shape_manual(values = c(21, 4)) +
  theme(legend.position = c(0.1, 0.91),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# add the title
title_30_60 <- ggdraw() + 
  draw_label(paste0(TARGET_EXP, ": 30 to 60 cm depth"),
             fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
  # add margin on the left of the drawing canvas,
  # so title is aligned with left edge of first plot
  theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_30_60 <- plot_grid(title_30_60,
                     plot_grid(m_30_60_res,
                               m_30_60_PI,
                               align = "hv", nrow = 1, ncol = 2),
                     nrow = 2,
                     rel_heights = c(0.1, 1))

# save plot to disk
# ggsave(filename = paste0("m_", TARGET, "_val_res_PI90_30_60.pdf"),
#        plot = m_30_60,
#        path = paste0("out/figs/models/", TARGET),
#        width = 10, height = 5)

# interactive mapview maps
# residuals
mapview(sf_predobs_val_30_60,
        zcol = "res",
        layer.name = paste0(TARGET_EXP, " residuals: 30 to 60 cm"),
        col.regions = brewer.pal(11, "RdBu"),
        legend = TRUE,
        viewer.suppress = FALSE)
# within PI90
mapview(sf_predobs_val_30_60 %>% 
          mutate(within_PI90 = as.character(within_PI90)),
        zcol = "within_PI90",
        layer.name = paste0(TARGET_EXP, " within PI90: 30 to 60 cm"),
        col.regions = c("#ef8a62", "#67a9cf"),
        legend = TRUE,
        viewer.suppress = FALSE)


## 60-100cm: Map residuals & PI90 ----------------------------------------------

# set min and max centered around 0
limits = max(abs(sf_predobs_val_60_100$res)) * c(-1, 1)

# residuals
m_60_100_res <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_60_100, aes(fill = res),
          pch = 21, colour = "black") +
  geom_text(aes(x = Inf, y = -Inf, label = tbl_accuracy_metrics_LSK_SRS_60_100_annotate$n),
            size = 3, hjust = 7.5, vjust = -28, parse = TRUE) +
  theme(legend.position = c(0.1, 0.85),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Residuals") +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = limits) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
# within PI90
m_60_100_PI <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_60_100,
          aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_shape_manual(values = c(21, 4)) +
  theme(legend.position = c(0.1, 0.91),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# add the title
title_60_100 <- ggdraw() + 
  draw_label(paste0(TARGET_EXP, ": 60 to 100 cm depth"),
             fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
  # add margin on the left of the drawing canvas,
  # so title is aligned with left edge of first plot
  theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_60_100 <- plot_grid(title_60_100,
                      plot_grid(m_60_100_res,
                                m_60_100_PI,
                                align = "hv", nrow = 1, ncol = 2),
                      nrow = 2,
                      rel_heights = c(0.1, 1))

# save plot to disk
# ggsave(filename = paste0("m_", TARGET, "_val_res_PI90_60_100.pdf"),
#        plot = m_60_100,
#        path = paste0("out/figs/models/", TARGET),
#        width = 10, height = 5)

# interactive mapview maps
# residuals
mapview(sf_predobs_val_60_100,
        zcol = "res",
        layer.name = paste0(TARGET_EXP, " residuals: 60 to 100 cm"),
        col.regions = brewer.pal(11, "RdBu"),
        legend = TRUE,
        viewer.suppress = FALSE)
# within PI90
mapview(sf_predobs_val_60_100 %>% 
          mutate(within_PI90 = as.character(within_PI90)),
        zcol = "within_PI90",
        layer.name = paste0(TARGET_EXP, " within PI90: 60 to 100 cm"),
        col.regions = c("#ef8a62", "#67a9cf"),
        legend = TRUE,
        viewer.suppress = FALSE)


## 100-200cm: Map residuals & PI90 ---------------------------------------------

# set min and max centered around 0
limits = max(abs(sf_predobs_val_100_200$res)) * c(-1, 1)

# residuals
m_100_200_res <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_100_200, aes(fill = res),
          pch = 21, colour = "black") +
  geom_text(aes(x = Inf, y = -Inf, label = tbl_accuracy_metrics_LSK_SRS_100_200_annotate$n),
            size = 3, hjust = 8.5, vjust = -28, parse = TRUE) +
  theme(legend.position = c(0.1, 0.85),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Residuals") +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = limits) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
# within PI90
m_100_200_PI <- ggplot() +
  theme_bw() +
  geom_sf(data = sf_NL_borders) +
  geom_sf(data = sf_predobs_val_100_200,
          aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_shape_manual(values = c(21, 4)) +
  theme(legend.position = c(0.1, 0.91),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")

# add the title
title_100_200 <- ggdraw() + 
  draw_label(paste0(TARGET_EXP, ": 100 to 200 cm depth"),
             fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
  # add margin on the left of the drawing canvas,
  # so title is aligned with left edge of first plot
  theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_100_200 <- plot_grid(title_100_200,
                       plot_grid(m_100_200_res,
                                 m_100_200_PI,
                                 align = "hv", nrow = 1, ncol = 2),
                       nrow = 2,
                       rel_heights = c(0.1, 1))

# save plot to disk
# ggsave(filename = paste0("m_", TARGET, "_val_res_PI90_100_200.pdf"),
#        plot = m_100_200,
#        path = paste0("out/figs/models/", TARGET),
#        width = 10, height = 5)

# interactive mapview maps
# residuals
mapview(sf_predobs_val_100_200,
        zcol = "res",
        layer.name = paste0(TARGET_EXP, " residuals: 100 to 200 cm"),
        col.regions = brewer.pal(11, "RdBu"),
        legend = TRUE,
        viewer.suppress = FALSE)
# within PI90
mapview(sf_predobs_val_100_200 %>% 
          mutate(within_PI90 = as.character(within_PI90)),
        zcol = "within_PI90",
        layer.name = paste0(TARGET_EXP, " within PI90: 100 to 200 cm"),
        col.regions = c("#ef8a62", "#67a9cf"),
        legend = TRUE,
        viewer.suppress = FALSE)


