#------------------------------------------------------------------------------
# Name:     60_predict_QRF_soil_maps.R
#           (QRF = quantile regression forest)
#
# Content:  - define target variable (response) and target prediction depth
#           - read in fitted quantile regression forest (QRF) model
#           - read in stack of covariates (predictors)
#           - predict response mean and quantiles (e.g. 50th/median, 5th & 95th
#             quantiles) using terra::predict and ranger::predict arguments
#           - calculate 90% prediction interval (PI90) & GSM accuracy thresholds
#           - visualize maps and write rasters of results to disk
#           
# Inputs:   - Fitted QRF: out/data/model/QRF_fit_[response]_obs[]_p[].Rds
#           - Stack of rasters used for model fitting:
#             out/data/covariates/final_stack/
#
# Output:   - target soil property (response) prediction maps (quantiles,
#             prediction intervals and GSM accuracy thresholds):
#             out/maps/target/[target]/GeoTIFFs
#
# Runtime:  - approx. 5.5 hr entire script
#             (for 16K obs, 195 p, 48 cores, 500 trees)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  May 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages -------------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "terra", "ranger", "foreach", "doParallel",
          "rasterVis")
lapply(pkgs, library, character.only = TRUE)



### Designate script parameters ------------------------------------------------

# 1) Specify DSM target soil property:
TARGET = "pH_KCl"

# 2) Specify GlobalSoilMap (GSM) target prediction depth [cm]
# (see out/data/covariates/target_GSM_depths):
D_UPPER = "d_100"; D_UPPER_NUM = 100
D_MID = "d_100_200_mid"; D_MID_NUM = 150
D_LOWER = "d_200"; D_LOWER_NUM = 200

# 3) Specify which (previously fitted) model to use to predict new data:
QRF_FIT <- read_rds("out/data/model/QRF_fit_pH_KCl_obs15338_p195_LLO_10FCV_optimal.Rds")

# summary of calibrated model (model fit)
QRF_FIT

# 4) Specify number of cores to use for parallel computation
cores = detectCores()
CL = makeCluster(cores)
registerDoParallel(CL) # set up parallel backend to use multiple cores



### Read in prediction raster stack & remove all but 1 depth increment ---------

# locate, read in and stack covariates to predict over
v_dir_cov <- dir("out/data/covariates/final_stack",
                 pattern = "\\.grd$", recursive = TRUE)

ls_r_cov <- foreach(cov = 1:length(v_dir_cov)) %do%
  raster(paste0("out/data/covariates/final_stack/", v_dir_cov[[cov]]))

r_stack_cov <- stack(ls_r_cov)

# locate, read in and stack rasters containing target GSM prediction depths
v_dir_depth <- dir("out/data/covariates/target_GSM_depths",
                   recursive = TRUE)

ls_r_depth <- foreach(d = 1:length(v_dir_depth)) %do%
  raster(paste0("out/data/covariates/target_GSM_depths/", v_dir_depth[[d]]))

r_stack_depth <- stack(ls_r_depth)

# at the moment, raster stack still contains all possible GSM depth increments 
v_depth <- names(r_stack_depth)

# remove the upper, midpoint and lower depth boundary from layers that will be dropped
v_depth_drop <- v_depth[!v_depth %in% c(D_UPPER, D_MID, D_LOWER)]

# drop other depth layers
r_stack_depth <- dropLayer(r_stack_depth, v_depth_drop)

# rename to same names as used for QRF model calibration
names(r_stack_depth) <- names(r_stack_depth) %>%
    stringr::str_replace(., paste0(D_UPPER, "$"), "d_upper") %>% 
    stringr::str_replace(., paste0(D_MID, "$"), "d_mid") %>%
    stringr::str_replace(., paste0(D_LOWER, "$"), "d_lower")

# combine covariates with GSM depth layers
r_stack_fit <- raster::stack(r_stack_cov, r_stack_depth)

# remove covariates that were not used to fit model (e.g. if factors had > 32 levels...)
r_stack_fit <- r_stack_fit[[QRF_FIT$finalModel$forest$independent.variable.names]]



### Predict target soil property using new data (entire NL) --------------------

# predict mean prediction for entire NL
system.time(
  r_qrf_pred_mean <- terra::predict(
    object = r_stack_fit,
    model = QRF_FIT$finalModel,
    type = "response",
    seed = 2021, # to control randomness
    #num.threads = 10L, # to not overload RAM
    fun = function(model, ...) predict(model, ...)$predictions,
    na.rm = TRUE,
    cores = CL,
    progress = "text")
)
# time elapse: 54 min

# predict 5th, 50th and 95th quantiles for entire NL
system.time(
  r_qrf_pred_quant <- terra::predict(
    object = r_stack_fit,
    model = QRF_FIT$finalModel,
    type = "quantiles",
    quantiles = c(0.05, 0.5, 0.95),
    seed = 2021, # to control randomness
    #num.threads = 10L, # to not overload RAM
    fun = function(model, ...) predict(model, ...)$predictions,
    index = c(1, 2, 3),
    na.rm = TRUE,
    cores = CL,
    progress = "text")
)
# time elapse: 4.6 hours

# name rasters
names(r_qrf_pred_mean) <- names(r_qrf_pred_mean) %>%
  stringr::str_replace(., "layer", "pred_mean")

names(r_qrf_pred_quant) <- names(r_qrf_pred_quant) %>%
  stringr::str_replace(., "layer.1", "pred5") %>% 
  stringr::str_replace(., "layer.2", "pred50") %>% 
  stringr::str_replace(., "layer.3", "pred95")

# calculate 90% prediction interval (PI90)
r_PI90 <- (r_qrf_pred_quant$pred95 - r_qrf_pred_quant$pred5)

# make reclassification matrix: let 3 = AAA, 2 = AA, 1 = A and 0 = none
m_rcl_GSM_thresholds <- matrix(c(0, 1, 3,
                                 1, 2, 2,
                                 2, 3, 1,
                                 3, maxValue(r_PI90), 0),
                               ncol = 3, byrow = TRUE)

# reclassify raster into GSM accuracy thresholds
r_PI90_thresholds <- reclassify(r_PI90, m_rcl_GSM_thresholds)

# define levels of categorical covariates in several steps (see ?raster::factorValues):
# 1) turn raster into factor
r_PI90_thresholds <- as.factor(r_PI90_thresholds)
# 2) retrieve levels
tbl_rat <- levels(r_PI90_thresholds)[[1]]
# 3) add description of classes
tbl_rat$threshold <- c("none", "A", "AA", "AAA")
# 4) designate levels to raster
levels(r_PI90_thresholds) <- tbl_rat

# stack all maps of results and rename
r_stack_predictions_PI90 <- stack(r_qrf_pred_mean, r_qrf_pred_quant,
                                r_PI90, r_PI90_thresholds)

names(r_stack_predictions_PI90) <- names(r_stack_predictions_PI90) %>%
  stringr::str_replace(., "layer.1", "PI90") %>% 
  stringr::str_replace(., "layer.2", "PI90_thresholds")



### Map the results ---------------------------------------------------------

# mean predictions
plot(r_stack_predictions_PI90$pred_mean,
     main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM, " cm"),
     col = viridis::magma(n = 200),
     axes = FALSE,
     box = FALSE)

# prediction of 5th quantile
plot(r_stack_predictions_PI90$pred5,
     main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM, " cm: 5th quantile"),
     col = viridis::magma(n = 200),
     axes = FALSE,
     box = FALSE)

# prediction of median (50th quantile)
plot(r_stack_predictions_PI90$pred50,
     main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM, " cm: median (50th quantile)"),
     col = viridis::magma(n = 200),
     axes = FALSE,
     box = FALSE)

# prediction of 95th quantile
plot(r_stack_predictions_PI90$pred95,
     main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM, " cm: 95th quantile"),
     col = viridis::magma(n = 200),
     axes = FALSE,
     box = FALSE)

# PI90
plot(r_stack_predictions_PI90$PI90,
     main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM,
                   " cm: PI90"),
     col = viridis::cividis(n = 200),
     axes = FALSE,
     box = FALSE)

# GSM accuracy thresholds
# make color scheme:
v_cols <- c("#db4325", "#eda247", "#e6e1bc", "#006164")

# using rasterVis::levelplot
m_thresholds <- levelplot(r_stack_predictions_PI90$PI90_thresholds,
                          main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM,
                                        " cm: accuracy thresholds"),
                          att = "threshold",
                          par.settings = list(axis.line = list(col = "transparent")),
                          scales = list(draw = FALSE),
                          col.regions = v_cols)

# using regular plot
plot(r_stack_predictions_PI90$PI90_thresholds,
     main = paste0(TARGET, " ", D_UPPER_NUM, "-", D_LOWER_NUM,
                   " cm: accuracy thresholds"),
     col = v_cols,
     axes = FALSE, box = FALSE, legend = FALSE)
legend(x = "right",
       legend = c("none", "A", "AA", "AAA"),
       fill = v_cols)



### Write maps of predictions to disk as GeoTIFFs ---------------------------

# save prediction maps
system.time(
  foreach(n = 1:nlayers(r_stack_predictions_PI90)) %do%
    writeRaster(r_stack_predictions_PI90[[n]],
                paste0("out/maps/target/", TARGET, "/GeoTIFFs/", TARGET, "_",
                       D_MID, "_QRF_", names(r_stack_predictions_PI90)[n], ".tif"),
                overwrite = TRUE)
) # time elapse sequential: 1.5 min


