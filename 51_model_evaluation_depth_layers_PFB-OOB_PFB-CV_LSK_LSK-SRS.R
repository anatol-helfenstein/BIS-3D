# Name:     51_model_evaluation_depth_layers_PFB_OOB_PFB_CV_LSK_LSK_SRS.R
#
# Content:  - read in target variable regression matrix and fitted model
#           - use "ranger" pkg to predict mean and all quantiles
#           - evaluate map accuracy using 4 methods/measures SEPARATED
#             INTO GSM DEPTH LAYERS:
#               - PFB-OOB: out of bag (OOB) calibration data
#               - PFB-CV: hold outs from location-grouped 10-fold CV of calibration data
#               - LSK: independent validation/test data: LSK/CCNL
#               - LSK-SRS: independent validation/test data (LSK/CCNL) taking into
#                 account probability sample, in our case stratified random
#                 sampling (SRS) because split into depth layers
#               * PFB-OOB, PFB-CV & LSK are computed over all depths in script
#               "50_model_evaluation_all_depths_PFB_OOB_PFB_CV_LSK.R"
#           - model evaluation over space (2D), plotting residuals and PICP of
#             PI90 per GSM depth increment over the entire NL
#
# Refs:     - QRF package and vignettes:
#             https://cran.r-project.org/web/packages/quantregForest/quantregForest.pdf
#             https://mran.microsoft.com/snapshot/2015-07-15/web/packages/quantregForest/vignettes/quantregForest.pdf
#           - General approach and equations for calculating accuracy metrics
#             for each GSM depth layer for a stratified random sampling design:
#               - "man/BIS-3D_validation_approach_GH_AH.pdf"
#               - "man/STRS inference_part1.pdf"
#               - "man/STRS inference_part2.pdf"
#           
# Inputs:   - target regression matrix: out/data/model/tbl_regmat_[TARGET].Rds
#           - final fitted QRF model using all calibration data (optimal):
#             "out/data/model/QRF_fit_[TARGET]_obs[]_p[]_[]CV_optimal.Rds"
#           - series of (10) QRF model fits to use for CV results:
#             "out/data/model/QRF_fit_[TARGET]_obs[]_p[]_[]CV_all_folds.Rds"
#
# Output:   - Accuracy plots and metrics over GSM depth increments of PFB-OOB,
#             PFB-CV, LSK and LSK-SRS
#           - maps of residuals and PICP of PI90 at validation locations
#             grouped by GSM depth increments
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  June 2021



### Empty memory and workspace; load required packages -------------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "sf", "ranger", "caret", "foreach", "viridis", "cowplot",
          "ggspatial", "mapview", "data.table", "RColorBrewer", "scales",
          "surveyplanning", "boot")
lapply(pkgs, library, character.only = TRUE)
# make sure 'mapview' pkg installed from github:
# remotes::install_github("r-spatial/mapview")
mapviewOptions(fgb = FALSE) # to visualize points on map



### Designate script parameters & load modelling data --------------------------

# 1) Specify DSM target soil property (response):
TARGET = "pH_KCl"
# expression of TARGET for model evaluation plots
TARGET_EXP = "pH [KCl]"
TARGET_PRED = expression(paste(hat(pH), " [KCl]"))

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

# 3) Specify which (previously fitted) optimal model to use:
QRF_FIT_optimal <- read_rds(paste0(
  "out/data/model/QRF_fit_", TARGET,
  "_obs", nrow(tbl_regmat_target_cal),
  "_p", ncol(dplyr::select(tbl_regmat_target, -c(split:hor, all_of(TARGET)))),
  "_LLO_10FCV_optimal.Rds"))

# 4) Specify which series of (10) models to use for cross-validation results
#    (each model uses inidices from 1 fold of 10-fold CV):
QRF_FIT_all_folds <- read_rds(paste0(
  "out/data/model/QRF_fit_", TARGET,
  "_obs", nrow(tbl_regmat_target_cal),
  "_p", ncol(dplyr::select(tbl_regmat_target, -c(split:hor, all_of(TARGET)))),
  "_LLO_10FCV_all_folds.Rds"))

# 5) Specify which quantiles we want to predict:
# to calculate PICP we choose entire range at the 1% scale
QUANTILES <- seq(0.00, 1.00, 0.01)

# 6) Specify number of cores to use for parallel computation
THREADS = parallel::detectCores()

# 7) Set predicted vs. observed plotting X and Y axis min, max, range and breaks
# calibration/training data
XY_MIN_CAL = min(tbl_regmat_target_cal[TARGET])
XY_MAX_CAL = max(tbl_regmat_target_cal[TARGET])
XY_RANGE_CAL = diff(range(XY_MIN_CAL, XY_MAX_CAL))
XY_BREAKS_CAL = unique(round(seq(XY_MIN_CAL, XY_MAX_CAL, XY_RANGE_CAL/10)))
# validation/test data
XY_MIN_VAL = min(tbl_regmat_target_val[TARGET])
XY_MAX_VAL = max(tbl_regmat_target_val[TARGET])
XY_RANGE_VAL = diff(range(XY_MIN_VAL, XY_MAX_VAL))
XY_BREAKS_VAL = unique(round(seq(XY_MIN_VAL, XY_MAX_VAL, XY_RANGE_VAL/10)))



### Prepare model evaluation datasets ==========================================

# We want to evaluate models using:
#   PFB-OOB: OOB predictions of optimal model (using all calibration data)
#   PFB-CV: Cross-validated predictions fit using indices of 10-fold CV (each model/
#       fold uses different subsets of calibration data)
#   LSK: Independent model validation using LSK/CCNL (depending on TARGET soil
#       property)

# Use modified ranger function from ISRIC to calculate mean in addition to the
# usual quantiles from QRF
source("R/other/predict_qrf_fun.R")


## PFB-OOB: OOB predictions of optimal model (calibration dataset) ------------------

# (QRF_FIT_optimal$finalModel$predictions are the inbag predictions....)

# OOB quantiles and mean
qrf_tree_cal_OOB <- predict.ranger.tree(QRF_FIT_optimal$finalModel,
                                        data = NULL,
                                        type = "treepred")

# predict all quantiles and then also the mean
tbl_qrf_cal_OOB <- data.frame(t(apply(qrf_tree_cal_OOB$predictions,
                                      1, quantile, QUANTILES, na.rm = TRUE))) %>%
  as_tibble() %>%
  rename_all(~ paste0("quant_", QUANTILES * 100)) %>%
  # predict mean value
  add_column(pred_mean = apply(qrf_tree_cal_OOB$predictions,
                               1, mean, na.rm = TRUE),
             .before = "quant_0")

# add observations, depths and compute logical if within PI90
tbl_predobs_cal_OOB <- tbl_qrf_cal_OOB %>% 
  add_column(d_upper = tbl_regmat_target_cal$d_upper,
             d_lower = tbl_regmat_target_cal$d_lower,
             d_mid = tbl_regmat_target_cal$d_mid,
             obs = pull(tbl_regmat_target_cal, TARGET),
             .before = "pred_mean") %>% 
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                               TRUE, FALSE))


## PFB-CV: Location-grouped 10-fold CV of calibration dataset ----------------------

# only want to predict for samples in hold out of each model
ls_tbl_predictors_hold_out <- foreach(i = 1:length(QRF_FIT_all_folds)) %do% {
  tbl_regmat_target_cal %>%
    dplyr::select(-(split:hor), -pH_KCl) %>% 
    .[QRF_FIT_all_folds[[i]]$control$indexOut[[1]],]
}

# retrieve OOB predictions only for hold out samples for all quantiles
ls_qrf_cv_quantiles <- foreach(i = 1:length(QRF_FIT_all_folds)) %do% {
  predict(QRF_FIT_all_folds[[i]]$finalModel,
          data = ls_tbl_predictors_hold_out[[i]],
          type = "quantiles",
          quantiles = QUANTILES,
          seed = 2021,
          num.threads = THREADS,
          verbose = TRUE)
}

# retrieve indices from final model of each fold (not all samples used in each fold)
ls_indices_hold_out <- map(QRF_FIT_all_folds, ~ .$control$indexOut) %>% 
  map(., ~unlist(.x, use.names = FALSE)) %>% 
  set_names(., paste0("Resample", 1:10))

# retrieve quantile predictions, indices indicating which samples in calibration
# data set used for final model of each fold/model and extend tibbles to include
# entire calibration dataset (filling in NA for folds/models where samples were
# not in final model)
ls_qrf_cal_cv <- map(ls_qrf_cv_quantiles, ~.$predictions) %>% 
  map(., ~ as_tibble(.x)) %>% 
  map(., ~ rename_all(.x, ~ paste0("quant_", QUANTILES * 100))) %>% 
  map2(.x = ., .y = ls_indices_hold_out, ~add_column(.x, index = .y)) %>% 
  map(., ~ complete(.x, index = full_seq(1:nrow(tbl_regmat_target_cal), 1))) %>%
  set_names(., paste0("Resample", 1:10))

# combine predictions of quantiles across all folds/models into 1 tibble
v_colnames <- Reduce(intersect, lapply(ls_qrf_cal_cv, names))

tbl_predobs_cal_cv <- lapply(
  ls_qrf_cal_cv,
  function(x) setkeyv(data.table(x), v_colnames, physical = FALSE))

tbl_predobs_cal_cv <- Reduce(
  function(...) merge(..., all = TRUE, sort = FALSE), tbl_predobs_cal_cv) %>% 
  as_tibble()

# remove NA's and arrange in same order as original calibration dataframe
tbl_predobs_cal_cv <- tbl_predobs_cal_cv %>% 
  drop_na() %>% 
  arrange(index) %>% 
  dplyr::select(-index) # no longer need index col

# add observations, depths, mean predictions & compute logical if within PI90
tbl_predobs_cal_cv <- tbl_predobs_cal_cv %>% 
  add_column(d_upper = tbl_regmat_target_cal$d_upper,
             d_lower = tbl_regmat_target_cal$d_lower,
             d_mid = tbl_regmat_target_cal$d_mid,
             obs = pull(tbl_regmat_target_cal, TARGET),
             pred_mean = QRF_FIT_optimal$pred %>% 
               as_tibble() %>% 
               arrange(rowIndex) %>% 
               pull(pred),
             .before = "quant_0") %>% 
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                               TRUE, FALSE))


## LSK: Predictions of independent validation dataset ---------------------------

# predict independent test dataset (LSK or CCNL)
qrf_tree_val <- predict.ranger.tree(QRF_FIT_optimal$finalModel,
                                    data = tbl_regmat_target_val %>%
                                      dplyr::select(-(split:hor), -pH_KCl),
                                    type = "treepred")

# predict all quantiles and then also the mean
tbl_qrf_val <- data.frame(t(apply(qrf_tree_val$predictions,
                                  1, quantile, QUANTILES, na.rm = TRUE))) %>% 
  as_tibble() %>% 
  rename_all(~ paste0("quant_", QUANTILES * 100)) %>% 
  # predict mean value
  add_column(pred_mean = apply(qrf_tree_val$predictions,
                               1, mean, na.rm = TRUE),
             .before = "quant_0")

# add observations, depths & compute logical if within PI90
tbl_predobs_val <- tbl_qrf_val %>% 
  add_column(d_upper = tbl_regmat_target_val$d_upper,
             d_lower = tbl_regmat_target_val$d_lower,
             d_mid = tbl_regmat_target_val$d_mid,
             obs = pull(tbl_regmat_target_val, TARGET),
             .before = "pred_mean") %>% 
  mutate(within_PI90 = if_else(obs >= quant_5 & obs <= quant_95,
                               TRUE, FALSE))

# include spatial component for assessing validation using sampling design (sf)
sf_predobs_val <- sf_regmat_target_val %>% 
  dplyr::select(sample_id:hor) %>% 
  bind_cols(., tbl_predobs_val)

# plot validation locations over probability sampling strata
m_sampling_design <- ggplot() +
  geom_sf(data = sf_LSK_strata) +
  geom_sf(data = sf_predobs_val)

# join points and polygons sf features (overlay)
sf_predobs_val <- st_join(sf_predobs_val, sf_LSK_strata) %>% 
  rename(str_id = OBJECTID,
         str_code = stratum_cu,
         str_leng = Shape_Leng,
         str_area_m2 = Shape_Area,
         str_area_ha = opp_ha)

# one location is in built-up area and has no designated stratum
mapview(sf_predobs_val %>%
          filter(str_code %in% NA))

# remove it
sf_predobs_val <- sf_predobs_val %>% 
  filter(!str_code %in% NA)

# area of all strata (should be approx land area of NL excluding cities & water)
area_tot_m2 = as.numeric(sum(st_area(sf_LSK_strata))) # in m^2



### Group by GSM depth layers --------------------------------------------------

# split data into designated GSM depth increments
# PFB-OOB: calibration OOB data
tbl_predobs_cal_OOB_0_5 <- tbl_predobs_cal_OOB %>% 
  filter(between(d_mid, 0, 4.9))
tbl_predobs_cal_OOB_5_15 <- tbl_predobs_cal_OOB %>% 
  filter(between(d_mid, 5, 14.9))
tbl_predobs_cal_OOB_15_30 <- tbl_predobs_cal_OOB %>% 
  filter(between(d_mid, 15, 29.9))
tbl_predobs_cal_OOB_30_60 <- tbl_predobs_cal_OOB %>% 
  filter(between(d_mid, 30, 59.9))
tbl_predobs_cal_OOB_60_100 <- tbl_predobs_cal_OOB %>% 
  filter(between(d_mid, 60, 99.9))
tbl_predobs_cal_OOB_100_200 <- tbl_predobs_cal_OOB %>% 
  filter(between(d_mid, 100, 200))
# PFB-CV: Location-grouped 10-fold CV calibration data
tbl_predobs_cal_cv_0_5 <- tbl_predobs_cal_cv %>% 
  filter(between(d_mid, 0, 4.9))
tbl_predobs_cal_cv_5_15 <- tbl_predobs_cal_cv %>% 
  filter(between(d_mid, 5, 14.9))
tbl_predobs_cal_cv_15_30 <- tbl_predobs_cal_cv %>% 
  filter(between(d_mid, 15, 29.9))
tbl_predobs_cal_cv_30_60 <- tbl_predobs_cal_cv %>% 
  filter(between(d_mid, 30, 59.9))
tbl_predobs_cal_cv_60_100 <- tbl_predobs_cal_cv %>% 
  filter(between(d_mid, 60, 99.9))
tbl_predobs_cal_cv_100_200 <- tbl_predobs_cal_cv %>% 
  filter(between(d_mid, 100, 200))
# validation data
sf_predobs_val_0_5 <- sf_predobs_val %>% 
  filter(between(d_mid, 0, 4.9))
sf_predobs_val_5_15 <- sf_predobs_val %>% 
  filter(between(d_mid, 5, 14.9))
sf_predobs_val_15_30 <- sf_predobs_val %>% 
  filter(between(d_mid, 15, 29.9))
sf_predobs_val_30_60 <- sf_predobs_val %>% 
  filter(between(d_mid, 30, 59.9))
sf_predobs_val_60_100 <- sf_predobs_val %>% 
  filter(between(d_mid, 60, 99.9))
sf_predobs_val_100_200 <- sf_predobs_val %>% 
  filter(between(d_mid, 100, 200))

# number of samples below 2m
# PFB-OOB
n_below_2m_cal_OOB <- tbl_predobs_cal_OOB %>% 
  filter(d_mid > 200) %>% 
  nrow()
# PFB-CV
n_below_2m_cal_cv <- tbl_predobs_cal_cv %>% 
  filter(d_mid > 200) %>% 
  nrow()
# validation data
n_below_2m_val <- sf_predobs_val %>% 
  filter(d_mid > 200) %>% 
  nrow()

# CHECK: If no samples were forgotten, this should be TRUE:
# PFB-OOB
nrow(tbl_predobs_cal_OOB_0_5) + nrow(tbl_predobs_cal_OOB_5_15) +
  nrow(tbl_predobs_cal_OOB_15_30) + nrow(tbl_predobs_cal_OOB_30_60) +
  nrow(tbl_predobs_cal_OOB_60_100) + nrow(tbl_predobs_cal_OOB_100_200) +
  n_below_2m_cal_OOB == nrow(tbl_predobs_cal_OOB)
# PFB-CV
nrow(tbl_predobs_cal_cv_0_5) + nrow(tbl_predobs_cal_cv_5_15) +
  nrow(tbl_predobs_cal_cv_15_30) + nrow(tbl_predobs_cal_cv_30_60) +
  nrow(tbl_predobs_cal_cv_60_100) + nrow(tbl_predobs_cal_cv_100_200) +
  n_below_2m_cal_cv == nrow(tbl_predobs_cal_cv)
# validation data
nrow(sf_predobs_val_0_5) + nrow(sf_predobs_val_5_15) +
  nrow(sf_predobs_val_15_30) + nrow(sf_predobs_val_30_60) +
  nrow(sf_predobs_val_60_100) + nrow(sf_predobs_val_100_200) +
  n_below_2m_val == nrow(sf_predobs_val)



### 0-5cm: Accuracy plots and metrics ------------------------------------------

# 0-5cm: PFB-OOB and PFB-CV accuracy plots and metrics -------------------------

# PFB-OOB accuracy metrics
tbl_accuracy_metrics_PFB_OOB_0_5 <- tibble(
  n = length(tbl_predobs_cal_OOB_0_5$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB_0_5$obs - tbl_predobs_cal_OOB_0_5$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB_0_5$obs - tbl_predobs_cal_OOB_0_5$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB_0_5$obs - tbl_predobs_cal_OOB_0_5$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB_0_5$obs - tbl_predobs_cal_OOB_0_5$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB_0_5$obs - tbl_predobs_cal_OOB_0_5$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB_0_5$obs - mean(tbl_predobs_cal_OOB_0_5$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB_0_5$obs - tbl_predobs_cal_OOB_0_5$quant_50)^2))/
             sum((tbl_predobs_cal_OOB_0_5$obs - mean(tbl_predobs_cal_OOB_0_5$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB_0_5, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB_0_5, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# PFB-CV accuracy metrics
tbl_accuracy_metrics_PFB_CV_0_5 <- tibble(
  n = length(tbl_predobs_cal_cv_0_5$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv_0_5$obs - tbl_predobs_cal_cv_0_5$pred_mean),
  me_median = mean(tbl_predobs_cal_cv_0_5$obs - tbl_predobs_cal_cv_0_5$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv_0_5$obs - tbl_predobs_cal_cv_0_5$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv_0_5$obs - tbl_predobs_cal_cv_0_5$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv_0_5$obs - tbl_predobs_cal_cv_0_5$pred_mean)^2))/
             sum((tbl_predobs_cal_cv_0_5$obs - mean(tbl_predobs_cal_cv_0_5$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv_0_5$obs - tbl_predobs_cal_cv_0_5$quant_50)^2))/
             sum((tbl_predobs_cal_cv_0_5$obs - mean(tbl_predobs_cal_cv_0_5$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv_0_5, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv_0_5, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))

# PFB-OOB accuracy plot
p_pred_obs_PFB_OOB_0_5 <- tbl_predobs_cal_OOB_0_5 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_OOB_0_5$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_OOB_0_5$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_0_5,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_0_5,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_0_5,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_0_5,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_0_5,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")
# PFB-CV accuracy plot
p_pred_obs_PFB_CV_0_5 <- tbl_predobs_cal_cv_0_5 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_CV_0_5$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_CV_0_5$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_0_5,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_0_5,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_0_5,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_0_5,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_0_5,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")


# 0-5cm: LSK and LSK-SRS accuracy plots and metrics ----------------------------

# NO accuracy plot or metrics for validation data for 0-5cm because only 1 obs
sf_predobs_val_0_5



### 5-15cm: Accuracy plots and metrics -----------------------------------------

# 5-15cm: PFB-OOB and PFB-CV accuracy plots and metrics ------------------------

# PFB-OOB accuracy metrics
tbl_accuracy_metrics_PFB_OOB_5_15 <- tibble(
  n = length(tbl_predobs_cal_OOB_5_15$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB_5_15$obs - tbl_predobs_cal_OOB_5_15$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB_5_15$obs - tbl_predobs_cal_OOB_5_15$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB_5_15$obs - tbl_predobs_cal_OOB_5_15$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB_5_15$obs - tbl_predobs_cal_OOB_5_15$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB_5_15$obs - tbl_predobs_cal_OOB_5_15$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB_5_15$obs - mean(tbl_predobs_cal_OOB_5_15$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB_5_15$obs - tbl_predobs_cal_OOB_5_15$quant_50)^2))/
             sum((tbl_predobs_cal_OOB_5_15$obs - mean(tbl_predobs_cal_OOB_5_15$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB_5_15, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB_5_15, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# PFB-CV accuracy metrics
tbl_accuracy_metrics_PFB_CV_5_15 <- tibble(
  n = length(tbl_predobs_cal_cv_5_15$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv_5_15$obs - tbl_predobs_cal_cv_5_15$pred_mean),
  me_median = mean(tbl_predobs_cal_cv_5_15$obs - tbl_predobs_cal_cv_5_15$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv_5_15$obs - tbl_predobs_cal_cv_5_15$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv_5_15$obs - tbl_predobs_cal_cv_5_15$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv_5_15$obs - tbl_predobs_cal_cv_5_15$pred_mean)^2))/
             sum((tbl_predobs_cal_cv_5_15$obs - mean(tbl_predobs_cal_cv_5_15$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv_5_15$obs - tbl_predobs_cal_cv_5_15$quant_50)^2))/
             sum((tbl_predobs_cal_cv_5_15$obs - mean(tbl_predobs_cal_cv_5_15$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv_5_15, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv_5_15, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))

# PFB-OOB accuracy plot
p_pred_obs_PFB_OOB_5_15 <- tbl_predobs_cal_OOB_5_15 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_OOB_5_15$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_OOB_5_15$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_5_15,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_5_15,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_5_15,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_5_15,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_5_15,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")
# PFB-CV accuracy plot
p_pred_obs_PFB_CV_5_15 <- tbl_predobs_cal_cv_5_15 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_CV_5_15$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_CV_5_15$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_5_15,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_5_15,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_5_15,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_5_15,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_5_15,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")


# 5-15cm: LSK and LSK-SRS accuracy plots and metrics ---------------------------

# first make accuracy plot and metrics for LSK, ignoring SRS (just for comparison)
# LSK accuracy metrics
tbl_accuracy_metrics_LSK_5_15 <- tibble(
  n = length(sf_predobs_val_5_15$obs),
  one_one = "1:1",
  me = mean(sf_predobs_val_5_15$obs - sf_predobs_val_5_15$pred_mean),
  me_median = mean(sf_predobs_val_5_15$obs - sf_predobs_val_5_15$quant_50),
  rmse = sqrt(mean((sf_predobs_val_5_15$obs - sf_predobs_val_5_15$pred_mean)^2)),
  rmse_median = sqrt(mean((sf_predobs_val_5_15$obs - sf_predobs_val_5_15$quant_50)^2)),
  mec = 1-((sum((sf_predobs_val_5_15$obs - sf_predobs_val_5_15$pred_mean)^2))/
             sum((sf_predobs_val_5_15$obs - mean(sf_predobs_val_5_15$obs))^2)),
  mec_median = 1-((sum((sf_predobs_val_5_15$obs - sf_predobs_val_5_15$quant_50)^2))/
                    sum((sf_predobs_val_5_15$obs - mean(sf_predobs_val_5_15$obs))^2)),
  n_within_PI90 = nrow(filter(sf_predobs_val_5_15, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_5_15, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# LSK accuracy plot
p_pred_obs_LSK_5_15 <- sf_predobs_val_5_15 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_5_15$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_5_15$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_5_15,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_5_15,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_5_15,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_5_15,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_5_15,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
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
    mutate(across(obs:quant_100,
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

# control randomness of bootstrapping for repoducibility
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
  ylab(as.expression(paste("pH [KCl]"))) +
  xlab(expression(paste(hat(pH), " [KCl]"))) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
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

# 15-30cm: PFB-OOB and PFB-CV accuracy plots and metrics -----------------------

# PFB-OOB accuracy metrics
tbl_accuracy_metrics_PFB_OOB_15_30 <- tibble(
  n = length(tbl_predobs_cal_OOB_15_30$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB_15_30$obs - tbl_predobs_cal_OOB_15_30$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB_15_30$obs - tbl_predobs_cal_OOB_15_30$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB_15_30$obs - tbl_predobs_cal_OOB_15_30$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB_15_30$obs - tbl_predobs_cal_OOB_15_30$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB_15_30$obs - tbl_predobs_cal_OOB_15_30$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB_15_30$obs - mean(tbl_predobs_cal_OOB_15_30$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB_15_30$obs - tbl_predobs_cal_OOB_15_30$quant_50)^2))/
                    sum((tbl_predobs_cal_OOB_15_30$obs - mean(tbl_predobs_cal_OOB_15_30$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB_15_30, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB_15_30, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# PFB-CV accuracy metrics
tbl_accuracy_metrics_PFB_CV_15_30 <- tibble(
  n = length(tbl_predobs_cal_cv_15_30$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv_15_30$obs - tbl_predobs_cal_cv_15_30$pred_mean),
  me_median = mean(tbl_predobs_cal_cv_15_30$obs - tbl_predobs_cal_cv_15_30$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv_15_30$obs - tbl_predobs_cal_cv_15_30$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv_15_30$obs - tbl_predobs_cal_cv_15_30$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv_15_30$obs - tbl_predobs_cal_cv_15_30$pred_mean)^2))/
             sum((tbl_predobs_cal_cv_15_30$obs - mean(tbl_predobs_cal_cv_15_30$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv_15_30$obs - tbl_predobs_cal_cv_15_30$quant_50)^2))/
                    sum((tbl_predobs_cal_cv_15_30$obs - mean(tbl_predobs_cal_cv_15_30$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv_15_30, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv_15_30, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))

# PFB-OOB accuracy plot
p_pred_obs_PFB_OOB_15_30 <- tbl_predobs_cal_OOB_15_30 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_OOB_15_30$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_OOB_15_30$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_15_30,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_15_30,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_15_30,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_15_30,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_15_30,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")
# PFB-CV accuracy plot
p_pred_obs_PFB_CV_15_30 <- tbl_predobs_cal_cv_15_30 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_CV_15_30$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_CV_15_30$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_15_30,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_15_30,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_15_30,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_15_30,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_15_30,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")


# 15-30cm: LSK and LSK-SRS accuracy plots and metrics --------------------------

# first make accuracy plot and metrics for LSK, ignoring SRS (just for comparison)
# LSK accuracy metrics
tbl_accuracy_metrics_LSK_15_30 <- tibble(
  n = length(sf_predobs_val_15_30$obs),
  one_one = "1:1",
  me = mean(sf_predobs_val_15_30$obs - sf_predobs_val_15_30$pred_mean),
  me_median = mean(sf_predobs_val_15_30$obs - sf_predobs_val_15_30$quant_50),
  rmse = sqrt(mean((sf_predobs_val_15_30$obs - sf_predobs_val_15_30$pred_mean)^2)),
  rmse_median = sqrt(mean((sf_predobs_val_15_30$obs - sf_predobs_val_15_30$quant_50)^2)),
  mec = 1-((sum((sf_predobs_val_15_30$obs - sf_predobs_val_15_30$pred_mean)^2))/
             sum((sf_predobs_val_15_30$obs - mean(sf_predobs_val_15_30$obs))^2)),
  mec_median = 1-((sum((sf_predobs_val_15_30$obs - sf_predobs_val_15_30$quant_50)^2))/
                    sum((sf_predobs_val_15_30$obs - mean(sf_predobs_val_15_30$obs))^2)),
  n_within_PI90 = nrow(filter(sf_predobs_val_15_30, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_15_30, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# LSK accuracy plot
p_pred_obs_LSK_15_30 <- sf_predobs_val_15_30 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_15_30$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_15_30$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_15_30,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_15_30,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_15_30,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_15_30,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_15_30,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
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
    mutate(across(obs:quant_100,
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

# control randomness of bootstrapping for repoducibility
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
  ylab(as.expression(paste("pH [KCl]"))) +
  xlab(expression(paste(hat(pH), " [KCl]"))) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
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

# 30-60cm: PFB-OOB and PFB-CV accuracy plots and metrics -----------------------

# PFB-OOB accuracy metrics
tbl_accuracy_metrics_PFB_OOB_30_60 <- tibble(
  n = length(tbl_predobs_cal_OOB_30_60$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB_30_60$obs - tbl_predobs_cal_OOB_30_60$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB_30_60$obs - tbl_predobs_cal_OOB_30_60$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB_30_60$obs - tbl_predobs_cal_OOB_30_60$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB_30_60$obs - tbl_predobs_cal_OOB_30_60$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB_30_60$obs - tbl_predobs_cal_OOB_30_60$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB_30_60$obs - mean(tbl_predobs_cal_OOB_30_60$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB_30_60$obs - tbl_predobs_cal_OOB_30_60$quant_50)^2))/
                    sum((tbl_predobs_cal_OOB_30_60$obs - mean(tbl_predobs_cal_OOB_30_60$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB_30_60, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB_30_60, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# PFB-CV accuracy metrics
tbl_accuracy_metrics_PFB_CV_30_60 <- tibble(
  n = length(tbl_predobs_cal_cv_30_60$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv_30_60$obs - tbl_predobs_cal_cv_30_60$pred_mean),
  me_median = mean(tbl_predobs_cal_cv_30_60$obs - tbl_predobs_cal_cv_30_60$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv_30_60$obs - tbl_predobs_cal_cv_30_60$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv_30_60$obs - tbl_predobs_cal_cv_30_60$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv_30_60$obs - tbl_predobs_cal_cv_30_60$pred_mean)^2))/
             sum((tbl_predobs_cal_cv_30_60$obs - mean(tbl_predobs_cal_cv_30_60$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv_30_60$obs - tbl_predobs_cal_cv_30_60$quant_50)^2))/
                    sum((tbl_predobs_cal_cv_30_60$obs - mean(tbl_predobs_cal_cv_30_60$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv_30_60, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv_30_60, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))

# PFB-OOB accuracy plot
p_pred_obs_PFB_OOB_30_60 <- tbl_predobs_cal_OOB_30_60 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_OOB_30_60$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_OOB_30_60$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_30_60,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_30_60,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_30_60,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_30_60,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_30_60,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")
# PFB-CV accuracy plot
p_pred_obs_PFB_CV_30_60 <- tbl_predobs_cal_cv_30_60 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_CV_30_60$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_CV_30_60$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_30_60,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_30_60,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_30_60,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_30_60,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_30_60,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")


# 30-60cm: LSK and LSK-SRS accuracy plots and metrics --------------------------

# first make accuracy plot and metrics for LSK, ignoring SRS (just for comparison)
# LSK accuracy metrics
tbl_accuracy_metrics_LSK_30_60 <- tibble(
  n = length(sf_predobs_val_30_60$obs),
  one_one = "1:1",
  me = mean(sf_predobs_val_30_60$obs - sf_predobs_val_30_60$pred_mean),
  me_median = mean(sf_predobs_val_30_60$obs - sf_predobs_val_30_60$quant_50),
  rmse = sqrt(mean((sf_predobs_val_30_60$obs - sf_predobs_val_30_60$pred_mean)^2)),
  rmse_median = sqrt(mean((sf_predobs_val_30_60$obs - sf_predobs_val_30_60$quant_50)^2)),
  mec = 1-((sum((sf_predobs_val_30_60$obs - sf_predobs_val_30_60$pred_mean)^2))/
             sum((sf_predobs_val_30_60$obs - mean(sf_predobs_val_30_60$obs))^2)),
  mec_median = 1-((sum((sf_predobs_val_30_60$obs - sf_predobs_val_30_60$quant_50)^2))/
                    sum((sf_predobs_val_30_60$obs - mean(sf_predobs_val_30_60$obs))^2)),
  n_within_PI90 = nrow(filter(sf_predobs_val_30_60, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_30_60, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# LSK accuracy plot
p_pred_obs_LSK_30_60 <- sf_predobs_val_30_60 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_30_60$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_30_60$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_30_60,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_30_60,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_30_60,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_30_60,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_30_60,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
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
    mutate(across(obs:quant_100,
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

# control randomness of bootstrapping for repoducibility
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
  ylab(as.expression(paste("pH [KCl]"))) +
  xlab(expression(paste(hat(pH), " [KCl]"))) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
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

# 60-100cm: PFB-OOB and PFB-CV accuracy plots and metrics ----------------------

# PFB-OOB accuracy metrics
tbl_accuracy_metrics_PFB_OOB_60_100 <- tibble(
  n = length(tbl_predobs_cal_OOB_60_100$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB_60_100$obs - tbl_predobs_cal_OOB_60_100$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB_60_100$obs - tbl_predobs_cal_OOB_60_100$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB_60_100$obs - tbl_predobs_cal_OOB_60_100$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB_60_100$obs - tbl_predobs_cal_OOB_60_100$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB_60_100$obs - tbl_predobs_cal_OOB_60_100$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB_60_100$obs - mean(tbl_predobs_cal_OOB_60_100$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB_60_100$obs - tbl_predobs_cal_OOB_60_100$quant_50)^2))/
                    sum((tbl_predobs_cal_OOB_60_100$obs - mean(tbl_predobs_cal_OOB_60_100$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB_60_100, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB_60_100, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# PFB-CV accuracy metrics
tbl_accuracy_metrics_PFB_CV_60_100 <- tibble(
  n = length(tbl_predobs_cal_cv_60_100$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv_60_100$obs - tbl_predobs_cal_cv_60_100$pred_mean),
  me_median = mean(tbl_predobs_cal_cv_60_100$obs - tbl_predobs_cal_cv_60_100$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv_60_100$obs - tbl_predobs_cal_cv_60_100$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv_60_100$obs - tbl_predobs_cal_cv_60_100$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv_60_100$obs - tbl_predobs_cal_cv_60_100$pred_mean)^2))/
             sum((tbl_predobs_cal_cv_60_100$obs - mean(tbl_predobs_cal_cv_60_100$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv_60_100$obs - tbl_predobs_cal_cv_60_100$quant_50)^2))/
                    sum((tbl_predobs_cal_cv_60_100$obs - mean(tbl_predobs_cal_cv_60_100$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv_60_100, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv_60_100, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))

# PFB-OOB accuracy plot
p_pred_obs_PFB_OOB_60_100 <- tbl_predobs_cal_OOB_60_100 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_OOB_60_100$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_OOB_60_100$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_60_100,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_60_100,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_60_100,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_60_100,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_60_100,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")
# PFB-CV accuracy plot
p_pred_obs_PFB_CV_60_100 <- tbl_predobs_cal_cv_60_100 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_CV_60_100$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_CV_60_100$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_60_100,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_60_100,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_60_100,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_60_100,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_60_100,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")


# 60-100cm: LSK and LSK-SRS accuracy plots and metrics -------------------------

# first make accuracy plot and metrics for LSK, ignoring SRS (just for comparison)
# LSK accuracy metrics
tbl_accuracy_metrics_LSK_60_100 <- tibble(
  n = length(sf_predobs_val_60_100$obs),
  one_one = "1:1",
  me = mean(sf_predobs_val_60_100$obs - sf_predobs_val_60_100$pred_mean),
  me_median = mean(sf_predobs_val_60_100$obs - sf_predobs_val_60_100$quant_50),
  rmse = sqrt(mean((sf_predobs_val_60_100$obs - sf_predobs_val_60_100$pred_mean)^2)),
  rmse_median = sqrt(mean((sf_predobs_val_60_100$obs - sf_predobs_val_60_100$quant_50)^2)),
  mec = 1-((sum((sf_predobs_val_60_100$obs - sf_predobs_val_60_100$pred_mean)^2))/
             sum((sf_predobs_val_60_100$obs - mean(sf_predobs_val_60_100$obs))^2)),
  mec_median = 1-((sum((sf_predobs_val_60_100$obs - sf_predobs_val_60_100$quant_50)^2))/
                    sum((sf_predobs_val_60_100$obs - mean(sf_predobs_val_60_100$obs))^2)),
  n_within_PI90 = nrow(filter(sf_predobs_val_60_100, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_60_100, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# LSK accuracy plot
p_pred_obs_LSK_60_100 <- sf_predobs_val_60_100 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_60_100$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_60_100$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_60_100,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_60_100,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_60_100,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_60_100,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_60_100,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
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
    mutate(across(obs:quant_100,
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

# control randomness of bootstrapping for repoducibility
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
  ylab(as.expression(paste("pH [KCl]"))) +
  xlab(expression(paste(hat(pH), " [KCl]"))) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
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

# 100-200cm: PFB-OOB and PFB-CV accuracy plots and metrics ---------------------

# PFB-OOB accuracy metrics
tbl_accuracy_metrics_PFB_OOB_100_200 <- tibble(
  n = length(tbl_predobs_cal_OOB_100_200$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB_100_200$obs - tbl_predobs_cal_OOB_100_200$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB_100_200$obs - tbl_predobs_cal_OOB_100_200$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB_100_200$obs - tbl_predobs_cal_OOB_100_200$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB_100_200$obs - tbl_predobs_cal_OOB_100_200$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB_100_200$obs - tbl_predobs_cal_OOB_100_200$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB_100_200$obs - mean(tbl_predobs_cal_OOB_100_200$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB_100_200$obs - tbl_predobs_cal_OOB_100_200$quant_50)^2))/
                    sum((tbl_predobs_cal_OOB_100_200$obs - mean(tbl_predobs_cal_OOB_100_200$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB_100_200, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB_100_200, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# PFB-CV accuracy metrics
tbl_accuracy_metrics_PFB_CV_100_200 <- tibble(
  n = length(tbl_predobs_cal_cv_100_200$obs),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv_100_200$obs - tbl_predobs_cal_cv_100_200$pred_mean),
  me_median = mean(tbl_predobs_cal_cv_100_200$obs - tbl_predobs_cal_cv_100_200$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv_100_200$obs - tbl_predobs_cal_cv_100_200$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv_100_200$obs - tbl_predobs_cal_cv_100_200$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv_100_200$obs - tbl_predobs_cal_cv_100_200$pred_mean)^2))/
             sum((tbl_predobs_cal_cv_100_200$obs - mean(tbl_predobs_cal_cv_100_200$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv_100_200$obs - tbl_predobs_cal_cv_100_200$quant_50)^2))/
                    sum((tbl_predobs_cal_cv_100_200$obs - mean(tbl_predobs_cal_cv_100_200$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv_100_200, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv_100_200, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))

# PFB-OOB accuracy plot
p_pred_obs_PFB_OOB_100_200 <- tbl_predobs_cal_OOB_100_200 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_OOB_100_200$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_OOB_100_200$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_100_200,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_100_200,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_100_200,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_100_200,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_OOB_100_200,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")
# PFB-CV accuracy plot
p_pred_obs_PFB_CV_100_200 <- tbl_predobs_cal_cv_100_200 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_PFB_CV_100_200$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_PFB_CV_100_200$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_100_200,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_100_200,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_100_200,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_100_200,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_PFB_CV_100_200,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(col = "PICP of PI90",
       shape = "PICP of PI90")


# 100-200cm: LSK and LSK-SRS accuracy plots and metrics ------------------------

# first make accuracy plot and metrics for LSK, ignoring SRS (just for comparison)
# LSK accuracy metrics
tbl_accuracy_metrics_LSK_100_200 <- tibble(
  n = length(sf_predobs_val_100_200$obs),
  one_one = "1:1",
  me = mean(sf_predobs_val_100_200$obs - sf_predobs_val_100_200$pred_mean),
  me_median = mean(sf_predobs_val_100_200$obs - sf_predobs_val_100_200$quant_50),
  rmse = sqrt(mean((sf_predobs_val_100_200$obs - sf_predobs_val_100_200$pred_mean)^2)),
  rmse_median = sqrt(mean((sf_predobs_val_100_200$obs - sf_predobs_val_100_200$quant_50)^2)),
  mec = 1-((sum((sf_predobs_val_100_200$obs - sf_predobs_val_100_200$pred_mean)^2))/
             sum((sf_predobs_val_100_200$obs - mean(sf_predobs_val_100_200$obs))^2)),
  mec_median = 1-((sum((sf_predobs_val_100_200$obs - sf_predobs_val_100_200$quant_50)^2))/
                    sum((sf_predobs_val_100_200$obs - mean(sf_predobs_val_100_200$obs))^2)),
  n_within_PI90 = nrow(filter(sf_predobs_val_100_200, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(sf_predobs_val_100_200, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation (char = characters)
  mutate(me_char = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse_char = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec_char = as.character(as.expression(paste0("MEC == ", round(mec, 2)))),
         n_char = as.character(as.expression(paste0("italic(n) == ", n))))
# LSK accuracy plot
p_pred_obs_LSK_100_200 <- sf_predobs_val_100_200 %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_accuracy_metrics_LSK_100_200$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_accuracy_metrics_LSK_100_200$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_100_200,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_accuracy_metrics_LSK_100_200,
            aes(x = Inf, y = -Inf, label = n_char), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_100_200,
            aes(x = Inf, y = -Inf, label = mec_char), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_100_200,
            aes(x = Inf, y = -Inf, label = rmse_char), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_accuracy_metrics_LSK_100_200,
            aes(x = Inf, y = -Inf, label = me_char), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
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
    mutate(across(obs:quant_100,
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

# control randomness of bootstrapping for repoducibility
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
  ylab(as.expression(paste("pH [KCl]"))) +
  xlab(expression(paste(hat(pH), " [KCl]"))) +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
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



### Accuracy metrics over depth of all 4 measures ------------------------------

# rowbind cols of sum stats of all GSM depth increments
# PFB-OOB
tbl_accuracy_metrics_PFB_OOB_d <- bind_rows(tbl_accuracy_metrics_PFB_OOB_0_5,
                                       tbl_accuracy_metrics_PFB_OOB_5_15,
                                       tbl_accuracy_metrics_PFB_OOB_15_30,
                                       tbl_accuracy_metrics_PFB_OOB_30_60,
                                       tbl_accuracy_metrics_PFB_OOB_60_100,
                                       tbl_accuracy_metrics_PFB_OOB_100_200) %>%
  add_column(d_increment = c(as.character("0 cm to 5 cm"),
                             as.character("5 cm to 15 cm"),
                             as.character("15 cm to 30 cm"),
                             as.character("30 cm to 60 cm"),
                             as.character("60 cm to 100 cm"),
                             as.character("100 cm to 200 cm")),
             .before = "n")

# save PFB-OOB metrics per GSM depth layer as table (.csv)
# write_csv(tbl_accuracy_metrics_PFB_OOB_d %>%
#             dplyr::select(-c(me_char:n_char)) %>% 
#             mutate_at(vars(me:n_within_PI90_not), ~round(., 2)),
#           paste0("out/data/model/", TARGET, "_model_evaluation_PFB_OOB_d.csv"))

# PFB-CV
tbl_accuracy_metrics_PFB_CV_d <- bind_rows(tbl_accuracy_metrics_PFB_CV_0_5,
                                    tbl_accuracy_metrics_PFB_CV_5_15,
                                    tbl_accuracy_metrics_PFB_CV_15_30,
                                    tbl_accuracy_metrics_PFB_CV_30_60,
                                    tbl_accuracy_metrics_PFB_CV_60_100,
                                    tbl_accuracy_metrics_PFB_CV_100_200) %>%
  add_column(d_increment = c(as.character("0 cm to 5 cm"),
                             as.character("5 cm to 15 cm"),
                             as.character("15 cm to 30 cm"),
                             as.character("30 cm to 60 cm"),
                             as.character("60 cm to 100 cm"),
                             as.character("100 cm to 200 cm")),
             .before = "n")

# save PFB-CV metrics per GSM depth layer as table (.csv)
# write_csv(tbl_accuracy_metrics_PFB_CV_d %>%
#             dplyr::select(-c(me_char:n_char)) %>% 
#             mutate_at(vars(me:n_within_PI90_not), ~round(., 2)),
#           paste0("out/data/model/", TARGET, "_model_evaluation_PFB_CV_d.csv"))

# validation data; because 0-5cm is empty, designate first row as NA
# LSK
tbl_accuracy_metrics_LSK_d <- bind_rows(tbl_accuracy_metrics_LSK_5_15 %>% 
                                         mutate_all(~na_if(., .)),
                                       tbl_accuracy_metrics_LSK_5_15,
                                       tbl_accuracy_metrics_LSK_15_30,
                                       tbl_accuracy_metrics_LSK_30_60,
                                       tbl_accuracy_metrics_LSK_60_100,
                                       tbl_accuracy_metrics_LSK_100_200) %>%
  add_column(d_increment = c(as.character("0 cm to 5 cm"),
                             as.character("5 cm to 15 cm"),
                             as.character("15 cm to 30 cm"),
                             as.character("30 cm to 60 cm"),
                             as.character("60 cm to 100 cm"),
                             as.character("100 cm to 200 cm")),
             .before = "n")

# save LSK per GSM depth layer as table (.csv)
# write_csv(tbl_accuracy_metrics_LSK_d %>%
#             dplyr::select(-c(me_char:n_char)) %>% 
#             mutate_at(vars(me:n_within_PI90_not), ~round(., 2)),
#           paste0("out/data/model/", TARGET, "_model_evaluation_LSK_d.csv"))

# LSK-SRS
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
# write_csv(tbl_accuracy_metrics_LSK_SRS_d %>%
#             mutate_at(vars(contains("_var")), ~round(., 5)) %>% 
#             mutate_at(vars(-contains("_var"), -c(d_increment:n)), ~round(., 2)),
#           paste0("out/data/model/", TARGET, "_model_evaluation_LSK_SRS_d.csv"))

# join accuracy metric tibbles of OOB, CV and spatial validation
tbl_sum_stats_d <- bind_rows(tbl_accuracy_metrics_PFB_OOB_d %>% 
                               add_column(Split = "PFB-OOB",
                                          .before = "d_increment"),
                             tbl_accuracy_metrics_PFB_CV_d %>% 
                               add_column(Split = "PFB-CV",
                                          .before = "d_increment"),
                             tbl_accuracy_metrics_LSK_d %>% 
                               add_column(Split = "LSK",
                                          .before = "d_increment"),
                             tbl_accuracy_metrics_LSK_SRS_d %>% 
                               add_column(Split = "LSK-SRS",
                                          .before = "d_increment"))

# assign levels to depth increments so that they are in reverse order
depth_order <- c("100 cm to 200 cm", "60 cm to 100 cm", "30 cm to 60 cm",
                 "15 cm to 30 cm", "5 cm to 15 cm", "0 cm to 5 cm")
tbl_sum_stats_d$d_increment <- factor(x = tbl_sum_stats_d$d_increment,
                                      levels = depth_order)

# assign levels to datasets so that they are in correct order
dataset_order <- c("PFB-OOB", "PFB-CV", "LSK", "LSK-SRS")
tbl_sum_stats_d$Split <- factor(x = tbl_sum_stats_d$Split,
                           levels = dataset_order)

# plot accuracy metrics over depth
p_sum_stats_d_n <- tbl_sum_stats_d %>% 
  ggplot(aes(x = d_increment, y = n,
             col = Split, shape = Split, fill = Split)) +
  geom_point(size = 2) +
  geom_line(aes(group = Split), linetype = "dashed") +
  # scale_colour_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_colour_manual(values = c("black", "black", "blue", "blue")) +
  # scale_shape_manual(values = c(16, 15, 3)) +
  scale_shape_manual(values = c(21, 22, 23, 25)) +
  scale_fill_manual(values = c("black", "black", "blue", "blue")) +
  labs(x = expression("Depth Increment [cm]"),
       y = expression(n)) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"))

# one more graph only to extract the legend for cowplot
p_sum_stats_d_n_legend <- tbl_sum_stats_d %>% 
  ggplot(aes(x = d_increment, y = n,
             col = Split, shape = Split, fill = Split)) +
  geom_point(size = 2) +
  geom_line(aes(group = Split), linetype = "dashed") +
  # scale_colour_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_colour_manual(values = c("black", "black", "blue", "blue")) +
  # scale_shape_manual(values = c(16, 15, 3)) +
  scale_shape_manual(values = c(21, 22, 23, 25)) +
  scale_fill_manual(values = c("black", "black", "blue", "blue")) +
  labs(y = expression(n)) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  labs(colour = NULL, shape = NULL, fill = NULL) +
  theme_bw()

p_sum_stats_d_me <- tbl_sum_stats_d %>% 
  ggplot(aes(x = d_increment, y = me,
             col = Split, shape = Split, fill = Split)) +
  geom_point(size = 2) +
  geom_line(aes(group = Split), linetype = "dashed") +
  geom_errorbar(aes(ymin = me_ci_low, ymax = me_ci_up, y = me),
                color = "dimgrey", width = 0.1) +
  # scale_colour_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_colour_manual(values = c("black", "black", "blue", "blue")) +
  # scale_shape_manual(values = c(16, 15, 3)) +
  scale_shape_manual(values = c(21, 22, 23, 25)) +
  scale_fill_manual(values = c("black", "black", "blue", "blue")) +
  labs(y = expression("ME")) +
  scale_y_continuous(position = "right", breaks = seq(0, 0.2, 0.1)) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.position = "none",
        # plot.margin = margin(l = -0.8, unit = "cm"),
        plot.margin = unit(c(0, 0, 0, -0.8), "cm"))

p_sum_stats_d_rmse <- tbl_sum_stats_d %>% 
  ggplot(aes(x = d_increment, y = rmse,
             col = Split, shape = Split, fill = Split)) +
  geom_point(size = 2) +
  geom_line(aes(group = Split), linetype = "dashed") +
  geom_errorbar(aes(ymin = rmse_ci_low, ymax = rmse_ci_up, y = rmse),
                color = "dimgrey", width = 0.1) +
  # scale_colour_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_colour_manual(values = c("black", "black", "blue", "blue")) +
  # scale_shape_manual(values = c(16, 15, 3)) +
  scale_shape_manual(values = c(21, 22, 23, 25)) +
  scale_fill_manual(values = c("black", "black", "blue", "blue")) +
  labs(y = expression("RMSE")) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.position = "none",
        # plot.margin = margin(l = -0.8, unit = "cm"),
        plot.margin = unit(c(0, 0, 0, -0.8), "cm"))

p_sum_stats_d_mec <- tbl_sum_stats_d %>% 
  ggplot(aes(x = d_increment, y = mec,
             col = Split, shape = Split, fill = Split)) +
  geom_point(size = 2) +
  geom_line(aes(group = Split), linetype = "dashed") +
  geom_errorbar(aes(ymin = mec_ci_low, ymax = mec_ci_up, y = mec),
                color = "dimgrey", width = 0.1) +
  # scale_colour_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_colour_manual(values = c("black", "black", "blue", "blue")) +
  # scale_shape_manual(values = c(16, 15, 3)) +
  scale_shape_manual(values = c(21, 22, 23, 25)) +
  scale_fill_manual(values = c("black", "black", "blue", "blue")) +
  labs(y = expression("MEC")) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.position = "none",
        # plot.margin = margin(l = -0.8, unit = "cm"),
        plot.margin = unit(c(0, 0.2, 0, -0.8), "cm"))

p_sum_stats_d_PI90 <- tbl_sum_stats_d %>% 
  ggplot(aes(x = d_increment, y = n_within_PI90,
             col = Split, shape = Split, fill = Split)) +
  geom_point(size = 2) +
  geom_line(aes(group = Split), linetype = "dashed") +
  geom_abline(slope = 0, intercept = 0.90, color = "gray") +
  scale_colour_manual(values = c("black", "black", "blue", "blue")) +
  scale_shape_manual(values = c(21, 22, 23, 25)) +
  scale_fill_manual(values = c("black", "black", "blue", "blue")) +
  labs(y = expression("PICP of PI90")) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.position = "none",
        # plot.margin = margin(l = -0.8, unit = "cm"),
        plot.margin = unit(c(0, 0.2, 0, -0.8), "cm"))

# include one overall legend for entire plot grid
legend <- get_legend(p_sum_stats_d_n_legend)

# add axis text labels manually to save plotting space
# Y axis title
axis_title <- ggdraw() + 
  draw_label("Depth [cm]", size = 11, x = 0, hjust = 0.8, vjust = 1, angle = 90) +
  theme(plot.margin = margin(0, 0, 0, 0))
# 0-5cm
axis_label_0_5 <- ggdraw() + 
  draw_label("0 cm to 5 cm", size = 9, x = 0, hjust = -0.37, vjust = 4.5) +
  theme(plot.margin = margin(0, 0, 0, 0))
# 5-15cm
axis_label_5_15 <- ggdraw() + 
  draw_label("5 cm to 15 cm", size = 9, x = 0, hjust = -0.25, vjust = 3.5) +
  theme(plot.margin = margin(0, 0, 0, 0))
# 15-30cm
axis_label_15_30 <- ggdraw() + 
  draw_label("15 cm to 30 cm", size = 9, x = 0, hjust = -0.14, vjust = 2.7) +
  theme(plot.margin = margin(0, 0, 0, 0))
# 30-60cm
axis_label_30_60 <- ggdraw() + 
  draw_label("30 cm to 60 cm", size = 9, x = 0, hjust = -0.14, vjust = 1.8) +
  theme(plot.margin = margin(0, 0, 0, 0))
# 60-100cm
axis_label_60_100 <- ggdraw() + 
  draw_label("60 cm to 100 cm", size = 9, x = 0, hjust = -0.07, vjust = 0.9) +
  theme(plot.margin = margin(0, 0, 0, 0))
# 100-200cm
axis_label_100_200 <- ggdraw() + 
  draw_label("100 cm to 200 cm", size = 9, x = 0, hjust = 0, vjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 0))

# combine using cowplot
p_sum_stats_d <- plot_grid(plot_grid(axis_title,
                                     plot_grid(axis_label_0_5,
                                               axis_label_5_15,
                                               axis_label_15_30,
                                               axis_label_30_60,
                                               axis_label_60_100,
                                               axis_label_100_200,
                                               align = "v", nrow = 6, ncol = 1,
                                               rel_heights = rep(1, 6)),
                                     p_sum_stats_d_n,
                                     p_sum_stats_d_me,
                                     p_sum_stats_d_rmse,
                                     p_sum_stats_d_mec,
                                     p_sum_stats_d_PI90,
                                     align = "v", nrow = 1, ncol = 7,
                                     rel_widths = c(0.1, 0.63, 1, 1, 1, 1, 1)),
                           plot_grid(NULL, legend, ncol = 1),
                           rel_widths = c(1, 0.16))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_evaluation_stats_d.pdf"),
#        plot = p_sum_stats_d,
#        path = paste0("out/figs/models/", TARGET),
#        width = 10, height = 5)



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
# title_5_15 <- ggdraw() + 
#   draw_label(paste0(TARGET_EXP, ": 5 cm to 15 cm depth"),
#              fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
#   # add margin on the left of the drawing canvas,
#   # so title is aligned with left edge of first plot
#   theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_5_15 <- plot_grid(m_5_15_res,
                    m_5_15_PI,
                    align = "hv", nrow = 1, ncol = 2)

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
# title_15_30 <- ggdraw() + 
#   draw_label(paste0(TARGET_EXP, ": 15 to 30 cm depth"),
#              fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
#   # add margin on the left of the drawing canvas,
#   # so title is aligned with left edge of first plot
#   theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_15_30 <- plot_grid(m_15_30_res,
                     m_15_30_PI,
                     align = "hv", nrow = 1, ncol = 2)

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
# title_30_60 <- ggdraw() + 
#   draw_label(paste0(TARGET_EXP, ": 30 to 60 cm depth"),
#              fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
#   # add margin on the left of the drawing canvas,
#   # so title is aligned with left edge of first plot
#   theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_30_60 <- plot_grid(m_30_60_res,
                     m_30_60_PI,
                     align = "hv", nrow = 1, ncol = 2)

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
# title_60_100 <- ggdraw() + 
#   draw_label(paste0(TARGET_EXP, ": 60 to 100 cm depth"),
#              fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
#   # add margin on the left of the drawing canvas,
#   # so title is aligned with left edge of first plot
#   theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_60_100 <- plot_grid(m_60_100_res,
                      m_60_100_PI,
                      align = "hv", nrow = 1, ncol = 2)

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
# title_100_200 <- ggdraw() + 
#   draw_label(paste0(TARGET_EXP, ": 100 to 200 cm depth"),
#              fontface = 'bold', size = 12, x = 0.5, hjust = 0.5) +
#   # add margin on the left of the drawing canvas,
#   # so title is aligned with left edge of first plot
#   theme(plot.margin = margin(0, 0, 0, 7))

# combine using cowplot
m_100_200 <- plot_grid(m_100_200_res,
                       m_100_200_PI,
                       align = "hv", nrow = 1, ncol = 2)

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


