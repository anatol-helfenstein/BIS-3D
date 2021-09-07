# Name:     50_model_evaluation_all_depths_PFB-OOB_PFB-CV_LSK.R
#
# Content:  - read in target variable regression matrix and fitted model
#           - use "ranger" pkg to predict mean and all quantiles
#           - evaluate map accuracy using 3 methods/measures:
#               - PFB-OOB: out of bag (OOB) calibration data
#               - PFB-CV: hold outs from location-grouped 10-fold CV of calibration data
#               - LSK: independent validation/test data: LSK/CCNL*
#               * Here we evaluate over all depths together and therefore cannot
#               account for stratified sampling design of LSK data (LSK-SRS); This is
#               implemented for each depth layer separately in script
#               "51_model_evaluation_depth_layers_PFB-OOB_PFB-CV_LSK_LSK-SRS.R"
#           - make predicted vs. observed plots and accuracy metrics for all
#             measures over all depths combined
#           - include Prediction Interval Coverage Probability (PICP) of 90%
#             prediction intervals (PI90) in plots
#           - calculate PICP of all PIs for all measures (LSK and LSK-SRS are
#             identical).
#           - variable importance plots for (30) most important covariates
#           - (assess validation results using GSM accuracy thresholds (A, AA, AAA),
#             NOT INCLUDED IN PAPER)
#
# Refs:     - QRF package and vignettes:
#             https://cran.r-project.org/web/packages/quantregForest/quantregForest.pdf
#             https://mran.microsoft.com/snapshot/2015-07-15/web/packages/quantregForest/vignettes/quantregForest.pdf
#           
# Inputs:   - target regression matrix: out/data/model/tbl_regmat_[TARGET].Rds
#           - final fitted QRF model using all calibration data (optimal):
#             "out/data/model/QRF_fit_[TARGET]_obs[]_p[]_[]CV_optimal.Rds"
#           - series of (10) QRF model fits to use for CV results:
#             "out/data/model/QRF_fit_[TARGET]_obs[]_p[]_[]CV_all_folds.Rds"
#
# Output:   - predicted vs. observed accuracy metrics plots including uncertainty
#             (PI90) of PFB-OOB-LSK
#           - PICP plot for all PIs of PFB-OOB, PFB-CV and LSK
#           - variable importance plot(s)
#           - (GSM accuracy thresholds of validation samples)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  June 2021



### Empty memory and workspace; load required packages -------------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "sf", "ranger", "caret", "foreach", "viridis", "cowplot",
          "ggspatial", "mapview", "data.table", "RColorBrewer", "scales")
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



### Explore model calibration (fit) --------------------------------------------

# summary of calibrated model (model fit)
QRF_FIT_optimal

# output above are averages of RMSE, Rsquared and MAE over all folds
QRF_FIT_optimal$resample

# compare results to results from all individual models (should be identical..?)
(tbl_qrf_all_folds_metrics <- tibble(RMSE = map(QRF_FIT_all_folds, ~.$results$RMSE) %>% 
         unlist(),
       Rsquared = map(QRF_FIT_all_folds, ~.$results$Rsquared) %>% 
         unlist(),
       MAE = map(QRF_FIT_all_folds, ~.$results$MAE) %>% 
         unlist(),
       Resample = paste0("Resample", 1:10)))

# metric used to choose final/optimal model
QRF_FIT_optimal$metric

# function used to select final/optimal model ("best" or "oneSE" rule of Breiman)
QRF_FIT_optimal$control$selectionFunction

# best tuning parameters
QRF_FIT_optimal$bestTune



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



### Accuracy plots (predicted vs. observed) and metrics of PFB-OOB-LSK ---------------

## PFB-OOB: OOB ---------------------------------------------------------------------
# gather model accuracy metrics of PFB-OOB
tbl_sum_stats_cal_OOB <- tibble(
  n = nrow(tbl_predobs_cal_OOB),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_OOB$obs - tbl_predobs_cal_OOB$pred_mean),
  me_median = mean(tbl_predobs_cal_OOB$obs - tbl_predobs_cal_OOB$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_OOB$obs - tbl_predobs_cal_OOB$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_OOB$obs - tbl_predobs_cal_OOB$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_OOB$obs - tbl_predobs_cal_OOB$pred_mean)^2))/
             sum((tbl_predobs_cal_OOB$obs - mean(tbl_predobs_cal_OOB$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_OOB$obs - tbl_predobs_cal_OOB$quant_50)^2))/
             sum((tbl_predobs_cal_OOB$obs - mean(tbl_predobs_cal_OOB$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_OOB, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_OOB, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec = as.character(as.expression(paste0("MEC == ", round(mec, 2)))))

# plot predicted (OOB) vs. observed; using geom_hex
p_pred_obs_OOB <- tbl_predobs_cal_OOB %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_hex(bins = 100) +
  scale_fill_viridis("Counts") +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
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
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_pred_obs_OOB.pdf"),
#        plot = p_pred_obs_OOB,
#        path = paste0("out/figs/models/", TARGET),
#        width = 6, height = 6)

# plot predicted (OOB) vs. observed including 90% prediction interval (error bars)
p_pred_obs_OOB_PI90 <- tbl_predobs_cal_OOB %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_sum_stats_cal_OOB$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_sum_stats_cal_OOB$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  ggtitle("PFB-OOB") +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  labs(col = "PFB-OOB:",
       shape = "PFB-OOB:") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_pred_obs_OOB_PI90.pdf"),
#        plot = p_pred_obs_OOB_PI90,
#        path = paste0("out/figs/models/", TARGET),
#        width = 8, height = 6)


## PFB-CV: Location-grouped 10-fold CV ---------------------------------------------
# gather model accuracy metrics of PFB-CV
tbl_sum_stats_cal_cv <- tibble(
  n = nrow(tbl_predobs_cal_cv),
  one_one = "1:1",
  me = mean(tbl_predobs_cal_cv$obs - tbl_predobs_cal_cv$pred_mean),
  me_median = mean(tbl_predobs_cal_cv$obs - tbl_predobs_cal_cv$quant_50),
  rmse = sqrt(mean((tbl_predobs_cal_cv$obs - tbl_predobs_cal_cv$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_cal_cv$obs - tbl_predobs_cal_cv$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_cal_cv$obs - tbl_predobs_cal_cv$pred_mean)^2))/
             sum((tbl_predobs_cal_cv$obs - mean(tbl_predobs_cal_cv$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_cal_cv$obs - tbl_predobs_cal_cv$quant_50)^2))/
             sum((tbl_predobs_cal_cv$obs - mean(tbl_predobs_cal_cv$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_cal_cv, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_cal_cv, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec = as.character(as.expression(paste0("MEC == ", round(mec, 2)))))

# plot predicted (CV) vs. observed; using geom_hex
p_pred_obs_cv <- tbl_predobs_cal_cv %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_hex(bins = 100) +
  scale_fill_viridis("Counts") +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
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
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_pred_obs_cv.pdf"),
#        plot = p_pred_obs_cv,
#        path = paste0("out/figs/models/", TARGET),
#        width = 6, height = 6)

# plot predicted (CV) vs. observed including 90% prediction interval (error bars)
p_pred_obs_cv_PI90 <- tbl_predobs_cal_cv %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_sum_stats_cal_cv$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_sum_stats_cal_cv$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
            hjust = 1.12, vjust = -1.5, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  ggtitle("PFB-CV") +
  scale_x_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  scale_y_continuous(breaks = XY_BREAKS_CAL,
                     limits = c(XY_MIN_CAL - 0.01 * XY_RANGE_CAL,
                                XY_MAX_CAL + 0.01 * XY_RANGE_CAL)) +
  coord_fixed(ratio = 1) +
  labs(col = "PFB-CV",
       shape = "PFB-CV") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_pred_obs_cv_PI90.pdf"),
#        plot = p_pred_obs_cv_PI90,
#        path = paste0("out/figs/models/", TARGET),
#        width = 8, height = 6)


## LSK: Independent validation dataset ------------------------------------------

# gather model sum statistics: metrics of independent validation data
tbl_sum_stats_val <- tibble(
  n = nrow(tbl_predobs_val),
  one_one = "1:1",
  me = mean(tbl_predobs_val$obs - tbl_predobs_val$pred_mean),
  me_median = mean(tbl_predobs_val$obs - tbl_predobs_val$quant_50),
  rmse = sqrt(mean((tbl_predobs_val$obs - tbl_predobs_val$pred_mean)^2)),
  rmse_median = sqrt(mean((tbl_predobs_val$obs - tbl_predobs_val$quant_50)^2)),
  mec = 1-((sum((tbl_predobs_val$obs - tbl_predobs_val$pred_mean)^2))/
             sum((tbl_predobs_val$obs - mean(tbl_predobs_val$obs))^2)),
  mec_median = 1-((sum((tbl_predobs_val$obs - tbl_predobs_val$quant_50)^2))/
             sum((tbl_predobs_val$obs - mean(tbl_predobs_val$obs))^2)),
  n_within_PI90 = nrow(filter(tbl_predobs_val, within_PI90 %in% TRUE))/n,
  n_within_PI90_not = nrow(filter(tbl_predobs_val, within_PI90 %in% FALSE))/n) %>%
  # Modify columns for plot annotation
  mutate(n = as.character(as.expression(paste0("italic(n) == ", n))),
         me = as.character(as.expression(paste0("ME == ", round(me, 2)))),
         rmse = as.character(as.expression(paste0("RMSE == ", round(rmse, 2)))),
         mec = as.character(as.expression(paste0("MEC == ", round(mec, 2)))))

# plot predicted vs. observed including 90% prediction interval (error bars)
p_pred_obs_val <- tbl_predobs_val %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_sum_stats_val$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_sum_stats_val$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -6.8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -4.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.12, vjust = -2.6, parse = TRUE) +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
            hjust = 1.13, vjust = -0.8, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  ggtitle("LSK") +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  labs(col = "LSK:",
       shape = "LSK:") +
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_pred_obs_val_PI90.pdf"),
#        plot = p_pred_obs_val,
#        path = paste0("out/figs/models/", TARGET),
#        width = 8, height = 6)



## PFB-OOB-LSK: Cowplot of accuracy plots and metrics --------------------------------

# retrieve legend from plot elements
p_pred_obs_OOB_PI90_legend <- tbl_predobs_cal_OOB %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_sum_stats_cal_OOB$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_sum_stats_cal_OOB$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
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
  labs(col = "PFB-OOB:",
       shape = "PFB-OOB:") +
  theme_bw() +
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))

p_pred_obs_cv_PI90_legend <- tbl_predobs_cal_cv %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_sum_stats_cal_cv$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_sum_stats_cal_cv$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -5.75, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.08, vjust = -3.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_cal_cv,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
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
  labs(col = "PFB-CV:",
       shape = "PFB-CV:") +
  theme_bw() +
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9))

p_pred_obs_val_legend <- tbl_predobs_val %>% 
  mutate(within_PI90 = case_when(
    within_PI90 %in% TRUE ~ paste0('TRUE (',round(tbl_sum_stats_val$n_within_PI90, 3), ')'),
    within_PI90 %in% FALSE ~ paste0('FALSE (', round(tbl_sum_stats_val$n_within_PI90_not, 3), ')'))) %>% 
  ggplot(aes(x = pred_mean, y = obs)) +
  geom_errorbar(aes(xmin = quant_5, xmax = quant_95, y = obs),
                color = "gray") +
  geom_point(aes(color = within_PI90, shape = within_PI90)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  scale_shape_manual(values = c(4, 21)) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = n), size = 3,
            hjust = 1.15, vjust = -6.8, parse = TRUE) +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = me), size = 3,
            hjust = 1.12, vjust = -4.5, parse = TRUE) +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = rmse), size = 3,
            hjust = 1.12, vjust = -2.6, parse = TRUE) +
  geom_text(data = tbl_sum_stats_val,
            aes(x = Inf, y = -Inf, label = mec), size = 3,
            hjust = 1.13, vjust = -0.8, parse = TRUE) +
  ylab(as.expression(paste(TARGET_EXP))) +
  xlab(TARGET_PRED) +
  scale_x_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  scale_y_continuous(breaks = XY_BREAKS_VAL,
                     limits = c(XY_MIN_VAL - 0.01 * XY_RANGE_VAL,
                                XY_MAX_VAL + 0.01 * XY_RANGE_VAL)) +
  coord_fixed(ratio = 1) +
  labs(col = "LSK:",
       shape = "LSK:") +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9))

legend_OOB <- get_legend(p_pred_obs_OOB_PI90_legend)
legend_cv <- get_legend(p_pred_obs_cv_PI90_legend)
legend_val <- get_legend(p_pred_obs_val_legend)

# title for all 3 legends
legend_title <- ggdraw() + 
  draw_label("PICP of PI90", size = 10, x = 0, hjust = -0.3, vjust = 1) +
  theme(plot.margin = margin(0, 0, 0, 0))

# combined accuracy plot and metrics of PFB-OOB, PFB-CV and LSK
p_pred_obs_PI90 <- plot_grid(p_pred_obs_OOB_PI90,
                             p_pred_obs_cv_PI90,
                             p_pred_obs_val,
                             plot_grid(legend_title,
                                       plot_grid(legend_OOB, legend_cv, legend_val,
                                                 nrow = 3, ncol = 1, align = "hv"),
                                       nrow = 2, ncol = 1, align = "hv",
                                       rel_heights = c(0.1, 0.9)),
                             nrow = 2, ncol = 2, align = "hv",
                             rel_widths = c(0.5, 0.5),
                             rel_heights = c(0.5, 0.5),
                             axis = "tblr")

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_pred_obs_PFB-OOB_PFB-CV_LSK.pdf"),
#        plot = p_pred_obs_PI90,
#        path = paste0("out/figs/models/", TARGET),
#        width = 9, height = 9)



### PICP for all 4 measures using all prediction quantiles ---------------------

# prepare column names for range of prediction intervals
v_PI_names <- foreach(PI = 1:50) %do%
  paste0("PI", PI * 2) %>% 
  unlist()

# create logical columns if each observation is in each PI
# calibration OOB data
tbl_predobs_cal_OOB <- foreach(PI = 1:50) %do% {
  mutate(tbl_predobs_cal_OOB,
         PI = if_else(obs >= across(paste0("quant_", 50 - PI)) &
                             obs <= across(paste0("quant_", 50 + PI)),
                           TRUE, FALSE))
} %>% 
  map(., ~select(.x, PI)) %>% 
  bind_cols() %>% 
  rename_at(vars(PI...1:PI...50), ~ v_PI_names) %>% 
  bind_cols(tbl_predobs_cal_OOB, .)
# calibration CV data
tbl_predobs_cal_cv <- foreach(PI = 1:50) %do% {
  mutate(tbl_predobs_cal_cv,
         PI = if_else(obs >= across(paste0("quant_", 50 - PI)) &
                             obs <= across(paste0("quant_", 50 + PI)),
                           TRUE, FALSE))
} %>% 
  map(., ~select(.x, PI)) %>% 
  bind_cols() %>% 
  rename_at(vars(PI...1:PI...50), ~ v_PI_names) %>% 
  bind_cols(tbl_predobs_cal_cv, .)
# validation data
tbl_predobs_val <- foreach(PI = 1:50) %do% {
  mutate(tbl_predobs_val,
         PI = if_else(obs >= across(paste0("quant_", 50 - PI)) &
                             obs <= across(paste0("quant_", 50 + PI)),
                           TRUE, FALSE))
} %>% 
  map(., ~select(.x, PI)) %>% 
  bind_cols() %>% 
  rename_at(vars(PI...1:PI...50), ~ v_PI_names) %>% 
  bind_cols(tbl_predobs_val, .)

# Calculate table of PICP: PI & proportion of observations within
# calibration OOB data
tbl_PICP_OOB <- tbl_predobs_cal_OOB %>% 
  summarise(across(PI2:PI100, summary)) %>% 
  add_column(var = c("class", "false", "true"), .before = "PI2") %>% 
  pivot_longer(cols = PI2:PI100) %>% 
  spread(key = var, value = value) %>% 
  mutate(PI = stringr::str_replace(name, "PI", "")) %>% 
  select(-c(class, name)) %>%
  mutate(false = as.numeric(false),
         true = as.numeric(true)) %>% 
  mutate(proportion_within = true/(false + true)) %>% 
  mutate(PI = as.numeric(PI)) %>% 
  arrange(PI) %>% 
  add_column(dataset = "PFB-OOB")
# calibration CV data
tbl_PICP_cv <- tbl_predobs_cal_cv %>% 
  summarise(across(PI2:PI100, summary)) %>% 
  add_column(var = c("class", "false", "true"), .before = "PI2") %>% 
  pivot_longer(cols = PI2:PI100) %>% 
  spread(key = var, value = value) %>% 
  mutate(PI = stringr::str_replace(name, "PI", "")) %>% 
  select(-c(class, name)) %>%
  mutate(false = as.numeric(false),
         true = as.numeric(true)) %>% 
  mutate(proportion_within = true/(false + true)) %>% 
  mutate(PI = as.numeric(PI)) %>% 
  arrange(PI) %>% 
  add_column(dataset = "PFB-CV")
# validation data
tbl_PICP_val <- tbl_predobs_val %>% 
  summarise(across(PI2:PI100, summary)) %>% 
  add_column(var = c("class", "false", "true"), .before = "PI2") %>% 
  pivot_longer(cols = PI2:PI100) %>% 
  spread(key = var, value = value) %>% 
  mutate(PI = stringr::str_replace(name, "PI", "")) %>% 
  select(-c(class, name)) %>%
  mutate(false = as.numeric(false),
         true = as.numeric(true)) %>% 
  mutate(proportion_within = true/(false + true)) %>% 
  mutate(PI = as.numeric(PI)) %>% 
  arrange(PI) %>% 
  add_column(dataset = "LSK and LSK-SRS")

# combine PICP tables of calibration (OOB and CV) and validation data
tbl_PICP <- full_join(tbl_PICP_OOB, tbl_PICP_cv) %>% 
  full_join(., tbl_PICP_val) %>% 
  select(dataset, PI, proportion_within) %>% 
  mutate(PI = PI/100) %>% # results as proportion, not as percentage
  arrange(PI)

# assign levels to datasets so that they are in correct order
dataset_order <- c("PFB-OOB", "PFB-CV", "LSK and LSK-SRS")
tbl_PICP$dataset <- factor(x = tbl_PICP$dataset,
                           levels = dataset_order)

# plot PICP of calibration (OOB & CV) and validation data
p_PICP <- tbl_PICP %>% 
  ggplot(aes(x = PI, y = proportion_within, color = dataset)) +
  geom_point(aes(shape = dataset, fill = dataset), size = 1.5) +
  geom_line() +
  # scale_colour_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_colour_manual(values = c("black", "black", "blue")) +
  # scale_shape_manual(values = c(16, 15, 3)) +
  scale_shape_manual(values = c(21, 22, 25)) +
  scale_fill_manual(values = c("black", "black", "blue")) +
  geom_abline(slope = 1, intercept = 0, color = "gray") +
  geom_text(data = tbl_sum_stats_cal_OOB,
            aes(x = Inf, y = Inf, label = one_one), size = 3,
            hjust = 2.5, vjust = 2, colour = "gray") +
  xlab(as.expression("Prediction Interval (PI)")) +
  ylab(as.expression("Proportion of observations within PI")) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  coord_fixed(ratio = 1) +
  labs(color = NULL, shape = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = c(0.82, 0.12))

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_PICP.pdf"),
#        plot = p_PICP,
#        path = paste0("out/figs/models/", TARGET),
#        width = 5, height = 5)



### Variable importance --------------------------------------------------------

# Variable importance measures:

# Permutation: computed from permuting OOB data: For each tree, prediction error on
# out-of-bag (OOB) portion of data is recorded (error rate for classification,
# MSE for regression). Then same is done after permuting each predictor variable.
# Difference between the two are then averaged over all trees, and normalized by
# the standard deviation of the differences. If the standard deviation of the
# differences is equal to 0 for a variable, the division is not done
# (but the average is almost always equal to 0 in that case).

# Impurity: variable importance measured using node impurity:
# The second measure is the total decrease in node impurities from splitting on
# variable, averaged over all trees. For regression, measured by residual sum of squares.

# mode chosen during model training to calculate variable importance
QRF_FIT_optimal$finalModel$importance.mode

# 20 most importance variables using mode chosen during model training (see above)
varImp(QRF_FIT_optimal)

# tbl of variable importances for all covariates
tbl_var_imp <- tibble(Covariate = names(QRF_FIT_optimal$finalModel$variable.importance),
                      Importance = QRF_FIT_optimal$finalModel$variable.importance) %>% 
  arrange(-Importance)

# plot variable importance (30 most important ones) using model chosen (see above)
p_var_imp <- tbl_var_imp %>% 
  slice(1:30) %>% 
  ggplot(., aes(y = reorder(Covariate, Importance))) +
  geom_bar(aes(weight = Importance)) +
  xlab("Variable importance (permutation)") +
  ylab("Covariates (p = 30)") +
  theme_bw()

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_var_imp_permutation.pdf"),
#        plot = p_var_imp,
#        path = paste0("out/figs/models/", TARGET),
#        width = 8, height = 8)

# local variable importance (for each sample)
# ATTENTION: check if this is correct!
tbl_var_imp_local <- as_tibble(QRF_FIT_optimal$finalModel$variable.importance.local) %>% 
  bind_cols(tbl_regmat_target %>% 
              filter(split %in% "train") %>% 
              dplyr::select(split:hor),
            .) %>% 
  rownames_to_column('id') %>% 
  gather(covariate, count, d_upper:water_wetness_probability_2015_25m) %>% 
  group_by(id) %>% 
  slice(which.max(count)) %>% 
  ungroup()

# plot local variable importance
p_var_imp_local <- plyr::count(tbl_var_imp_local, vars = "covariate") %>% 
  as_tibble() %>%
  filter(!freq %in% 1) %>% 
  ggplot(., aes(y = reorder(covariate, freq))) +
  geom_bar(aes(weight = freq)) +
  xlab("Local variable importance") +
  ylab("Covariates") +
  theme_bw()

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_var_imp_local.pdf"),
#        plot = p_var_imp_local,
#        path = paste0("out/figs/models/", TARGET),
#        width = 6, height = 8)



### GSM accuracy thresholds ----------------------------------------------------

tbl_predobs_val <- tbl_predobs_val %>% 
  mutate(residual = abs(obs - pred_mean)) %>% 
  mutate(thresh = case_when(residual <= 0.5 ~ "AAA",
                            residual <= 1.0 ~ "AA",
                            residual <= 1.5 ~ "A",
                            residual > 1.5 ~ "none"))

per_AAA = tbl_predobs_val %>% 
  filter(thresh == "AAA") %>% 
  nrow()/nrow(tbl_predobs_val) * 100

per_AA = tbl_predobs_val %>% 
  filter(thresh == "AA") %>% 
  nrow()/nrow(tbl_predobs_val) * 100

per_A = tbl_predobs_val %>% 
  filter(thresh == "A") %>% 
  nrow()/nrow(tbl_predobs_val) * 100

per_none = tbl_predobs_val %>% 
  filter(thresh == "none") %>% 
  nrow()/nrow(tbl_predobs_val) * 100

# combine results into tbl
tbl_thresholds <- tibble(Threshold = factor(x = c("AAA", "AA", "A", "none"),
                                            levels = c("AAA", "AA", "A", "none")),
                         percent = c(per_AAA, per_AA, per_A, per_none)) %>%
  # add position variable to label pie chart
  mutate(pos = 100 - (cumsum(percent) - 0.5 * percent)) %>% 
  # add percentage as text including percent sign for labelling pie chart
  mutate(percent_char = as.character(as.expression(paste0(round(percent, 0), "%"))))

# Make pie chart of GSM accuracy thresholds of validation set
p_thresholds <- ggplot(tbl_thresholds,
                       aes(x = "", y = percent, fill = Threshold)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(x = "", y = pos, label = percent_char), color = "black") +
  coord_polar("y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("green", "yellow", "orange", "red")) +
  ggtitle("Percent of validation samples within GSM accuracy thresholds")

# save plot to disk
# ggsave(filename = paste0("p_QRF_", TARGET, "_val_thresholds.pdf"),
#        plot = p_thresholds,
#        path = paste0("out/figs/models/", TARGET),
#        width = 6, height = 6)


