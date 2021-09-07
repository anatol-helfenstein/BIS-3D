#------------------------------------------------------------------------------
# Name:     QRF_comparison_approaches.R
#           (QRF = quantile regression forest)
#
# Content:  - train QRF model and predict newdata with a small representative
#             dataset to find out which pkgs etc. to use based on:
#               - can models be trained using cross-validation grouped by location?
#               - can the predict() fnc yield not just average predictions but also
#                 quantiles?
#
# Refs:     - QRF package and vignettes:
#             https://cran.r-project.org/web/packages/quantregForest/quantregForest.pdf
#             https://mran.microsoft.com/snapshot/2015-07-15/web/packages/quantregForest/vignettes/quantregForest.pdf
#           - Cross validation strategies for spatio-temporal data using "CAST" pkg:
#             https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html#model-training-and-prediction
#             https://cran.r-project.org/web/packages/CAST/vignettes/AOA-tutorial.html
#           
# Inputs:   - 
#
# Output:   -  
#
# Runtime:  - 
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  January 2021
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "quantregForest", "ranger", "caret", "CAST", "foreach")
lapply(pkgs, library, character.only = TRUE)



### Prepare modelling data --------------------------------------------------

# locate, read in and stack covariates to predict over
v_cov_names <- dir("out/data/covariates/final_stack",
                   pattern = "\\.grd$", recursive = TRUE)

ls_r_cov <- foreach(cov = 1:length(v_cov_names)) %do%
  raster(paste0("out/data/covariates/final_stack/", v_cov_names[[cov]]))

r_stack_cov <- stack(ls_r_cov)

# read in sample data: regression matrix of soil pH
tbl_regmat_target <- read_rds("out/data/model/tbl_regmat_pH_KCl_factor.Rds")

# decrease size of dataset for toy example
tbl_regmat_cal <- tbl_regmat_target %>% 
  filter(split %in% "train") %>% 
  .[1:800,]

tbl_regmat_val <- tbl_regmat_target %>% 
  filter(split %in% "test") %>% 
  .[1:200,]



### General QRF parameter definition and set up --------------------------------

# QRF parameters:
# - mtry: sets number of variables to try for each split when growing the tree;
#   same default is used as in randomForest (one third of the number of predictors)  
# - nodesize: fixes minimal number of instances in each terminal node, determining
#   how many observations at least lie in the same node (default = 10)
# - ntree: how many trees are grown in RF on which QRF are based on;
#   empirical evidence suggests that performance of prediction remains good even
#   when using only few trees (default = 100 trees)

# set seed to control randomness of cross-validation (CV)
set.seed(10)

# To prevent overfitting the model and improving separate & independent validation
# results later on, we make use of the "CAST" pkg of Hanna Meyer, embedded in a 
# "caret" framework. This allows machine learning (ML) for space-time data,
# for which models should be fit differently to non-spatio-temporal data...

# here, we use a 5-fold CV grouped by location (site_id) using CAST::CreatSpacetimeFolds()
indices <- CreateSpacetimeFolds(x = tbl_regmat_cal,
                                spacevar = "site_id",
                                k = 5)

# prepare table of predictors
tbl_predictors <- tbl_regmat_cal %>%
  dplyr::select(-(split:hor), -(pH_KCl)) %>% 
  as.data.frame() # "setting row names on tibble is deprecated"

# set mtry to 1/3 of # or predictors
n_mtry <- tbl_predictors %>% 
  ncol()/3 %>% 
  round(., 0) # not sure why round does not work???
n_mtry <- round(n_mtry)



### Fit QRF model using "quantregForest" pkg -----------------------------------

# set seed to control randomness of model training/fitting
set.seed(10)

# train QRF
system.time(
  qrf_fit_quantreg <- quantregForest(x = tbl_predictors,
                                     y = tbl_regmat_cal$pH_KCl,
                                     nthreads = 10L, # get NAs with > 10 threads
                                     keep.inbag = TRUE,
                                     importance = TRUE,
                                     quantiles = c(0.05,0.5,0.95),
                                     ntree = 500,
                                     mtry = n_mtry)
)
# time elapse 10 cores: 15 sec

# if newdata = NULL, predict() performs out-of-bag (OOB) prediction on dataset,
# i.e. for each of grown trees prediction for data points which were not used for
# fitting tree is done (no new data is involved)

# Per default only one observation per node is used for prediction. This can be
# set with the input argument all with default all=FALSE (one observation per 
# node used) and when setting all=TRUE, all observation per node are used. The
# use of only one observation per node is of advantage especially when working
# with large datasets since the algorithm can be very slow otherwise. Numerical
# experiments suggest that the performance remains good.
# In cases with big datasets & few new sample points as input for newdata,
# setting all = FALSE may be significantly slower than choosing all = TRUE.
# When newdata (test set) is very large, use all=FALSE (default) since much faster.

# control randomness of prediction (not sure if this is necessary for prediction?)
set.seed(10)

# OOB predictions using quantregForest approach
qrf_OOB_quantreg <- predict(qrf_fit_quantreg,
                            newdata = NULL,
                            what = c(0.05, 0.5, 0.95),
                            all = FALSE,
                            obs = 1)

# predict independent test dataset (LSK or CCNL)
qrf_test_quantreg <- predict(qrf_fit_quantreg,
                             newdata = tbl_regmat_val %>%
                               dplyr::select(-(split:hor), -pH_KCl),
                             what = c(0.05, 0.5, 0.95),
                             all = FALSE,
                             obs = 1)



### Fit QRF model using "ranger" pkg -------------------------------------------

set.seed(10)

# train QRF
system.time(
  qrf_fit_ranger <- ranger(x = tbl_predictors,
                           y = tbl_regmat_cal$pH_KCl,
                           num.tree = 500,
                           mtry = n_mtry,
                           importance = "permutation", # cannot choose both options
                           write.forest = TRUE,
                           keep.inbag = TRUE,
                           quantreg = TRUE, # this changes it to QRF instead of RF
                           oob.error = TRUE,
                           num.threads = 40L,
                           verbose = TRUE)
)
# time elapse 40 cores: 3 sec

# OOB predictions using ranger approach
qrf_OOB_ranger <- predict(qrf_fit_ranger,
                          data = NULL,
                          type = "quantiles",
                          quantiles = c(0.05, 0.5, 0.95),
                          verbose = TRUE)

# test predictions using ranger approach
qrf_test_ranger <- predict(qrf_fit_ranger,
                           data = tbl_regmat_val %>%
                             dplyr::select(-(split:hor), -pH_KCl),
                           type = "quantiles",
                           quantiles = c(0.05, 0.5, 0.95),
                           verbose = TRUE)



### Tune QRF model with "CAST" & "caret" & use either "quantregForest" or "ranger"
### ----------------------------------------------------------------------------

set.seed(10)

# train model using caret::train()
# using quantregForest() fnc of Meinhausen pkg
system.time(
  qrf_fit_quantreg_caret <- train(x = tbl_predictors,
                            y = tbl_regmat_cal$pH_KCl,
                            method = "qrf",
                            ntree = 500,
                            importance = TRUE,
                            keep.inbag = TRUE,
                            nthreads = 10L,
                            quantiles = c(0.05,0.5,0.95),
                            tuneGrid = data.frame("mtry" = n_mtry),
                            trControl = trainControl(method = "cv",
                                                     index = indices$index))
) # time elapse 10 cores: 41 sec

# make sure there are no NAs
qrf_fit_quantreg_caret$finalModel$predicted %>% 
  is.na() %>% 
  unique() # should be FALSE

# fit a QRF model using ranger
system.time(
  qrf_fit_ranger_caret <- train(x = tbl_predictors,
                          y = tbl_regmat_cal$pH_KCl,
                          method = "ranger",
                          num.trees = 500,
                          importance = "permutation",
                          #write.forest = TRUE,
                          keep.inbag = TRUE,
                          quantreg = TRUE, # this changes it to QRF instead of RF
                          oob.error = TRUE,
                          num.threads = 40L,
                          #quantiles = c(0.05,0.5,0.95),
                          tuneGrid = data.frame("mtry" = n_mtry,
                                                "splitrule" = "variance", # use default
                                                "min.node.size" = 5), # use default
                          trControl = trainControl(method = "cv",
                                                   index = indices$index))
) # time elapse 40 cores: 14 sec

# make sure there are no NAs
qrf_fit_ranger_caret$finalModel$predictions %>% 
  is.na() %>% 
  unique() # should be FALSE

# OOB predictions using ranger approach
qrf_OOB_ranger_caret <- predict(qrf_fit_ranger_caret$finalModel,
                                data = NULL,
                                type = "quantiles",
                                quantiles = c(0.05, 0.5, 0.95),
                                verbose = TRUE)

# test predictions using ranger approach
qrf_test_ranger_caret <- predict(qrf_fit_ranger_caret$finalModel,
                                 data = tbl_regmat_val %>%
                                   dplyr::select(-(split:hor), -pH_KCl),
                                 type = "quantiles",
                                 quantiles = c(0.05, 0.5, 0.95),
                                 verbose = TRUE)



### Compare different approaches -----------------------------------------------

# compare summaries of calibrated models (model fit/training)
qrf_fit_quantreg
qrf_fit_ranger
qrf_fit_ffs




### Use CAST's forward feature selection to remove covariates ------------------

set.seed(10)

# use CAST's forward feature selection method to remove variables that cause overfitting!
system.time(
  qrf_fit_ffs <- ffs(predictors = tbl_regmat_cal %>%
                       dplyr::select(-(split:hor), -pH_KCl) %>% 
                       as.data.frame(), # "setting row names on tibble is deprecated"
                     response = tbl_regmat_cal$pH_KCl,
                     metric = "Rsquared",
                     method = "ranger",
                     num.trees = 500,
                     importance = "permutation",
                     #write.forest = TRUE,
                     keep.inbag = TRUE,
                     quantreg = TRUE, # this changes it to QRF instead of RF
                     oob.error = TRUE,
                     num.threads = 48L,
                     tuneGrid = data.frame("mtry" = 2,
                                           "splitrule" = "variance", # use default
                                           "min.node.size" = 5), # use default
                     trControl = trainControl(method = "cv",
                                              index = indices$index),
                     withinSE = TRUE,
                     # to favour models with < variables & probably shorten calc time
                     verbose = TRUE)
)
# Just for this toy example with 800 obs and 183 predictors 33K models need to be fit
# takes several days, probably a week!!!

