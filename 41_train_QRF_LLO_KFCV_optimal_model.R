#------------------------------------------------------------------------------
# Name:     41_train_QRF_LLO_KFCV_optimal_model.R
#           (QRF = quantile regression forest, LLO = leave-location-out,
#           KFCV = k-fold cross-validation)
#
# Content:  - read in regression matrix and tuning grid of hyperparameters
#             specific to chosen target soil property (response/dependent var)
#           - fit QRF model using a cross-validation (CV) grouped by location of
#             the training data (requires "caret", "CAST" and "quantregForest" or
#             "ranger" pkg ("ranger" is preferable because much faster)) and
#             optimal hyperparameters
#           - save trained/fitted/calibrated model to disk 
#
# Refs:     - QRF package and vignettes:
#             https://cran.r-project.org/web/packages/quantregForest/quantregForest.pdf
#             https://mran.microsoft.com/snapshot/2015-07-15/web/packages/quantregForest/vignettes/quantregForest.pdf
#           - Cross validation strategies for spatio-temporal data using "CAST" pkg:
#             https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html
#             https://cran.r-project.org/web/packages/CAST/vignettes/AOA-tutorial.html
#           - Model tuning using "caret" pkg and RF hyperparameters:
#             https://topepo.github.io/caret/model-training-and-tuning.html
#             https://bradleyboehmke.github.io/HOML/random-forest.html#hyperparameters
#           
# Inputs:   - regression matrix of chosen target soil property:
#             "out/data/model/tbl_regmat_[TARGET].Rds"
#           - tuning grid of hyperparameters specific to chosen target soil property:
#             "out/data/model/tbl_QRF_hypertuning_[TARGET].Rds"
#
# Output:   - saved QRF model fit: into "out/data/model/" directory 
#
# Runtime:  - approx. 2 hr entire script
#             (for 16K obs, 195 p, 48 cores, 10-fold CV, 500 trees)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  April 2021
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "ranger", "caret", "CAST")
lapply(pkgs, library, character.only = TRUE)
# QRF can be calibrated using "ranger" or "quantregForest" (original Meinhausen pkg),
# but "ranger" implementation much faster because uses C++ and better use of cores



### Prepare modelling data --------------------------------------------------

# 1) Specify target soil property:
TARGET = "pH_KCl"

# 2) Read in regression matrix specific to target soil property
tbl_regmat_target <- read_rds(paste0("out/data/model/tbl_regmat_",
                                     TARGET, ".Rds"))

# 3) Read in tuning grid of hyperparameters specific to target soil property
tbl_tuning_extensive <- read_rds(paste0("out/data/model/tbl_RF_hypertuning_",
                                        TARGET, ".Rds"))

# 4) Specify number of cores/threads to use for parallel computation
THREADS = parallel::detectCores()
# use full computational power of SGL server



### Train optimal QRF model ----------------------------------------------------

# dataframe of covariate/predictors values at training locations
df_cov_train <- tbl_regmat_target %>%
  filter(split %in% "train") %>% 
  dplyr::select(-(split:hor), -all_of(TARGET)) %>% 
  as.data.frame() # "setting row names on tibble is deprecated"

# vector of response variable values (target soil property) at training locations
v_response <- tbl_regmat_target %>%
  filter(split %in% "train") %>%
  pull(all_of(TARGET))

# To prevent overfitting the model and improving separate & independent validation
# results later on, we make use of the "CAST" pkg of Hanna Meyer, embedded in a 
# "caret" framework. This allows machine learning (ML) for space-time data,
# for which models should be fit differently to non-spatio-temporal data...

# set seed to control randomness of cross-validation (CV)
set.seed(2021)

# Use 10-fold CV grouped by location (site_id) using CAST::CreatSpacetimeFolds()
indices <- CreateSpacetimeFolds(x = tbl_regmat_target %>%
                                  filter(split %in% "train"),
                                spacevar = "site_id",
                                k = 10)

# number of predictors/covariates used
P = ncol(df_cov_train)

# choose hyperparameters that performed best (lowest RMSE) from tuning grid
df_tuning_optimal <- expand.grid(mtry = tbl_tuning_extensive$mtry[1],
                                 min.node.size = tbl_tuning_extensive$min.node.size[1],
                                 splitrule = tbl_tuning_extensive$splitrule[1])

# Control randomness of sampling in each fold in multi-core environment
set.seed(2021)
# 10 folds + final model
seeds <- vector(mode = "list", length = length(indices$index) + 1)
for (i in 1:length(indices$index)) {
  seeds[[i]] <- sample.int(1000, 1) 
  # only 1 seed per fold because only 1 model (optimal); see ?trainControl seeds
}
# For the final model:
seeds[[11]] <- sample.int(1000, 1)

# set nuances of model training:
# By default, train chooses tuning parameter with best performance (e.g. lowest RMSE)
# "one standard error" rule of Breiman et al. (1984) suggests that tuning
# parameter associated with the best performance may overfit and so its better
# to choose simplest model within one standard error (SE) of the empirically
# optimal model; useful for simple trees that easily overfit but little or no
# effect for RF...

# trainControl for optimal final model
train_control_optimal <- trainControl(
  method = "cv",
  returnResamp = "all", # save all hold-out predictions for each resample
  savePredictions = "all",
  selectionFunction = "best", # or "oneSE"
  index = indices$index,
  seeds = seeds, # 1 seed for each fold + 1 for final model
  allowParallel = THREADS
)
# indexFinal = optional vector of integers indicating which samples are used to
# fit the final model after resampling. If NULL, then entire data set is used.

# set seed to control randomness of model training/fitting
set.seed(2021)
# There are rare cases where the underlying model function does not control the
# random number seed, especially if the computations are conducted in C code!
# Could this be the case for RF using ranger (written in C)?

# train model using caret::train() & "ranger" implementation
system.time(
  qrf_fit_optimal <- train(
    x = df_cov_train,
    y = v_response,
    method = "ranger",
    metric = "RMSE",
    num.trees = tbl_tuning_extensive$num.trees[1],
    importance = "permutation", # Not possible to retrieve both importance measures (?)
    replace = tbl_tuning_extensive$replace[1],
    sample.fraction = tbl_tuning_extensive$sample.fraction[1],
    local.importance = TRUE, # Var imp for each predictor & each sample
    keep.inbag = TRUE,
    quantreg = TRUE, # this changes it to QRF instead of RF
    oob.error = TRUE,
    num.threads = THREADS, # doesn't seem to work (no status bar)
    verbose = TRUE,
    # seed = 2021,
    tuneGrid = df_tuning_optimal,
    trControl = train_control_optimal
  )
)
# time elapse "optimal" model on 48 cores: 50 min
# time elapse "optimal" model (ntrees = 250) on 48 cores: 12 min
# time elapse "optimal" model (ntrees = 250, returnResamp TRUE) on 48 cores: 40 min

# summary of calibrated model (model fit)
qrf_fit_optimal

# in case of varying hyperparameters:
# plot(qrf_fit)

# output above are averages of RMSE, Rsquared and MAE over all folds
qrf_fit_optimal$resample

# make sure there are no NAs
qrf_fit_optimal$finalModel$predictions %>% 
  is.na() %>% 
  unique() # should be FALSE

# save fitted QRF model
write_rds(qrf_fit_optimal, paste0('out/data/model/QRF_fit_', TARGET,
                          '_obs', length(qrf_fit_optimal$finalModel$predictions),
                          '_p', P,
                          '_LLO_', length(qrf_fit_optimal$control$index),
                          'FCV_optimal.Rds'))



### Work-around to also save each fold of CV as separate QRF model -------------

# Since we also want the cross-validated predictions for all quantiles, we
# achieve that here by fitting a QRF 10 times using the sampling indices of one
# fold in 10-fold CV; i.e. 1st model is a single training/test split using the
# first fold indices, 2nd model is a single training/test split using second
# fold indices, etc. etc.

# create empty list ready to fill with each model object (10x)
ls_models <- vector(mode = "list", length = length(indices$index))

# prepare and fit each model in loop
system.time(
  for(i in 1:length(indices$index)) {
    
    # Control randomness of sampling in single training/test split in multi-core env
    set.seed(2021)

    # trainControl for 1 training/test split using indices from 1 fold
    train_control_single_split <- trainControl(
      method = "LGOCV",
      number = 1, # 1 training/test split
      selectionFunction = "best", # or "oneSE"
      index = indices$index[i],
      indexOut = indices$indexOut[i],
      # fit final model using only training samples of fold i
      indexFinal = indices$index[[i]],
      # 1 seed for each fold + 1 for final model
      seeds = c(seeds[i], seeds[11]),
      allowParallel = THREADS
      )
    # indexFinal = optional vector of integers indicating which samples are used to
    # fit the final model after resampling. If NULL, then entire data set is used.

    # set seed to control randomness of model training/fitting
    set.seed(2021)
    # There are rare cases where the underlying model function does not control the
    # random number seed, especially if the computations are conducted in C code!
    # Could this be the case for RF using ranger (written in C)?

    # train model using caret::train() & "ranger" implementation
    qrf_fit_single_split <- train(
      x = df_cov_train,
      y = v_response,
      method = "ranger",
      metric = "RMSE",
      num.trees = tbl_tuning_extensive$num.trees[1],
      importance = "permutation", # Not possible to retrieve both importance measures (?)
      replace = tbl_tuning_extensive$replace[1],
      sample.fraction = tbl_tuning_extensive$sample.fraction[1],
      local.importance = TRUE, # Var imp for each predictor & each sample
      keep.inbag = TRUE,
      quantreg = TRUE, # this changes it to QRF instead of RF
      oob.error = TRUE,
      num.threads = THREADS, # doesn't seem to work (no status bar)
      verbose = TRUE,
      # seed = 2021,
      tuneGrid = df_tuning_optimal,
      trControl = train_control_single_split
    )
    # time elapse 1 model single split on 48 cores: 8 min
    
    # save each model in list
    ls_models[[i]] <- qrf_fit_single_split
    
    }
)
# time elapse 10 models using 48 cores: 1.3 h (78 min)

# save all folds of training data as separate models
write_rds(ls_models, paste0('out/data/model/QRF_fit_', TARGET,
                            '_obs', length(qrf_fit_optimal$finalModel$predictions),
                            '_p', P,
                            '_LLO_', length(qrf_fit_optimal$control$index),
                            'FCV_all_folds.Rds'))


