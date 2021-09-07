#------------------------------------------------------------------------------
# Name:     40_train_RF_LLO_KFCV_hyperparameter_tuning.R
#           (RF = random forest, LLO = leave-location-out,
#           KFCV = k-fold cross-validation)
#
# Content:  - read in BIS regression matrix and select target (response) variable
#           - turn categorical variables into factors
#           - split into training and test data
#           - fit RF models all using a cross-validation (CV) grouped by
#             location of the training data (requires "caret", "CAST" and
#             "ranger" pkg ("ranger" is preferable because much faster)
#           - different RF models are fit using a full cartesian grid of hyper-
#             parameters (e.g. ntree, mtry, node size, splitrule, resampling
#             type and size) and model performance values for each set of hyper-
#             parameters is saved
#
# Refs:     - Cross validation strategies for spatio-temporal data using "CAST" pkg:
#             https://cran.r-project.org/web/packages/CAST/vignettes/CAST-intro.html
#             https://cran.r-project.org/web/packages/CAST/vignettes/AOA-tutorial.html
#           - Model tuning using "caret" pkg and RF hyperparameters:
#             https://topepo.github.io/caret/model-training-and-tuning.html
#             https://bradleyboehmke.github.io/HOML/random-forest.html#hyperparameters
#           
# Inputs:   - regression matrix of entire BIS: out/data/model/tbl_regmat_BIS.Rds
#           - covariates and covariate metadata (here only used to check levels
#             of categorical covariates across entire NL):
#             out/data/covariates/final_stack/
#             data/covariates/covariates_metadata.csv
#
# Output:   - saved target variable data set
#           - saved table of model performance results of hyperparameter combinations:
#             out/data/model/tbl_RF_hypertuning_[TARGET]_[selectionFunction].Rds
#             out/data/model/tbl_RF_hypertuning_[TARGET]_[selectionFunction].csv
#
# Runtime:  - several hours depending on number of hyperparameters
#           - e.g. hyperparameter grid of 336 rows on 48 cores: 5 h 50 min
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  July 2021
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "ranger", "caret", "CAST", "foreach")
lapply(pkgs, library, character.only = TRUE)



### Prepare modelling data --------------------------------------------------

# 1) Specify DSM target soil property:
TARGET = "pH_KCl"

# 2) Specify number of cores/threads to use for parallel computation
THREADS = parallel::detectCores()
# use full computational power of SGL server

# locate, read in and stack covariates to predict over
v_cov_names <- dir("out/data/covariates/final_stack",
                   pattern = "\\.grd$", recursive = TRUE)

ls_r_cov <- foreach(cov = 1:length(v_cov_names)) %do%
  raster(paste0("out/data/covariates/final_stack/", v_cov_names[[cov]]))

r_stack_cov <- stack(ls_r_cov)

# read in regression matrix of entire BIS
system.time(
  tbl_regmat_BIS <- read_rds("out/data/model/tbl_regmat_BIS.Rds")
  ) # time elapse: 7 min to read in data!

# possible target soil properties
tbl_regmat_BIS %>% 
  dplyr::select(soil_target) %>% 
  unnest_legacy() %>% 
  colnames()

# select soil property of interest
tbl_regmat_target <- tbl_regmat_BIS %>% 
  unnest_legacy(soil_target, .preserve = c(cov, soil_chem, soil_phys, soil_profile,
                                           env_fact, metadata, unknown)) %>% 
  dplyr::select(BIS_tbl:d_mid, all_of(TARGET), cov) %>% 
  filter_at(vars(all_of(TARGET)), all_vars(!is.na(.))) %>% 
  unnest_legacy(cov)

# designate categorical variables as factors; first read in covariate metadata
tbl_cov_meta <- read_csv("data/covariates/covariates_metadata.csv") %>% 
  # only interested in covariates we use in model
  filter(name %in% names(r_stack_cov))

# vector of categorical covariate names
v_cat <- tbl_cov_meta %>% 
  filter(values_type %in% "categorical") %>% 
  .$name %>% 
  sort()

# designate all categorical variables as factors
# (do not do this for entire BIS tbl because factors take up a lot more memory)
tbl_regmat_target <- tbl_regmat_target %>%
  mutate_if(., colnames(.) %in% v_cat, as.factor)

# split into training and test set (LSK or CCNL = test set)
tbl_regmat_target <- tbl_regmat_target %>% 
  mutate(split = case_when(!BIS_tbl %in% "CCNL" & !BIS_tbl %in% "LSK" ~ "train",
                           BIS_tbl %in% "CCNL" | BIS_tbl %in% "LSK" ~ "test"),
         .before = "BIS_tbl")

# number of levels of categorical variables in training dataset
v_lev_train <- tbl_regmat_target %>% 
  filter(split %in% "train") %>% 
  dplyr::select(all_of(v_cat)) %>% 
  map(., ~levels(.x)) %>% 
  map(., ~length(.x)) %>% 
  unlist()

# total number of levels of categorical variables across entire NL
v_lev_cov <- foreach(cat = 1:length(v_cat)) %do%
  nrow(levels(r_stack_cov[[v_cat]][[cat]])[[1]]) %>% 
  unlist()

# these categorical covariates need to be reclassified
(v_cat_recl <- tbl_regmat_target %>% 
  dplyr::select(all_of(v_cat)) %>% 
  .[,!(v_lev_cov == v_lev_train)] %>% 
  colnames())
# should be only 4 geomorphological covariates that we cannot use in QRF anyway
# because > 32 levels

# compare levels of covariates with unequal number of levels
# ls_cat_recl_train <- tbl_regmat_target %>%
#   filter(split %in% "train") %>%
#   dplyr::select(all_of(v_cat_recl)) %>%
#   map(., ~levels(.x))

# ls_cat_recl_cov <- foreach(recl = 1:length(v_cat_recl)) %do%
#   levels(r_stack_cov[[v_cat_recl]][[recl]])[[1]]

# QRF quantregForest() allows categorical predictors with max. 32 classes
n32 <- map(tbl_regmat_target, ~levels(.)) %>% 
  map(., ~length(.) > 32) %>% 
  unlist() %>% 
  which()

# remove predictors with > 32 classes
tbl_regmat_target <- tbl_regmat_target %>% 
  dplyr::select(-c(names(n32)))

# save dataset used for modelling to disk
write_rds(tbl_regmat_target, paste0("out/data/model/tbl_regmat_", TARGET, ".Rds"))



### Train RF model -------------------------------------------------------------

# RF hyperparameters (in e.g. "randomForest", "quantregForest" & "ranger" pkgs):
# 1)  Number of trees in the forest ("ntree" or "num.trees"):
#     although empirical evidence suggests that performance of prediction
#     remains good even when using only e.g. 100 trees, it should not be set too
#     small to ensure that every input gets predicted at least a few times.
# 2)  Number of predictors/covariates/features (P = total number of predictors)
#     to consider at any given split ("mtry"):
#     "quantregForest" & "randomForest" pkg default is 1/3 of P following
#     Breiman 2001 recommendation; "ranger" pkg default: rounded down square
#     root of P; for the purpose of tuning mtry parameter, Kuhn & Johnson 2013
#     recommend starting with 5 values of mtry that are evenly spaced between 2
#     and P.
# 3)  Complexity of each tree (e.g. "nodesize" = "min.node.size" and others):
#     - nodesize: fixes minimal number of instances in each terminal node,
#       determining how many observations at least lie in the same node;
#       default in "quantregForest" pkg = 10; default in "ranger" pkg 
#       "min.node.size" for regression = 5
#     - ranger::max.depth: Maximal tree depth. A value of NULL or 0 (default)
#       corresponds to unlimited depth, 1 to tree stumps (1 split per tree).
# 4)  Sampling scheme:
#     Default is bootstrapping where 100% of observations are sampled with
#     replacement; here, we can adjust both the sample size and whether to sample
#     with or without replacement. Decreasing the sample size leads to more diverse
#     trees and thereby lower between-tree correlation. If there are e.g. a few
#     dominating features in data set, reducing sample size can help minimize
#     between tree correlation. Also, when you have categorical predictors with
#     varying number of levels, sampling with replacement can lead to biased
#     variable split selection. Consequently, if you have categories that are not
#     balanced, sampling without replacement provides a less biased use of all
#     levels across the trees in the RF
#     - ranger::case.weights: Weights for sampling of training observations.
#     Observations with larger weights will be selected with higher probability
#     in the bootstrap samples for the trees.
# 5)  Splitting rule to use during tree construction ("splitrule"):
#     In "ranger", default splitrule is "variance"


# Model tuning: cross-validation & tuning using hyperparameters ----------------

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

# extensive tuning grid of hyperparameters
tbl_tuning_extensive <- expand.grid(num.trees = c(100, 150, 200, 250, 500, 750, 1000),
                                    mtry = round(c(sqrt(P), P * c(.25, .333, .4))),
                                    min.node.size = c(1, 3, 5),
                                    replace = c(TRUE, FALSE),
                                    sample.fraction = c(.5, .63, .8),
                                    splitrule = "variance", # default
                                    # cols to be filled in after training:
                                    RMSE = NA,
                                    Rsquared = NA,
                                    folds_RMSE = NA,
                                    folds_Rsquared = NA,
                                    folds_MAE = NA,
                                    folds = NA) %>%
  as_tibble() %>%
  # sample.fraction can only be tuned if replace = FALSE; change sample fraction
  # to 1 if replacement = TRUE and remove resulting duplicated rows
  mutate(sample.fraction = case_when(replace %in% TRUE ~ 1.0,
                                     replace %in% FALSE ~ sample.fraction)) %>%
  distinct()

# Control randomness of sampling in each fold in multi-core environment
set.seed(2021)
# 10 folds
seed_folds <- vector(mode = "list", length = length(indices$index))
for (i in 1:length(indices$index)) {
  seed_folds[[i]] <- sample.int(1000, nrow(tbl_tuning_extensive))
}
# For the final model:
seed_final <- sample.int(1000, 1)


# fitting/training/calibrating models ------------------------------------------
# full cartesian grid search by fitting model for ith hyperparameter combination
system.time(
  for(i in seq_len(nrow(tbl_tuning_extensive))) {
    
    # set nuances of model training:
    # By default, train chooses tuning parameter with best performance (e.g. if
    # chosen metric is RMSE, then it chooses model with lowest RMSE)
    # "one standard error" rule of Breiman et al. (1984) suggests that tuning
    # parameter associated with the best performance may overfit and so its
    # better to choose simplest model within one standard error (SE) of the
    # empirically optimal model; this is useful for simple trees that easily
    # overfit but little or no effect for RF...
    # "selectionFunction" only makes a difference when comparing models with
    # different hyperparameters within the same "train" object (unlike here...)
    train_control <- trainControl(method = "cv",
                                  # don't save predictions to save computation time
                                  savePredictions = FALSE,
                                  selectionFunction = "best", # or "oneSE"
                                  index = indices$index,
                                  # seeds for each set of hyperparameters
                                  seeds = c(map(seed_folds, ~ .x[i]),
                                            seed_final),
                                  allowParallel = THREADS)
    # indexFinal = optional vector of integers indicating which samples used to
    # fit the final model after resampling. If NULL, then entire dataset is used
    
    # set seed to control randomness of model training/fitting
    set.seed(2021)
    # There are rare cases where the underlying model function does not control
    # the random number seed, especially if the computations are conducted in C
    # code! Could this be the case for RF using ranger (written in C)?
    
    # train model using caret::train() & "ranger" implementation
    rf_fit <- train(x = df_cov_train,
                    y = v_response,
                    method = "ranger",
                    metric = "RMSE", # default for regression problems
                    num.trees = tbl_tuning_extensive$num.trees[i],
                    replace = tbl_tuning_extensive$replace[i],
                    # Suffices to run standard RF instead of QRF for testing
                    # hyperparameters since we tune for mean predictions
                    # (I tested this and results are identical)
                    quantreg = FALSE, 
                    oob.error = FALSE, # to save computation time
                    num.threads = THREADS,
                    # doesn't seem to work (no status bar)
                    verbose = TRUE,
                    # seed = 2021,
                    tuneGrid = data.frame(
                      mtry = tbl_tuning_extensive$mtry[i],
                      splitrule = tbl_tuning_extensive$splitrule[i],
                      min.node.size = tbl_tuning_extensive$min.node.size[i]),
                    trControl = train_control)
    
    # export average RMSE & Rsquared over all folds & table of metrics per fold
    tbl_tuning_extensive$RMSE[i] <- rf_fit$results$RMSE
    tbl_tuning_extensive$Rsquared[i] <- rf_fit$results$Rsquared
    tbl_tuning_extensive$folds_RMSE[i] <- list(rf_fit$resample$RMSE)
    tbl_tuning_extensive$folds_Rsquared[i] <- list(rf_fit$resample$Rsquared)
    tbl_tuning_extensive$folds_MAE[i] <- list(rf_fit$resample$MAE)
    tbl_tuning_extensive$folds[i] <- list(rf_fit$resample$Resample)
  }
)
# time elapse QRF 160 combinations (48 cores): 4.2 hours
# time elapse QRF 192 combinations (48 cores): 5 hours
# time elapse QRF 336 combinations (48 cores): 7 hours
# time elapse RF 336 combinations (48 cores): 5 h 50 min

# Sort tibble of tuning results by best performance (lowest RMSE)
# However, if increase in RMSE is < 0.1%, then prioritize model with less trees
tbl_tuning_extensive <- tbl_tuning_extensive %>% 
  mutate(index = RMSE < (min(RMSE)+(min(RMSE)*0.001))) %>% 
  group_by(index) %>% 
  arrange(-index, num.trees, RMSE)

# save table of hypertuning results to disk
write_rds(tbl_tuning_extensive,
          paste0("out/data/model/tbl_RF_hypertuning_", TARGET, ".Rds"))

# Flat files can't store the list columns
write_csv(tbl_tuning_extensive %>% 
            dplyr::select(num.trees:Rsquared),
          paste0("out/data/model/tbl_RF_hypertuning_", TARGET, ".csv"))

# in case of varying hyperparameters, we can plot (using plot or ggplot)
# plot(rf_fit, metric = "RMSE")
# ggplot(rf_fit, metric = "Rsquared")
# "Randomly selected predictors" = mtry


