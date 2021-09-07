#------------------------------------------------------------------------------
# Name:     make_recl_tbl.R
#           (recl = reclassify, tbl = table)
#
# Content:  - Make reclassifying metadata table for each categorical covariate
#             based on ID values in raster S4 attribute table
#           
# Inputs:   - covariate stack including DEM derivatives (21_cov_dem_deriv_saga.R):
#             out/data/covariates/02_r_stack_cov.grd
#
# Output:   - csv tables with values of categorical covariates with 4 columns
#             (value, description, reclassified value, reclassified description)
#             that can be manually designated to reclassification classes in a
#             next step
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  September 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "rgdal", "rasterVis", "foreach", "doParallel")
# terra package
lapply(pkgs, library, character.only = TRUE)



### Designate categorical covariates as such -----------------------------------

# read from raster stack
r_stack_cov <- stackOpen("out/data/covariates/02_r_stack_cov.grd")
# using terra package:
# rast("out/data/covariates/02_r_stack_cov.grd")
# ERROR 4: `' not recognized as a supported file format.

# Remove phrase "resampled" from raster stack covariate names
names(r_stack_cov) <- names(r_stack_cov) %>% 
  stringr::str_replace(., "_resampled", "")

# read in covariate metadata
tbl_cov_meta <- read_csv("data/covariates/covariates_metadata.csv") %>% 
  # only interested in covariates we use in model
  filter(name %in% names(r_stack_cov))

# make sure all covariates up to date have metadata info
names(r_stack_cov)[!names(r_stack_cov) %in% tbl_cov_meta$name] # should be 0

# make sure there are only continuous or categorical "values_type" (binary)
tbl_cov_meta$values_type %>% 
  unique()

# get names of all categorical covariates
v_cov_cat_names <- tbl_cov_meta %>% 
  filter(values_type %in% "categorical") %>% 
  dplyr::select(name) %>% 
  as.list() %>% 
  unlist(., use.names = FALSE)

# retrieve CLORPT factor that each categorical covariate belongs to
v_cov_cat_clorpt <- tbl_cov_meta %>% 
  filter(values_type %in% "categorical") %>% 
  dplyr::select(category) %>% 
  as.list() %>% 
  unlist(., use.names = FALSE)

# set up parallel backend to use multiple cores
cores <- detectCores()
cl <- makeCluster(cores - 20) # to not overload memory
registerDoParallel(cl)

# designate categorical covariates as such (parallel)
system.time(
  ls_cov_cat <- foreach(cat = 1:length(v_cov_cat_names)) %dopar% {
    raster::ratify(r_stack_cov[[v_cov_cat_names[cat]]])
  }
)
# time elapse: 4 min

# make reclassifying metadata table for each categorical covariate
for (cat in 90:length(v_cov_cat_names)) {
  # define length of number of classes in each categorical covariate
  n_classes <- length(ls_cov_cat[[cat]]@data@attributes[[1]]$ID)
  # create reclassifying metadata table template
  tbl_reclassify <- tibble(value = ls_cov_cat[[cat]]@data@attributes[[1]]$ID,
                           description = character(n_classes),
                           value_rcl = numeric(n_classes),
                           description_rcl = character(n_classes))
  # save each template .csv on disk for each categorical covariate
  # add the date to avoid overwriting old already filled in csv tables
  write_csv(tbl_reclassify,
            paste0("data/covariates/", v_cov_cat_clorpt[[cat]],
                   "/", v_cov_cat_names[[cat]], "_reclassify_", Sys.Date(), ".csv"))
}

# stop parallel backend
stopCluster(cl)


