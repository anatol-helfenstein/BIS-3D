#------------------------------------------------------------------------------
# Name:     22_cov_cat_recl_gdal_par.R
#           (cov = covariate, cat = categorical, recl = reclassify, gdal = GDAL,
#           par = parallel)
#
# Content:  - define categorical variables in covariate stack as such
#           - reclassify: combine levels of each categorical covariate into new
#             levels that are more broad
#           - designate NA values and mask categorical covariates
#           - write all covariates to disk (final stack for modelling)
#           
# Inputs:   - covariate stack including DEM derivatives (21_cov_dem_deriv_saga.R):
#             out/data/covariates/02_r_stack_cov.grd
#
# Temporary storage:  out/data/covariates/reclassify/  (GDAL in- and outputs)
#                     out/data/covariates/mask_categorical/
#
# Output:   - rasters written to disk as .grd and .gri files (including attribute tables):
#             out/data/covariates/final_stack/
#
# Runtime:  - approx. 40-50 min (entire script)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  November 2020
#-------------------------------------------------------------------------------



### empty memory and workspace; load required packages -------------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "raster", "rgdal", "foreach", "doParallel")
# terra package
lapply(pkgs, library, character.only = TRUE)



### Specify covariates to be reclassified; read in reclassification tables -----

# read from raster stack and including DEM derivatives
r_stack_cov <- stackOpen("out/data/covariates/01_r_stack_cov.grd")
r_stack_DEMderiv <- stackOpen("out/data/covariates/r_stack_DEMderiv.grd")
r_stack_cov <- stack(r_stack_cov, r_stack_DEMderiv)

# Remove phrase "resampled" and "masked" from raster stack covariate names
names(r_stack_cov) <- names(r_stack_cov) %>% 
  stringr::str_replace(., "_resampled", "") %>% 
  stringr::str_replace(., "_masked", "")

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

# read in tables with reclassified values
ls_tbl_recl <- foreach(tbl = 1:length(v_cov_cat_names)) %do% {
  readr::read_csv(paste0("data/covariates/", v_cov_cat_clorpt[tbl], "/",
                         v_cov_cat_names[tbl], "_reclassify.csv"))
}

# arrange tables by increasing reclassified values
ls_tbl_recl <- map(ls_tbl_recl, ~arrange(.x, value_rcl))

# remove categorical covariates with only 1 class
v_1class <- map(ls_tbl_recl, ~(nrow(.x) > 1)) %>% 
  unlist()

# remove categorical covariates with only 1 class
v_cov_cat_names <- v_cov_cat_names[v_1class]
v_cov_cat_clorpt <- v_cov_cat_clorpt[v_1class]
ls_tbl_recl <- ls_tbl_recl[v_1class]

# stack with categorical covariates to reclassify
r_stack_cov_cat <- r_stack_cov[[v_cov_cat_names]]



### Reclassify categorical covariates using GDAL -------------------------------

# Reclassify using GDAL and run it externally on system
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
system(paste(gdal_calc.py, "--help"))
system(paste(gdal_translate, "--help"))

# Also tried unofficial gdal approach of a reclassify function (better syntax):
# https://github.com/chiatt/gdal_reclassify
# was unable to get it to work

# set up parallel backend to use multiple cores
cores <- detectCores()
cl <- makeCluster(cores - 2) # to not overload memory
registerDoParallel(cl)

# save to disk for reclassifying in GDAL without using precious Rsession memory space
system.time(
  foreach(cat = 1:length(v_cov_cat_names)) %dopar%
    raster::writeRaster(r_stack_cov_cat[[cat]],
                        paste0("out/data/covariates/reclassify/",
                               names(r_stack_cov_cat[[cat]]), ".tif"),
                        overwrite = TRUE)
)
# time elapsed: 5.5 min

# reclassify categorical covariates using GDAL in 2 steps:
# A) Reclassify using gdal_calc.py based on values in reclassify .csv tables;
#    in this step we set all NA values to 0 because fnc doesn't except multiple NA values
# B) Designate NA values (=0) as such using gdal_translate

# first check that there are no 0 values that contain information
v_0_values <- map(ls_tbl_recl, ~filter(.x, value_rcl %in% 0)) %>% 
  map(., ~(nrow(.x) > 0)) %>% 
  unlist()
v_cov_cat_names[v_0_values] # should be 0

# prepare strings for --calc argument in gdal_calc.py as function requires them
# 1) main part of string with all non-NA values except last row
ls_recl_gdal_main <- foreach(tbl = 1:length(v_cov_cat_names)) %do% {
  ls_tbl_recl[[tbl]] %>% 
  filter(!value_rcl %in% NA) %>% 
  slice(1:nrow(.)-1) %>% 
  mutate(gdal_rcl = paste0(value_rcl, '*(A==', value, ')+')) %>% 
    .$gdal_rcl %>% 
    str_c(., collapse = "")
}

# 2) last (or only row) without "+" at end of string
ls_recl_gdal_tail <- foreach(tbl = 1:length(v_cov_cat_names)) %do% {
  ls_tbl_recl[[tbl]] %>% 
    filter(!value_rcl %in% NA) %>% 
    tail(1) %>% 
    mutate(gdal_rcl = paste0(value_rcl, '*(A==', value, ')')) %>% 
    .$gdal_rcl %>% 
    str_c(., collapse = "")
}

# 3) NA values to reclassify to 0
ls_recl_gdal_na <- foreach(tbl = 1:length(v_cov_cat_names)) %do% {
  ls_tbl_recl[[tbl]] %>%
    filter(value_rcl %in% NA) %>% 
    mutate(gdal_rcl = paste0('+0*(A==', value, ')')) %>% 
    .$gdal_rcl %>% 
    str_c(., collapse = "")
}

# combine 1-3 into one long string for every categorical covariate
ls_recl_gdal <- foreach(tbl = 1:length(v_cov_cat_names)) %do% {
  str_c(ls_recl_gdal_main[[tbl]],
        ls_recl_gdal_tail[[tbl]],
        ls_recl_gdal_na[[tbl]],
        collapse = "")
}

# for still unknown reason get following error when calculating all rasters in parallel:
# Error in system(paste0("gdal_calc.py -A out/data/covariates/reclassify/",  : 
# task 1 failed - "this S4 class is not subsettable"
# seems to be known issue for processing rasters in foreach loop parallel (but unsolved)

# Therefore split calculations into n chunks
n = 4
v_cat <- 1:length(v_cov_cat_names)
ls_chunks <- split(v_cat, sort(v_cat %% n))

# parallel
system.time(
  foreach(n = 1:length(ls_chunks)) %do% {
    foreach(cat = ls_chunks[[n]]) %dopar%
      system(paste0('gdal_calc.py -A out/data/covariates/reclassify/',
                    names(r_stack_cov_cat[[cat]]), '.tif',
                    ' --outfile=out/data/covariates/reclassify/',
                    names(r_stack_cov_cat[[cat]]), '_reclassified.tif',
                    ' --calc="', ls_recl_gdal[[cat]], '"',
                    ' --overwrite'))
    }
)
# time elapse parallel: 13 min
# time elapse sequential: ca. 1.5 hours



### Mask covariates using GDAL -------------------------------------------------

# force format/datatype specific for each covariate to save disk space using "ot" argument
# For R raster pkg datatypes see ?dataType
# For gdal datatypes see https://grass.osgeo.org/grass78/manuals/r.out.gdal.html
tbl_datatype_cov <- map(as.list(r_stack_cov_cat), ~dataType(.x)) %>% 
  unlist() %>% 
  as_tibble() %>% 
  add_column(cov = names(r_stack_cov_cat), .before = "value") %>% 
  rename(R_datatype = value) %>% 
  # datatypes may have changed due to reclassification (uses less space on disk)
  add_column(R_datatype_rcl = map(ls_tbl_recl, ~filter(.x, !value_rcl %in% NA)) %>%
               map(., ~ifelse(min(.x$value_rcl) >= 0 & max(.x$value_rcl) <= 255,
                              "INT1U", "other")) %>% 
               unlist()) %>% 
  mutate(R_datatype_rcl = case_when(R_datatype_rcl %in% "INT1U" ~ "INT1U",
                                    R_datatype_rcl %in% "other" ~ R_datatype)) %>% 
  mutate(GDAL_datatype = case_when(R_datatype_rcl %in% "INT4S" ~ "Int32",
                                   R_datatype_rcl %in% "INT1U" ~ "Byte",
                                   R_datatype_rcl %in% "INT2U" ~ "UInt16",
                                   R_datatype_rcl %in% "FLT4S" ~ "Float32"))

# mask rasters in parallel by designating water and outside NL = 0
# use same mask as for continuous covariates
system.time(
  foreach(cat = 1:length(v_cov_cat_names)) %dopar%
    system(paste0('gdal_calc.py -A out/data/covariates/reclassify/',
                  names(r_stack_cov_cat[[cat]]), '_reclassified.tif',
                  ' -B out/data/covariates/mask_continuous/ahn2_nodata_mask.tif',
                  ' --outfile=out/data/covariates/mask_categorical/',
                  names(r_stack_cov_cat[[cat]]), '_masked.tif',
                  ' --calc="A*B" --type=',
                  tbl_datatype_cov$GDAL_datatype[cat], ' --overwrite'))
)
# time elapsed: ca 13.5 min

# Since we cannot list more than one nodatavalue (NA), we first classified all
# NA values as 0 (because this value is not occupied in any of the categorical covariates)
# in a second step designate 0 = NA using gdal_translate
system.time(
  foreach(cat = 1:length(v_cov_cat_names)) %dopar% {
    system(paste0('gdal_translate -ot ', tbl_datatype_cov$GDAL_datatype[cat],
                  ' -a_nodata 0 out/data/covariates/mask_categorical/',
                  names(r_stack_cov_cat[[cat]]), '_masked.tif',
                  ' out/data/covariates/reclassify/',
                  names(r_stack_cov_cat[[cat]]), '_reclassified_na.tif'))
  }
)
# time elapse: 5.5 min

# stop parallel backend
stopCluster(cl)

# remove temporary / intermediary GeoTIFF files on disk
foreach(cat = 1:length(v_cov_cat_names)) %do%
  unlink(paste0("out/data/covariates/reclassify/", v_cov_cat_names[[cat]],
                "_reclassified.tif"))



### Add descriptions to reclassified covariates --------------------------------

# read in reclassified rasters
ls_cat_cov_recl <- foreach(cat = 1:length(v_cov_cat_names)) %do%
  raster(paste0("out/data/covariates/reclassify/", v_cov_cat_names[[cat]],
                "_reclassified_na.tif"))

# set up parallel backend to use multiple cores
cores <- detectCores()
cl <- makeCluster(20) # to not overload memory
registerDoParallel(cl)

# designate categorical covariates as such (parallel)
system.time(
  ls_cat_cov_recl <- foreach(cat = 1:length(ls_cat_cov_recl)) %dopar% {
    raster::ratify(ls_cat_cov_recl[[cat]], count = TRUE)
  }
)
# time elapse: 9.5 min

# 255 values are NA: this is the raster default for datatype INT1U
v_255 <- map(ls_cat_cov_recl, ~levels(.x)[[1]]) %>%
  map(., ~as_tibble(.x)) %>% 
  map(., ~filter(.x, ID %in% 255)) %>% 
  map(., ~(nrow(.x) > 0)) %>% 
  unlist()
v_cov_cat_names[v_255]

# define levels of categorical covariates in several steps (see ?raster::factorValues):
# 1) retrieve levels
ls_tbl_rat <- map(ls_cat_cov_recl, ~levels(.x)[[1]]) %>% 
  map(., ~as_tibble(.x)) %>% 
  map(., ~filter(.x, !ID %in% 255)) # remove NA values

# 2) add description of classes
ls_tbl_rat <- foreach(i = 1:length(ls_tbl_rat)) %do% {
  add_column(ls_tbl_rat[[i]], description = ls_tbl_recl[[i]] %>% 
               filter(!description_rcl %in% NA) %>%
               .$description_rcl %>% 
               unique())
  }

# 3) designate categorical covariates with reclassified classes
foreach(i = 1:length(ls_cat_cov_recl)) %do% {
  levels(ls_cat_cov_recl[[i]])[[1]] <- as.data.frame(ls_tbl_rat[[i]])
}
# warnings()



### Make raster stack and save to disk --------------------------------------

# make a stack
r_stack_cov_cat <- stack(ls_cat_cov_recl)

# remove "reclassified" in raster names
names(r_stack_cov_cat) <- names(r_stack_cov_cat) %>% 
  stringr::str_replace(., "_reclassified_na", "")

# Saving as GeoTIFF does not save entire attribute table
# rasters's native .grd format saves attribute table including descriptions
system.time(
  foreach(cat = 1:length(ls_cat_cov_recl)) %dopar%
    raster::writeRaster(r_stack_cov_cat[[cat]],
                        paste0("out/data/covariates/final_stack/",
                               names(r_stack_cov_cat[[cat]]), ".grd"),
                        datatype = tbl_datatype_cov$R_datatype_rcl[cat],
                        overwrite = TRUE)
)
# time elapsed: 8 min (28 cores); 6 min (38 cores)

# specify continuous covariates
v_cov_cont_names <- tbl_cov_meta %>% 
  filter(values_type %in% "continuous") %>% 
  .$name

# stack of continuous covariates
r_stack_cov_cont <- r_stack_cov[[v_cov_cont_names]]

# save continuous covariates as well
system.time(
  foreach(cont = 1:length(v_cov_cont_names)) %dopar%
    raster::writeRaster(r_stack_cov_cont[[cont]],
                        paste0("out/data/covariates/final_stack/",
                               names(r_stack_cov_cont[[cont]]), ".grd"),
                        datatype = raster::dataType(r_stack_cov_cont)[cont],
                        overwrite = TRUE)
)
# time elapsed: 3.5 min

# stop parallel backend
stopCluster(cl)


