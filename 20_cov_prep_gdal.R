#------------------------------------------------------------------------------
# Name:     20_cov_prep_gdal.R
#           (cov = covariate, prep = preparation, gdal = GDAL)
#
# Content:  - assemble & prepare predictors (covariates as raster data):
#               - designate coordinate system (projection)
#               - resample covariates so they have the same origin, cell locations and extent
#               - mask nodata areas (water and areas outside NL) of continuous covariates
#                 (categorical covariates masked after reclassification in later script)
#               - assemble into raster stack and save
#           
# Inputs:   - covariate rasters (GeoTIFF (.tif)): data/covariates/
#
# Temporary storage:  out/data/covariates/resample/  (GDAL in- and outputs)
#                     out/data/covariates/easting_northing/ (new rasters)
#                     out/data/covariates/mask_continuous/
#
# Output:   - covariate stack: out/data/covariates/01_r_stack_cov.grd
#
# Runtime:  - ca. 20-30 min (entire script)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  November 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "here", "sf", "rgdal", "raster", "foreach", "doParallel")

lapply(pkgs, library, character.only = TRUE)



### assemble spatial covariates as raster data ----------------------------------

# Data root
data_root <- here::here("data/covariates")

# Read from all covariate folders (only tif related files, e.g. no text metadata files)
# for now also only covariates at 25m resolution
# \\ = escape special character (.); $ = end of string
dirs_rec <- dirname(dir(here::here("data/covariates"),
                        pattern = "25m\\.tif$", recursive = TRUE))

dirs_nonrec <- dirname(dir(here::here("data/covariates")))

# List folders with covariate data
(folder_names <- rlang::set_names(
  unique(dirs_rec[!dirs_rec %in% dirs_nonrec])
))

# Paths where files are located
(folder_paths <- purrr::map(
  .x = folder_names,
  .f = ~ paste(data_root, .x, sep = "/")
))

ls_files <- map(folder_paths, ~ dir(path = .x, pattern = "25m\\.tif$", full.names = TRUE))

# concatenate to vector to remove directory information
files <- c(ls_files$climate, ls_files$geology, ls_files$organism,
           ls_files$relief, ls_files$soil)

# turn covariate tifs to raster data
# let's read in 2 rasters as an example:
r_bodem2014_soiltype_25m <- files[[grep("bodem2014_soiltype_25m", files)]] %>% 
  raster(.) %>% 
  as.factor(.)
r_ahn2_25m <- files[[grep("ahn2_25m", files)]] %>% 
  raster(.)

# designate proper coordinate system (CRS)
crs(r_bodem2014_soiltype_25m) <- CRS('+init=epsg:28992')
crs(r_ahn2_25m) <- CRS('+init=epsg:28992')

p_ahn2 <- plot(r_ahn2_25m,
               main = "AHN2 [25m resolution]",
               xlab = "Longitude [epsg: 28992 (Amersfoort / RD New Netherlands)]",
               ylab = "Latitude [epsg: 28992 (Amersfoort / RD New Netherlands)]")

levels_soiltype <- levels(r_bodem2014_soiltype_25m)[[1]] %>% 
  c() %>% 
  .$ID %>% 
  length()

p_bodem2014_soiltype <- plot(r_bodem2014_soiltype_25m,
                             main = "Soil Type [25m resolution]",
                             col = bpy.colors(levels_soiltype),
                             xlab = "Longitude [epsg: 28992 (Amersfoort / RD New Netherlands)]",
                             ylab = "Latitude [epsg: 28992 (Amersfoort / RD New Netherlands)]")

# for now remove soil maps as predictors
cov_soil <- grepl("/soil/", files)
files <- files[!cov_soil]

# create raster stack
# r_stack <- stack(files)
# cannot do this because tifs don't all have the same extent (?)

# read all covariates seperately without making a stack
system.time(
  ls_r_cov <- map(files, ~raster(.x)) %>% 
    set_names(., paste0(map(., ~names(.x)))) %>%  # rename designated name
    unlist()
)

# Total number of covariates and their names
length(ls_r_cov)
summary(ls_r_cov)

# clean up: remove temporary variables and data
rm(pkgs, data_root, dirs_nonrec, dirs_rec,
   files, ls_files, folder_names, folder_paths, cov_soil)



### Check coordinate system, origin and extent to determine which covariates need
### to be resampled-------------------------------------------------------------

## check coordinate reference system (crs)
crs_all_cov <- map(ls_r_cov, ~crs(.x)) %>% 
  unlist() %>% 
  as.character()

unique(crs_all_cov)
# cordinate systems appear to be unequal

# designate correct CRS to all covariates
for (rr in 1:length(ls_r_cov)) {
  # browser()
  proj4string(ls_r_cov[[rr]]) <- CRS('+init=epsg:28992')
}
# all rasters now have the same crs


## check origin
ls_origin_all_cov <- map(ls_r_cov, ~origin(.x)) %>% 
   unique()
# not all covariates have the same origin

# logical of rasters with origin = 0,0
is_origin0 <- map(ls_r_cov, ~origin(.x)) %>% 
  map(., ~sum(.x)) %>% 
  map(., ~grepl("^0$", .x)) %>% 
  unlist(use.names = FALSE) # naming uses up more memory

# list of covariates that need to be resampled due to different origin (n = 123)
ls_r_cov_origin_dif <- ls_r_cov[!is_origin0]

# list of covariates with correct origin
ls_r_cov_origin0 <- ls_r_cov[is_origin0]


## check extent
extent_cov_origin0 <- map(ls_r_cov_origin0, ~extent(.x))
unique(extent_cov_origin0)
# different extents

# only a few covariates (n = 3) have same origin and extent (required to make stack)
is_extent_ahn2 <- map(extent_cov_origin0,
                      ~isTRUE(.x == extent(ls_r_cov_origin0$ahn2_25m))) %>%
                      unlist(use.names = FALSE)
ls_r_cov_no_res <- ls_r_cov_origin0[is_extent_ahn2]

# thus, all others need to be resampled to get the same origin and extent
# (resolution is identical for all)
ls_r_cov_res <- c(ls_r_cov_origin0[!is_extent_ahn2],
                  ls_r_cov_origin_dif)

# set up parallel backend to use multiple cores
cores <- detectCores()
cl <- makeCluster(cores - 2) # to not overload memory
registerDoParallel(cl)

# save to disk for resampling in GDAL without using precious Rsession memory space
system.time(
  foreach(rr = 1:length(ls_r_cov_res)) %dopar%
    raster::writeRaster(ls_r_cov_res[[rr]],
                paste0("out/data/covariates/resample/",
                       names(ls_r_cov_res[rr]), ".tif"),
                overwrite = TRUE)
)
# time elapsed: 3 min
# (time elapsed sequential: 1 h 15 min)

# stop parallel backend
stopCluster(cl)



### Resample covariates using GDAL ------------------------------------------

# Resample using GDAL and run it externally on system
# the following code is from Hengl & MacMillan's "Predictive Soil Mapping with R"
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
#> Warning in system(paste(gdalwarp, "--help")): error in running command

# Use resampling method specific to whether covariate is categorical vs. continuous
# Categorical covariate: nearest neighbor ("near" in gdalwarp fnc)
# Continuous covariate: cubic spline ("cubicspline" in gdalwarp fnc)

# In order to make it specific to categorical/continuous read in metadata:
tbl_cov_meta <- read_csv("data/covariates/covariates_metadata.csv") %>% 
  # only metadata info of covariates we need to resample
  filter(name %in% names(ls_r_cov_res))

# make sure there are only continuous or categorical "values_type" (binary)
tbl_cov_meta$values_type %>% 
  unique()

# match row order of metadata to list of covariates
tbl_cov_meta <- tbl_cov_meta[match(names(ls_r_cov_res), tbl_cov_meta$name),]

# force format/datatype specific for each covariate to save disk space using "ot" argument
# For R raster pkg datatypes see ?dataType
# For gdal datatypes see https://grass.osgeo.org/grass78/manuals/r.out.gdal.html
# add column with metadata info cat/cont to resample specific to this
tbl_data_value_type_cov <- map(ls_r_cov_res, ~dataType(.x)) %>% 
  unlist() %>% 
  as_tibble() %>% 
  add_column(cov = names(ls_r_cov_res), .before = "value") %>% 
  add_column(value_type = tbl_cov_meta$values_type, .before = "value") %>% 
  mutate(resampling = case_when(value_type %in% "categorical" ~ "near",
                                value_type %in% "continuous" ~ "cubicspline"),
         .before = "value") %>% 
  rename(R_datatype = value) %>% 
  mutate(GDAL_datatype = case_when(R_datatype %in% "INT4S" ~ "Int32",
                                   R_datatype %in% "INT1U" ~ "Byte",
                                   R_datatype %in% "INT2U" ~ "UInt16",
                                   R_datatype %in% "FLT4S" ~ "Float32"))

# set up parallel backend to use multiple cores
cores <- detectCores()
cl <- makeCluster(cores - 10) # use less cores than previously (more intensive)
registerDoParallel(cl)

# in parallel
system.time(
  foreach(sgdf = 1:length(ls_r_cov_res)) %dopar%
    system(paste0('gdalwarp out/data/covariates/resample/',
                  tbl_data_value_type_cov$cov[sgdf], '.tif',
                  ' out/data/covariates/resample/',
                  tbl_data_value_type_cov$cov[sgdf], '_resampled.tif -ot ',
                  tbl_data_value_type_cov$GDAL_datatype[sgdf],
                  ' -r \"', tbl_data_value_type_cov$resampling[sgdf], '\" -te ',
                  paste(as.vector(raster::extent(ls_r_cov_no_res$ahn2_25m))[c(1,3,2,4)], collapse=" "),
                  ' -tr ', raster::res(ls_r_cov_no_res$ahn2_25m)[1], ' ',
                  raster::res(ls_r_cov_no_res$ahn2_25m)[2],
                  ' -overwrite'))
)
# time elapsed: ca 4.5 min
# (time elapsed sequential: 18 min)

# stop parallel backend
stopCluster(cl)



### Create raster stack of covariates, add Northing and Easting ----------------

# Data root
data_root <- here::here("out/data/covariates/resample")

# resampled and masked covariate file names
ls_files <- list.files(path = data_root, pattern = "25m_resampled\\.tif$")

# resampled covariate file names including path
ls_file_paths <- map(ls_files, ~paste(data_root, .x, sep = "/"))

# create raster stack of resampled covariates
r_stack_cov <- stack(ls_file_paths)

# although all CRS are the same, the string needs to be identical
crs(r_stack_cov) <- CRS('+init=epsg:28992')

# create Northing and Easting matrices
tbl_East_North <- coordinates(r_stack_cov) %>% 
  as_tibble() %>% 
  mutate(Easting = x, Northing = y)

# Turn into rasters as additional geographical covariates
r_Easting <- rasterFromXYZ(xyz = tbl_East_North %>% 
                             dplyr::select(x, y, Easting),
                           res = res(r_stack_cov),
                           crs = crs(r_stack_cov))

r_Northing <- rasterFromXYZ(xyz = tbl_East_North %>% 
                              dplyr::select(x, y, Northing),
                            res = res(r_stack_cov),
                            crs = crs(r_stack_cov))

# newly created raster layers need to be saved to disk
writeRaster(r_Easting, "out/data/covariates/easting_northing/easting_25m.tif",
            overwrite = TRUE)
writeRaster(r_Northing, "out/data/covariates/easting_northing/northing_25m.tif",
            overwrite = TRUE)

# read in saved newly created rasters to designate their disk location
# in Rsession they are not read into working memory
r_Easting <- raster("out/data/covariates/easting_northing/easting_25m.tif")
r_Northing <- raster("out/data/covariates/easting_northing/northing_25m.tif")

# although all CRS are the same, the string needs to be identical
crs(r_Easting) <- crs('+init=epsg:28992')
crs(r_Northing) <- crs('+init=epsg:28992')

# combine all covariates: resampled, non-resampled and Easting/Northing
r_stack_cov <- stack(c(r_stack_cov, ls_r_cov_no_res, r_Easting, r_Northing))

is_Easting <- grep("layer.2", names(r_stack_cov))
is_Northing <- grep("layer.3", names(r_stack_cov))

# rename Easting and Northing
names(r_stack_cov[[is_Easting]]) <- "easting_25m"
names(r_stack_cov[[is_Northing]]) <- "northing_25m"

# remove "resampled" in raster names
names(r_stack_cov) <- names(r_stack_cov) %>% 
  stringr::str_replace(., "_resampled", "")



### Mask continuous covariates -------------------------------------------------

# In order to make it specific to continuous read in metadata:
tbl_cov_meta <- read_csv("data/covariates/covariates_metadata.csv") %>% 
  # only metadata info of covariates we need to resample
  filter(name %in% names(r_stack_cov))

# retrieve names of continuous covariates
v_cont <- tbl_cov_meta %>% 
  filter(values_type %in% "continuous") %>% 
  .$name

# check out values of mask (values need to be reversed)
r_mask_rev <- raster("data/other/ahn2_nodata_25m.tif")

unique(values(r_mask_rev)) # change so that 1 -> 0 and NA -> 1

# create raster with value = 0
r_val0 <- setValues(r_mask_rev, 0)

# create raster mask: 0 = water and outside NL, 1 = target mapping pixels
r_mask <- mask(x = r_val0,
               mask = r_mask_rev,
               maskvalue = NA,
               updatevalue = 1)

# save mask to disk
writeRaster(r_mask, "out/data/covariates/mask_continuous/ahn2_nodata_mask.tif",
            overwrite = TRUE)

# read in saved newly created raster mask to designate their disk location
# in Rsession they are not read into working memory
r_mask <- raster("out/data/covariates/mask_continuous/ahn2_nodata_mask.tif")

# mask continuous covariates (sequential)
system.time(
  ls_r_cov_cont_masked <- foreach(cont = 1:length(v_cont)) %do% {
   raster::mask(x = r_stack_cov[[v_cont[cont]]],
                mask = r_mask,
                maskvalue = 0) 
  }
) # time elapse: 3.7 min (sequential)

# save masked rasters (in Rsession memory) to disk
system.time(
  foreach(cont = 1:length(ls_r_cov_cont_masked)) %do%
    raster::writeRaster(ls_r_cov_cont_masked[[cont]],
                        paste0("out/data/covariates/mask_continuous/",
                               v_cont[cont], ".tif"),
                        overwrite = TRUE)
) # time elapsed: 4.5 min

# approach above does not work for covariate "water wetness probability 2015"
# use GDAL instead
system(paste0('gdal_calc.py -A out/data/covariates/mask_continuous/',
              'water_wetness_probability_2015_25m.tif',
              ' -B out/data/covariates/mask_continuous/ahn2_nodata_mask.tif',
              ' --outfile=out/data/covariates/mask_continuous/',
              'water_wetness_probability_2015_25m_masked.tif',
              ' --calc="A*B" --overwrite'))

# read in saved rasters
ls_r_cov_cont_masked <- foreach(cont = 1:length(v_cont)) %do%
  raster::raster(paste0("out/data/covariates/mask_continuous/",
                        v_cont[cont], ".tif"))

# read in water wetness covariate masked using GDAL as well
water_wetness_probability_2015_25m <- raster("out/data/covariates/mask_continuous/water_wetness_probability_2015_25m_masked.tif")

# replace continuous rasters with new masked ones
r_stack_cov <- dropLayer(r_stack_cov, v_cont)
r_stack_cov <- stack(r_stack_cov, stack(ls_r_cov_cont_masked))
r_stack_cov <- dropLayer(r_stack_cov, "water_wetness_probability_2015_25m")
r_stack_cov <- stack(r_stack_cov, water_wetness_probability_2015_25m)

# remove "masked" in raster names (only in water wetness)
names(r_stack_cov) <- names(r_stack_cov) %>% 
  stringr::str_replace(., "_masked", "")



### plot some covariates and save them -----------------------------------------

# get an idea of 2D extent, as well as number of layers (rasters)
dim(r_stack_cov)

# plot a few rasters as a quick quality check
plot(r_stack_cov[[25]], main = names(r_stack_cov[[25]]))
spplot(r_stack_cov[[90]], main = names(r_stack_cov[[90]]))

# save raster stack using saveStack (only saves references to files written on disk)
stackSave(r_stack_cov, "out/data/covariates/01_r_stack_cov.grd")


