#------------------------------------------------------------------------------
# Name:     21_cov_dem_deriv_saga.R
#           (cov = covariate, dem = digtial elevation model, deriv = derivatives,
#           saga = SAGA GIS)
#
# Content:  - code from Hengl and MacMillan 2019 Predictive Soil Mapping in R
#           
# Inputs:   - DEM (AHN2) as covariate (or entire covariate stack):
#             out/data/covariates/01_r_stack_cov.grd
#
# Temporary storage:  out/data/covariates/DEM_derivatives/ (AHN2 input & SAGA GIS outputs)
#
# Output:   - covariate raster stack including DEM derivatives:
#             out/data/covariates/02_r_stack_cov.grd
#
# Runtime:  - approx. 1 hr 15 min (entire script)
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  November 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

pkgs <- c("tidyverse", "sf", "rgdal", "raster")
lapply(pkgs, library, character.only = TRUE)



### Ensure R and SAGA GIS connection -------------------------------------------

# connect R and SAGA GIS
# code from Hengl and MacMillan 2019 Predictive Soil Mapping in R
if(Sys.info()['sysname']=="Windows"){
  saga_cmd = "C:/Progra~1/SAGA-GIS/saga_cmd.exe"
} else {
  saga_cmd = "saga_cmd"
}

# if installation of SAGA GIS was successful, can access from command line
system(paste(saga_cmd, "-v"))
#> Warning in system(paste(saga_cmd, "-v")): error in running command



### Input data: DEM (AHN2) --------------------------------------------------

# read from raster stack
r_stack_cov <- stackOpen("out/data/covariates/01_r_stack_cov.grd")
# would be faster to just read in DEM as single raster GeoTIFF....

sgdf_ahn2_25m <- r_stack_cov$ahn2_25m %>% 
  as(., "SpatialGridDataFrame")

# Maybe change to CRS that SAGA GIS can understand:
# http://www.saga-gis.org/saga_tool_doc/2.2.4/pj_proj4_0.html

# write as SAGA data using GDAL
writeGDAL(sgdf_ahn2_25m, "out/data/covariates/DEM_derivatives/ahn2_25m.sdat", "SAGA")



### Function to automate DEM parameters of interest -------------------------

# define function
# code from Hengl and MacMillan 2019 Predictive Soil Mapping in R
saga_DEM_derivatives <- function(INPUT, MASK=NULL,
                                 sel=c("SLP","TWI","CRV","VBF","VDP","OPN","DVM")){
  if(!is.null(MASK)){
    # Fill in missing DEM pixels:
    suppressWarnings( system(paste0(saga_cmd,
                                    ' grid_tools 25 -GRID=\"', INPUT,
                                    '\" -MASK=\"', MASK, '\" -CLOSED=\"',
                                    INPUT, '\"')) )
  }
  # Slope:
  if(any(sel %in% "SLP")){
    try( suppressWarnings( system(paste0(saga_cmd,
                                         ' ta_morphometry 0 -ELEVATION=\"',
                                         INPUT, '\" -SLOPE=\"',
                                         gsub(".sgrd", "_slope.sgrd", INPUT),
                                         '\" -C_PROF=\"',
                                         gsub(".sgrd", "_cprof.sgrd", INPUT), '\"') ) ) )
  }
  # Topographic Wetness Index (TWI):
  if(any(sel %in% "TWI")){
    try( suppressWarnings( system(paste0(saga_cmd,
                                         ' ta_hydrology 15 -DEM=\"',
                                         INPUT, '\" -TWI=\"',
                                         gsub(".sgrd", "_twi.sgrd", INPUT), '\"') ) ) )
  }
  # Multiresolution Valley Bottom Flatness (MrVBF):
  if(any(sel %in% "VBF")){
    try( suppressWarnings( system(paste0(saga_cmd,
                                         ' ta_morphometry 8 -DEM=\"',
                                         INPUT, '\" -MRVBF=\"',
                                         gsub(".sgrd", "_vbf.sgrd", INPUT),
                                         '\" -T_SLOPE=10 -P_SLOPE=3') ) ) )
  }
  ## Valley depth:
  if(any(sel %in% "VDP")){
    try( suppressWarnings( system(paste0(saga_cmd,
                                         ' ta_channels 7 -ELEVATION=\"',
                                         INPUT, '\" -VALLEY_DEPTH=\"',
                                         gsub(".sgrd", "_vdepth.sgrd",
                                              INPUT), '\"') ) ) )
  }
  ## Openness (positive (openp) and negative (openn)):
  if(any(sel %in% "OPN")){
    try( suppressWarnings( system(paste0(saga_cmd,
                                         ' ta_lighting 5 -DEM=\"',
                                         INPUT, '\" -POS=\"',
                                         gsub(".sgrd", "_openp.sgrd", INPUT),
                                         '\" -NEG=\"',
                                         gsub(".sgrd", "_openn.sgrd", INPUT),
                                         '\" -METHOD=0' ) ) ) )
  }
  ## Deviation from Mean Value:
  if(any(sel %in% "DVM")){
    suppressWarnings( system(paste0(saga_cmd,
                                    ' statistics_grid 1 -GRID=\"',
                                    INPUT, '\" -DEVMEAN=\"',
                                    gsub(".sgrd", "_devmean.sgrd", INPUT),
                                    '\" -RADIUS=11' ) ) )
  }
}

# run function on DEM (processes all DEM derivatives at once)
# Note that SAGA GIS will by default optimize computing of DEM derivatives by using
# most of the available cores to compute (parallelization is turned on automatically).
system.time(
  saga_DEM_derivatives("out/data/covariates/DEM_derivatives/ahn2_25m.sgrd")
)
# Errors shown:
# Error: >> WKT: unknown projection [Double_Stereographic]
# time elapsed running SAGA on system in parallel: 8128 sec (approx. 2.5 h)

# plot DEM derivatives
library(viridis) # for better plotting visuals

dem.lst <- list.files("out/data/covariates/DEM_derivatives",
                      pattern=glob2rx("^ahn2_25m_*.sdat"),
                      full.names = TRUE)

r_stack_DEMderiv <- raster::stack(dem.lst)

# plot and save plot to disk
pdf(paste0("out/maps/explorative/m_DEMderiv_", Sys.Date(), ".pdf"),
    width = 7, height = 7)
p_DEMderiv <- plot(r_stack_DEMderiv, col = rev(magma(10, alpha = 0.8)))
dev.off()

# visually explore DEM derivatives to see if calculations make sense
# library(mapview)

#system.time(
#  mapview(r_stack_cov$ahn2_25m_cprof,
#          maxpixels = ncell(r_stack_cov$ahn2_25m_cprof)) # full resolution (takes longer)
#)

# add DEM derivatives to covariate stack and save
# designate correct projection to DEM derivatives (SAGA GIS changed it?)
crs(r_stack_DEMderiv) <- crs(r_stack_cov)

r_stack_cov_DEMderiv <- stack(r_stack_cov, r_stack_DEMderiv)

# save raster stack using saveStack (only saves references to files written on disk)
# save entire stack including DEM derivatives
stackSave(r_stack_cov_DEMderiv, "out/data/covariates/02_r_stack_cov.grd")

# save stack of only DEM derivatives (so that this script does not have to be run
# unless we update DEM derivatives)
stackSave(r_stack_DEMderiv, "out/data/covariates/r_stack_DEMderiv.grd")


