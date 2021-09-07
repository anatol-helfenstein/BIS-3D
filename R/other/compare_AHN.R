#

library(raster)

# read in different AHN's
# take resampled AHN1 because raw file doesn't have same extent
# r_ahn1_25m <- raster("data/covariates/relief/ahn_25m.tif")
r_ahn1_25m <- raster("out/data/covariates/final_stack/ahn_25m.gri")
r_ahn2_25m <- raster("data/covariates/relief/ahn2_25m.tif")
r_ahn3_25m <- raster("data/covariates/relief/ahn3_25m.tif")

# at the moment, we are using AHN2 as a reference for resampling and other
# preparation steps, so subtract AHN1 and AHN3 from AHN2
r_ahn2_ahn3 <- r_ahn2_25m - r_ahn3_25m
r_ahn2_ahn1 <- r_ahn2_25m - r_ahn1_25m

# plot differences in AHN
plot(r_ahn2_ahn3)
plot(r_ahn2_ahn1)

# save rasters to disk to explore in QGIS
writeRaster(r_ahn2_ahn3, "out/data/covariates/AHN_comparison/ahn2_minus_ahn3_25m.tif")
writeRaster(r_ahn2_ahn1, "out/data/covariates/AHN_comparison/ahn2_minus_ahn1_25m.tif")
