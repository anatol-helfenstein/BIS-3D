#### Readme for KNMI maximum temperature data ####
By T.L. van Orsouw
October 7th, 2019


#### ORIGINAL ####
Name:		temperature - Long term average 1981-2010 - Average yearly maximum temperature
Source:		https://data.knmi.nl/datasets/Txg4/4
Format: 	netCDF
Type:		Raster 
Gridsize:	1000x1000m
Availability:	2012 version
Temporal 
coverage:	1981-2010
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	Prediction of mean yearly maximum temperature based on observations and universal kriging.

Language:	English



#### PROCESSING ####
1.	Conversion from netCDF to ESRI-raster.

2.	Defined projection (RD_new).

3.	Resampling from 1000x1000m grid to 25x25m, using existing DEM (AHN2) to snap the raster to.

4.	Clipped raster using international border of The Netherlands.	

5.	Coversion to geoTIFF



#### FINAL FILE ####
Name:		temp_yearlyax_1981_2010_25m.tif
Location:	W:\ESG\DOW_SGL\Projects\GRS33306development\spatial_data\DutchClimateData
Format: 	geoTIFF (1-band)
Type:		Raster
Gridsize:	25x25m
Temporal 
coverage:	1981-2010
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	Prediction of mean yearly maximum temperature based on observations and universal kriging.

Language:	English