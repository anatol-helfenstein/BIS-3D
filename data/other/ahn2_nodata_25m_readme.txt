#### Readme for AHN2 NO DATA at 25m resolution ####
By A.F. Helfenstein
2020-09-14


#### ORIGINAL ####
Name:		ahn2_5m_nodata.tif
Source:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\relief
Format: 	GeoTIFF
Type:		raster
Gridsize:	5m x 5m
Availability:	?
Temporal 
coverage:	
Spatial 
coverage:	Netherlands
Projection:	RD_new

Content (EN):	The "ahn2_5m_nodata.tif" file contains all buildings and water bodies. Might be useful to clip all covariates by this map...

Language:	



#### PROCESSING ####
1. Warp (reproject) raster from 5m to 25m resolution using method "cubispline" in QGIS (runs gdalwarp using GDAL in background)
Note: Not sure which method is best to use? I think cubicspline is best for continuous data



#### FINAL FILE ####
Name:		ahn2_nodata_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\relief
Format: 	GeoTiff files (.tif)
Type:		raster
Gridsize:	25m x 25m
Temporal 
coverage:	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	The "ahn2_nodata_25m.tif" file contains all buildings and water bodies. Might be useful to clip all covariates by this map...

Language:	