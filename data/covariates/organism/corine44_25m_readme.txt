#### Readme for land use EU (CORINE44_100) data ####
By A.F. Helfenstein
2020-03-11


#### ORIGINAL ####
Name:		CORINE44_100
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\_Europe_Europa\LandSurface
Format: 	ESRI layer file
Type:		raster tiff 
Gridsize:	100m x 100m
Availability:	?
Temporal 
coverage:	?
Spatial 
coverage:	Europe
Projection:	WGS 1972 Albers

Content (EN):	land use in Europe (40 categories)

Language:	English



#### PROCESSING ####
1. Project raster from "WGS 1972 Albers" to RD_new

2. Clip raster to Landsgrens2017 (borders Netherlands)

3. Raster to geoTIFF-format (conversion)

4. Rename from "CORINE44_100_ProjectRaster_C" to "corine44_25m.tif"



#### FINAL FILE ####
Name:		corine44_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Gridsize:	25x25m
Temporal 
coverage:	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	land use (40 categories)

Language:	English