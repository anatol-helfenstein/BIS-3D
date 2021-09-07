#### Readme for soil texture map from BOFEK2012 (version 2) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			BOFEK2012_versie2.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\BOFEK)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		2012 (?)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		soil texture classes (74 categories)

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "BOFEK2012") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to geoTIFF-format (conversion)

3. Change name from "BOFEK2012_versie2_PolygonToRaster" to "bofek2012_texture_25m"



#### FINAL FILE ####
Name:			bofek2012_texture_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
Coverage:		2012 (?)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		soil texture classes (74 categories)

Language:		Dutch