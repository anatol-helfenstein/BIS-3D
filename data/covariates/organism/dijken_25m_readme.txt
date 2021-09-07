#### Readme for dikes in the Netherlands (from geomorphological map of the Netherlands (2019 version) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			GMK2019-Dijken.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Geomorfologie\v2019)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		binary variable: dikes / no dikes

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "GeomorfCcode_4") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "GMK2019Dijken_PolygonToRaster" to "dijken_25m"



#### FINAL FILE ####
Name:			dijken_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		binary variable: dikes / no dikes

Language:		Dutch