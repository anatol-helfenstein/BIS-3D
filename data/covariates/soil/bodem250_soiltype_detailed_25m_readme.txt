#### Readme for soil types in the Netherlands ####
By A.F. Helfenstein
2020-04-03


#### SOURCE FILE ####
Name:			Bodem_250.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		?
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		detailed soil types in the Netherlands (266 classes)
			see "bodem250_soiltype_detailed.csv"

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "CODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to GeoTIFF-format (conversion)

3. Change name from "Bodem_250_PolygonToRaster" to "bodem250_soiltype_detailed_25m"



#### FINAL FILE ####
Name:			bodem250_soiltype_detailed_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
Coverage:		?
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		detailed soil types in the Netherlands (266 classes)
			see "bodem250_soiltype_detailed.csv"

Language:		Dutch