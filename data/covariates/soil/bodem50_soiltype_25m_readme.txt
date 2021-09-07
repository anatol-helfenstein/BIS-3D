#### Readme for soil types from bodem50 map ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			bodem50_gwt_tekst.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Bod-Gwt-50)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		?
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		315 categories e.g. Mn25A

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "EERSTE_BOD") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to geoTIFF-format (conversion)

3. Change name from "bodem50_eerste_bod_PolygonToRaster" to "bodem50_soiltype_25m"



#### FINAL FILE ####
Name:			bodem50_soiltype_25m.tif
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

Content:		315 categories e.g. Mn25A

Language:		Dutch