#### Readme for Netherlands (less) detailed soil type map (Bodemkaart50000 Versie 2014) ####
By A.F. Helfenstein
2020-04-01


#### SOURCE FILE ####
Name:			Bodemkaart50000_v2014_grouped.lyr
				layer: "Bodemtypen"
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Bodem-50_2014)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		2014
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		(less) detailed soil type map of the Netherlands
				318 categories

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "BODEM1") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to GeoTIFF-format (conversion)

3. Change name from "Bodemtypen_PolygonToRaster" to "bodem2014_soiltype_25m"



#### FINAL FILE ####
Name:			bodem2014_soiltype_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
Coverage:		2014
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		(less) detailed soil type map of the Netherlands
				318 categories

Language:		Dutch