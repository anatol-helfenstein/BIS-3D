#### Readme for subsurface material in groundwater zones ####
By A.F. Helfenstein
2020-04-01


#### SOURCE FILE ####
Name:			KRW_Grote_grondwaterlichamen_2004.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\Water\KRW)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2004
Spatial 
coverage:		National
Projection:		RD_new
Content (NE):	subsurface material in groundwater zones (appear to be rough approximations):
					1: zand
					2: klei
					3: duin
					4: krijt


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "GWBSUBST") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "KRWGrotegrondwaterlichamen2004_PolygonToRaster" to "grondwater_2004_25m"



#### FINAL FILE ####
Name:			grondwater_2004_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content (NE):	subsurface material in groundwater zones (appear to be rough approximations):
					1: zand
					2: klei
					3: duin
					4: krijt


Language:		Dutch