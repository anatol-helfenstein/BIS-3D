#### Readme for geomorphological categories in base soil map (basiskaart) ####
By A.F. Helfenstein
2020-04-01


#### SOURCE FILE ####
Name:			aw_basiskrt_mei_2006.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Aardkundige_Waarden)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2006 (?)
Spatial 
coverage:		National / Netherlands (not full coverage)
Projection:		RD_new
Content:		geomorphological categories in the base soil map (see "basiskaart_geomorf.csv")

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "GEOMORF") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

3. Change name from "aw_basiskrt_mei_2006_PolygonToRaster" to "basiskaart_geomorf_25m"



#### FINAL FILE ####
Name:			basiskaart_geomorf_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National / Netherlands (not full coverage)
Projection:		RD_new

Content:		geomorphological categories in the base soil map (see "basiskaart_geomorf.csv")

Language:		Dutch