#### Readme for geomorphological map of the Netherlands (2004 version) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			Geomorfologische_Kaart_van_Nederland_versie_2004.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Geomorfologie\v2004)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		(2004 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		geomorphological units / classes / categories of the Netherlands

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "CODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "GeomorfologieNL_PolygonToRaster" to "geomorph2004_25m"



#### FINAL FILE ####
Name:			geomorph2004_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		(2004 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		geomorphological units / classes / categories of the Netherlands

Language:		Dutch