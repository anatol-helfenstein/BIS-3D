#### Readme for geomorphological map of the Netherlands (2008 version) ####
By A.F. Helfenstein
2020-03-28


#### SOURCE FILE ####
Name:			GKN2008-30eenh-Eng.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Geomorfologie\v2008)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		(2008 version)
Spatial 
coverage:		National
Projection:		RD_new

Content:		attribute of interest "GENESE_AK" (geomorphological map)
				see geomorph2008_attributes.csv


Language:		English
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "GENESE_AK") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "Geomorfologische_Kaart_van_Nederland_versie_2008_PolygonToRaster_genese" to "geomorph2008_genese_25m"



#### FINAL FILE ####
Name:			geomorph2008_genese_25m.tif
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

Content:		attribute of interest "CODE" (detailed geomorphological map)
				see geomorph2008_attributes.csv


Language:		English