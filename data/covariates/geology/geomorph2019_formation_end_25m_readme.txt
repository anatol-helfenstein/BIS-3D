#### Readme for end of geological formation process (from geomorphological map of the Netherlands (2019 version)) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			GMK2019-GeoMorfologie.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Geomorfologie\v2019)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- see website with legend info: https://legendageomorfologie.wur.nl/
				- Code for end of geological formation process that defines geomorphological unit
					1: Vanaf ca. 1850 - heden (Antropoceen).
					2: Holoceen
					3: Laat-Pleistoceen
					4: Vroeg- en Midden- Pleistoceen
					5: Pre-Pleistoceen

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "Geologischtijdperk_vormingsproces_eindigt_id" = "Period (Begin_vormingsproces_omschr)")* using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* something labeled incorrectly? is it beginning or end of formation process? TALK TO MARIJN!

4. Raster to other format (.tif)

5. Change name from "GMK2019GeoMorfologie_PolygonToRaster_vormingsproces_eindigt" to "geomorph2019_formation_end_25m"



#### FINAL FILE ####
Name:			geomorph2019_formation_end_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- see website with legend info: https://legendageomorfologie.wur.nl/
				- Code for end of geological formation process that defines geomorphological unit
					1: Vanaf ca. 1850 - heden (Antropoceen).
					2: Holoceen
					3: Laat-Pleistoceen
					4: Vroeg- en Midden- Pleistoceen
					5: Pre-Pleistoceen

Language:		Dutch