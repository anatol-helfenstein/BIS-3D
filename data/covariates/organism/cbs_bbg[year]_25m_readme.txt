#### Readme for Bodemgebruik in Nederland (bestand van het bodemgebruik (BBG)) or bodem statistiek of the Centraal Bureau voor de Statistiek (CBS) (CBS_BodStat_[year]) data ####
By A.F. Helfenstein
2020-03-24


#### ORIGINAL ####
Name:			CBS_BodStat_1993.lyr
			CBS_BodStat_1996.lyr
			CBS_BodStat_2000.lyr / CBS-BBG-2000.lyr (these are identical)
			CBS-BBG-2003-english.lyr
			CBS-BBG-2006.lyr
			CBS-BBG-2008.lyr
			CBS-BBG-2010.lyr
			CBS-BBG-2012.lyr
			CBS-BBG-2015.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\CBS_BBG_(Bodemstatistiek)
Format: 		ESRI layer file (.lyr)
Type:			polygon
Gridsize:	
Availability:	?
Temporal 
coverage:		1993, 1996, 2000, 2003, 2006, 2008, 2010, 2012, 2015
Spatial 
coverage:		Netherlands
Projection:		RD_new

Content (EN):		We are interested in the following variables in each of the files listed above:
			"BG_93", "BG_96", "BBG200A", "BG2000A", "BG2003A", "BG2006A", "BG06DIG1" (cat), "BG2008A", "BG2010A", "BG2012", "BG2015"

Language:		Dutch & English



#### PROCESSING ####
1. 	Polygon to raster using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
	* snap raster = all cells are designated to same geographical location

2. 	Raster to other format (.tif)

3. 	Change name from e.g. "CBSBBG2006_PolygonToRaster" to "cbs_bbg2006_25m" or
	change name from e.g. "CBSBBG2006_PolygonToRaster_cat" to "cbs_bbg2006_cat_25m" ("BG06DIG1" = cat)or
   	change name from e.g. "landgebruik_bbg2010_PolygonToRaster" to "cbs_bbg2010_25m"



#### FINAL FILE ####
Name:			cbs_bbg[year]_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 		geoTiff (.tif)
Type:			raster
Gridsize:		25m x 25m
Temporal 
coverage:		1993, 1996, 2000, 2003, 2006, 2008, 2010, 2012, 2015
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	

Language:		Dutch & English