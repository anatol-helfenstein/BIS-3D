#### Readme for overgrown terrain data (Top50-NL 2019) ####
By A.F. Helfenstein
2020-04-09


#### ORIGINAL ####
Name:		TOP50NL_2019_Sep.lyr
Source:		GeoDesk
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top50-NL
Format: 	ArcGIS layer file (.lyr)
Type:		polygon	
Availability:	?
Temporal 
coverage:	2019
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	land cover terrain data from top50 NL 2019:	
			1101	=	bebouwd gebied
			1107	=	kassengebied
			1437	=	spoorbaanlichaam
			1502	=	bos: loofbos
			1505	=	bos: naaldbos
			1506	=	bos: gemengd bos
			1507	=	bos: griend
			1508	=	populieren
			1520	=	akkerland
			1521	=	grasland
			1522	=	boomgaard
			1523	=	boomkwekerij
			1524	=	heide
			1525	=	zand
			1526	=	overig
			1527	=	overig
			1529	=	ongedefinieerd
			1530	=	dodenakker
			1531	=	fruitkwekerij
			1629	=	basaltblokken, steenglooiing
			1651	=	aanlegsteiger

Language:	Dutch



#### PROCESSING ####
1. 	Polygon to raster (variable of interest: "TDNCODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
	* snap raster = all cells are designated to same geographical location

2. 	Raster to other format (.tif)

3. 	Change name from "TERREIN_VLAK_PolygonToRaster_2019" to "terrein2019_top50nl_25m"



#### FINAL FILE ####
Name:		terrein2019_top50nl_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	GeoTiff (.tif)
Type:		raster
Gridsize:	25m x 25m
Temporal 
coverage:	2019
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	land cover terrain data from top50 NL 2019:	
			1101	=	bebouwd gebied
			1107	=	kassengebied
			1437	=	spoorbaanlichaam
			1502	=	bos: loofbos
			1505	=	bos: naaldbos
			1506	=	bos: gemengd bos
			1507	=	bos: griend
			1508	=	populieren
			1520	=	akkerland
			1521	=	grasland
			1522	=	boomgaard
			1523	=	boomkwekerij
			1524	=	heide
			1525	=	zand
			1526	=	overig
			1527	=	overig
			1529	=	ongedefinieerd
			1530	=	dodenakker
			1531	=	fruitkwekerij
			1629	=	basaltblokken, steenglooiing
			1651	=	aanlegsteiger

Language:	Dutch