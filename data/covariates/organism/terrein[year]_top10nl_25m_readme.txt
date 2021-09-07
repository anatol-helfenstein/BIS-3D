#### Readme for overgrown terrein data (Top10-NL 2009-2019) ####
By A.F. Helfenstein
2020-04-07


#### ORIGINAL ####
Name:		Terrein.lyr
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-NL)
		*there is a subfolder for each year
Format: 	ArcGIS layer file (.lyr)
Type:		polygon	
Availability:	?
Temporal 
coverage:	2009 - 2019
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	- vegetated / overgrown terrein from topographic inventory classified in groups.
		- Years 2015, 2016 and 2019 files have a bug because when using "polygon to raster" 
		function in ArcGIS, GIS always crashed at 62% completion of the operation
		- groups in files 2009-2012:
			1	=	aanlegsteiger
			2	=	akkerland
			3	=	basaltblokken, steenglooiing
			4	=	bebouwd gebied
			5	=	boomgaard
			6	=	boomkwekerij
			7	=	bos: gemengd bos
			8	=	bos: griend
			9	=	bos: loofbos
			10	=	bos: naaldbos
			11	=	dodenakker
			12	=	dodenakker met bos
			13	=	fruitkwekeij
			14	=	grasland
			15	=	heide
			17	=	onbekend
			18	=	overig
			19	=	populieren
			20	=	spoorbaanlichaam
			21	=	zand
		- groups in 2013 file:
			1	=	grasland
			2	=	populieren
			3	=	overig
			4	=	akkerland
			5	=	bos: loofbos
			6	=	bos: gemengd bos
			7	=	bos: naaldbos
			8	=	fruitkwekerij
			9	=	zand
			10	=	basaltblokken, steenglooiing
			11	=	dodenakker
			12	=	boomgaard
			13	=	boomkwekerij
			14	=	bebouwd gebied
			15	=	spoorbaanlichaam
			16	=	aanlegsteiger
			17	=	heide
			18	=	dodenakker met bos
			19	=	bos: griend
		- groups in 2014 file:
			1	=	grasland
			2	=	akkerland
			3	=	overig
			4	=	bebouwd gebied
			5	=	fruitkwekerij
			6	=	bos: loofbos
			7	=	bos: naaldbos
			8	=	zand
			9	=	bos: gemengd bos
			10	=	populieren
			11	=	dodenakker
			12	=	boomgaard
			13	=	basaltblokken, steenglooiing
			14	=	boomkwekerij
			15	=	aanlegsteiger
			16	=	spoorbaanlichaam
			17	=	heide
			18	=	dodenakker met bos
			19	=	bos: griend
		- groups in 2017 and 2018 files:
			101	=	bebouwd gebied
			502	=	bos: loofbos
			505	=	bos: naaldbos
			506	=	bos: gemengd bos
			507	=	bos: griend
			508	=	populieren
			520	=	akkerland
			521	=	grasland
			522	=	boomgaard
			523	=	boomkwekerij
			524	=	heide
			525	=	zand
			526	=	overig
			530	=	dodenakker
			531	=	fruitkwekerij
			629	=	basaltblokken, steenglooiing
			651	=	aanlegsteiger
			999	=	braakliggend, spoorbaanlichaam

Language:	Dutch



#### PROCESSING ####
1. Polygon to raster (variable of interest: "TYPELANDGEBRUIK_C1", "TYPELANDGEBRUIK" or "TDNCODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

3. Change name from "Terrein_PolygonToRaster_[year]" to "terrein[year]_top10nl_25m".



#### FINAL FILE ####
Name:		terrein[year]_top10nl_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	GeoTiff (.tif)
Type:		raster
Gridsize:	25m x 25m
Temporal 
coverage:	2009 - 2019
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	- vegetated / overgrown terrein from topographic inventory classified in groups.
		- Years 2015, 2016 and 2019 files have a bug because when using "polygon to raster" 
		function in ArcGIS, GIS always crashed at 62% completion of the operation
		- groups in files 2009-2012:
			1	=	aanlegsteiger
			2	=	akkerland
			3	=	basaltblokken, steenglooiing
			4	=	bebouwd gebied
			5	=	boomgaard
			6	=	boomkwekerij
			7	=	bos: gemengd bos
			8	=	bos: griend
			9	=	bos: loofbos
			10	=	bos: naaldbos
			11	=	dodenakker
			12	=	dodenakker met bos
			13	=	fruitkwekeij
			14	=	grasland
			15	=	heide
			17	=	onbekend
			18	=	overig
			19	=	populieren
			20	=	spoorbaanlichaam
			21	=	zand
		- groups in 2013 file:
			1	=	grasland
			2	=	populieren
			3	=	overig
			4	=	akkerland
			5	=	bos: loofbos
			6	=	bos: gemengd bos
			7	=	bos: naaldbos
			8	=	fruitkwekerij
			9	=	zand
			10	=	basaltblokken, steenglooiing
			11	=	dodenakker
			12	=	boomgaard
			13	=	boomkwekerij
			14	=	bebouwd gebied
			15	=	spoorbaanlichaam
			16	=	aanlegsteiger
			17	=	heide
			18	=	dodenakker met bos
			19	=	bos: griend
		- groups in 2014 file:
			1	=	grasland
			2	=	akkerland
			3	=	overig
			4	=	bebouwd gebied
			5	=	fruitkwekerij
			6	=	bos: loofbos
			7	=	bos: naaldbos
			8	=	zand
			9	=	bos: gemengd bos
			10	=	populieren
			11	=	dodenakker
			12	=	boomgaard
			13	=	basaltblokken, steenglooiing
			14	=	boomkwekerij
			15	=	aanlegsteiger
			16	=	spoorbaanlichaam
			17	=	heide
			18	=	dodenakker met bos
			19	=	bos: griend
		- groups in 2017 and 2018 files:
			101	=	bebouwd gebied
			502	=	bos: loofbos
			505	=	bos: naaldbos
			506	=	bos: gemengd bos
			507	=	bos: griend
			508	=	populieren
			520	=	akkerland
			521	=	grasland
			522	=	boomgaard
			523	=	boomkwekerij
			524	=	heide
			525	=	zand
			526	=	overig
			530	=	dodenakker
			531	=	fruitkwekerij
			629	=	basaltblokken, steenglooiing
			651	=	aanlegsteiger
			999	=	braakliggend, spoorbaanlichaam


Language:	Dutch