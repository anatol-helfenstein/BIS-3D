#### Readme for overgrown terrain data (Top10-Classic 1996-2003) ####
By A.F. Helfenstein
2020-04-09


#### ORIGINAL ####
Name:		* Layerfiles before 1996 aren't complete for the entire Netherlands
		Top10_Classic_1996.lyr
		Top10_Classic_1997.lyr
		Top10_Classic_1998.lyr
		Top10_Classic_1999.lyr
		Top10_Classic_2000.lyr
		Top10_Classic_2001.lyr
		Top10_Classic_2002.lyr
		Top10_Classic_2003.lyr
Source:		GeoDesk
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-Classic
Format: 	ArcGIS layer file (.lyr)
Type:		polygon	
Availability:	?
Temporal 
coverage:	1996-2003
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	- vegetated / overgrown terrein from topographic inventory classified in groups:
			101; 102	=	bebouwing
			107	=	kas/warenhuis
			200	=	autosnelweg
			234; 250; 240; 230; 220; 210	=	hoofdweg
			290; 320; 330; 280; 310; 300	=	regionale weg
			334; 324; 314; 208; 287; 244	=	lokale weg
			343; 342; 341	=	ged. verhard / onverhard
			360	=	fietspad
			346; 345; 390; 340; 353; 347	=	overige weg of straat
			502	=	loofbos
			505	=	naaldbos
			506	=	gemengd bos
			507	=	griend
			520	=	bouwland
			521	=	grasland
			508	=	populierenopstand
			522	=	boomgaard
			531	=	fruitwekerij
			523	=	boomwekerij
			524	=	heide
			525	=	zand
			530	=	begraafplaats
			526	=	overig gebruik
			654	=	dok
			651	=	steiger
			659; 629	=	steenglooing / krib
			690; 611; 610	=	water
			630	=	dras en riet
			621	=	droogvallende grond

Language:	Dutch



#### PROCESSING ####
1. 	Polygon to raster (variable of interest: "TOPO_CODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
	* snap raster = all cells are designated to same geographical location

2. 	Raster to other format (.tif)

3. 	Change name from "Top10Vlakken[year]_PolygonToRaster" to "terrein[year]_top10classic_25m"



#### FINAL FILE ####
Name:		terrein[year]_top10classic_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	GeoTiff (.tif)
Type:		raster
Gridsize:	25m x 25m
Temporal 
coverage:	1996-2003
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	- vegetated / overgrown terrein from topographic inventory classified in groups:
			101; 102	=	bebouwing
			107	=	kas/warenhuis
			200	=	autosnelweg
			234; 250; 240; 230; 220; 210	=	hoofdweg
			290; 320; 330; 280; 310; 300	=	regionale weg
			334; 324; 314; 208; 287; 244	=	lokale weg
			343; 342; 341	=	ged. verhard / onverhard
			360	=	fietspad
			346; 345; 390; 340; 353; 347	=	overige weg of straat
			502	=	loofbos
			505	=	naaldbos
			506	=	gemengd bos
			507	=	griend
			520	=	bouwland
			521	=	grasland
			508	=	populierenopstand
			522	=	boomgaard
			531	=	fruitwekerij
			523	=	boomwekerij
			524	=	heide
			525	=	zand
			530	=	begraafplaats
			526	=	overig gebruik
			654	=	dok
			651	=	steiger
			659; 629	=	steenglooing / krib
			690; 611; 610	=	water
			630	=	dras en riet
			621	=	droogvallende grond


Language:	Dutch