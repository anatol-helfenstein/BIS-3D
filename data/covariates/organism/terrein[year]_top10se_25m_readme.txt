#### Readme for overgrown terrain data (Top10-SE 2003-2006) ####
By A.F. Helfenstein
2020-04-07


#### ORIGINAL ####
Name:		TOP10_vlakken_2003.lyr
		Top10_Vlakken_2004.lyr
		Top10_Vlakken_2005.lyr
		Top10_Vlakken_2006.lyr
Source:		GeoDesk (there is a subfolder for each year)
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-SE\Versie 2003
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-SE\Versie 2004
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-SE\Versie 2005
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-SE\Versie 2006
Format: 	ArcGIS layer file (.lyr)
Type:		polygon	
Availability:	?
Temporal 
coverage:	2003 - 2006
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

3. 	Change name from "TOP10vlakken2003_PolygonToRaster", "Top102004Vlakken_PolygonToRaster, "Top10Vlakken_PolygonToRaster_2005" and "Top10Vlakken_PolygonToRaster_2006"
	to "terrein[year]_top10se_25m"



#### FINAL FILE ####
Name:		terrein[year]_top10se_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	GeoTiff (.tif)
Type:		raster
Gridsize:	25m x 25m
Temporal 
coverage:	2003-2006
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