#### Readme for overgrown terrein data (BGT data 2016 & 2018) ####
By A.F. Helfenstein
2020-04-03


#### ORIGINAL ####
Name:		BGT november 2016 -> Vlakgeometrie -> begroeidTerreindeel_v
		BGT_nov17_Vlakken.lyr -> begroeidTerreindeel_v
		BGT november 2018 -> Vlakgeometrie -> begroeidTerreindeel_v
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\BGT)
Format: 	ArcGIS layer file (.lyr)
Type:		polygon	
Availability:	?
Temporal 
coverage:	2016 - 2018
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	vegetated / overgrown terrein from topographic inventory classified in 34 groups
		see "terrein2016_begroeid_bgt.csv", "terrein2017_begroeid_bgt.csv" and "terrein2018_begroeid_bgt.csv" attribute tables

Language:	Dutch



#### PROCESSING ####
1. Polygon to raster (variable of interest: "fysiekVoorkomen") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

3.	Change name from "begroeidTerreindeel_v_PolygonToRaster_2016" to "terrein2016_begroeid_bgt_25m".
	Change name from "begroeidTerreindeel_v_PolygonToRaster_2017" to "terrein2017_begroeid_bgt_25m"
	Change name from "begroeidTerreindeel_v_PolygonToRaster_2018" to "terrein2018_begroeid_bgt_25m"



#### FINAL FILE ####
Name:		terrein2016_begroeid_bgt_25m.tif
		terrein2017_begroeid_bgt_25m.tif
		terrein2018_begroeid_bgt_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	GeoTiff (.tif)
Type:		raster
Gridsize:	25m x 25m
Temporal 
coverage:	2016 - 2018
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	vegetated / overgrown terrein from topographic inventory classified in 34 groups
		see "terrein2016_begroeid_bgt.csv", "terrein2017_begroeid_bgt.csv" and "terrein2018_begroeid_bgt.csv" attribute tables

Language:	Dutch