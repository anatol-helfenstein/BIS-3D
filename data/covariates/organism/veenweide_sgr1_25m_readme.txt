#### Readme for Peat grassland / pasture areas management data (Veenweidegebieden) ####
By A.F. Helfenstein
2020-03-30


#### SOURCE FILE ####
Name:			SGR1_Veenweidegebieden.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\GRR\SGR1)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		?
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		attribute of interest "SCODE" (management focus in peat areas)
					0: not peatland area (?)
					1: accent landbouw
					2: accent landbouw en natuur
					3: accent natuur


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "SCODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "SGR1_Veenweidegebieden_PolygonToRaster" to "veenweide_sgr1_25m"



#### FINAL FILE ####
Name:			veenweide_sgr1_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		management focus in peat areas:
					0: not peatland area (?)
					1: accent landbouw
					2: accent landbouw en natuur
					3: accent natuur


Language:		Dutch