#### Readme for Peat grassland / pasture areas management data (Veenweidegebieden) ####
By A.F. Helfenstein
2020-03-30


#### SOURCE FILE ####
Name:			SGR2_veenweide.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\GRR\SGR2)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		?
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		attribute of interest "OMSCHRIJF" (management focus in peat areas)
					1: overige veenweidegebieden
					2: no data (?) / not peatlands (?)
					3: zoekgebieden strategie vernatting
					4: keuze actieve vernatting of polderlandschap


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "OMSCHRIJF") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "SGR2_veenweide_PolygonToRaster" to "veenweide_sgr2_25m"



#### FINAL FILE ####
Name:			veenweide_sgr2_25m.tif
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

Content:		attribute of interest "OMSCHRIJF" (management focus in peat areas)
					- no data (?) / not peatlands (?)
					- keuze actieve vernatting of polderlandschap
					- zoekgebieden strategie vernatting
					- overige veenweidegebieden


Language:		Dutch