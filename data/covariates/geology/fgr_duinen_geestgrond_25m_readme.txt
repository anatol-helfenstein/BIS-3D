#### Readme for type of dunes (aanvulling duinen geestgrond) from Fysisch Geografische Regios (FGR) ####
By A.F. Helfenstein
2020-03-26


#### SOURCE FILE ####
Name:			Aanvulling_fysisch_geografische_regios_duinen.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Landschap)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		
Spatial 
coverage:		National
Projection:		RD_new
Content (NE):	dune type (aanvulling duinen geestgrond):
			1: geestgronden
			2: kalkarme duinen


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "AFK") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "FGRaanvullingduinengeestgrond_PolygonToRaster" to "fgr_duinen_geestgrond_25m"



#### FINAL FILE ####
Name:			fgr_duinen_geestgrond_25m.tif
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

Content (NE):	dune type (aanvulling duinen geestgrond):
			1: geestgronden
			2: kalkarme duinen


Language:		Dutch