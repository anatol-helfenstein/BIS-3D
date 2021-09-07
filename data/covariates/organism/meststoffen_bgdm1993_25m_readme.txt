#### Readme for animal manure data (Besluit Gebruik Dierlijke Meststoffen (BGDM) 1993) ####
By A.F. Helfenstein
2020-03-30


#### SOURCE FILE ####
Name:			Besluit_Gebruik_Dierlijke_Meststoffen_BGDM_1993.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\Milieu)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		1993 (?)
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		Binary variable animal manure:
					- 
					- aangewezen


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "GEBIED") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "BesluitGebruikDierlijkeMeststoffenBGDM1993_PolygonToRaster" to "meststoffen_bgdm1993_25m"



#### FINAL FILE ####
Name:			meststoffen_bgdm1993_25m.tif
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

Content:		Binary variable animal manure:
					- 
					- aangewezen

Language:		Dutch