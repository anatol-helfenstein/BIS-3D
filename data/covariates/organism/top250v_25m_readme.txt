#### Readme for landscape types (BORIS_refdata_top250vlak) data ####
By A.F. Helfenstein
2020-03-11


#### ORIGINAL ####
Name:		BORIS_refdata_top250vlak
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\BORIS_refdata\BORIS_refdata_top250vlak.lyr
Format: 	lyr file
Type:		polygon 
Gridsize:	?
Availability:	?
Temporal 
coverage:	?
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	Landscape types

Language:	English



#### PROCESSING ####
1. Clip to Landsgrens2017 (borders Netherlands)

2.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

3.	Raster to geoTIFF-format (conversion)

4. Rename from "top250v_Clip_PolygonToRaster.tif" to "top250v_25m.tif"



#### FINAL FILE ####
Name:		top250v_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Gridsize:	25x25m
Temporal 
coverage:	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	landscape types

Language:	English