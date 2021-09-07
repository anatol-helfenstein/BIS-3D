#### Readme for land use map (LGN4) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN4_percelen.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-4
Format: 		ESRI layer file (.lyr)
Type:			raster
Gridsize:		25m x 25m
Availability:	?
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: grass (gras)
					2: maize (mais)
					3: potatoes (aardappelen)
					4: beetroot (bieten)
					5: cereals (granen)
					6: other crops (overige gewassen)
					7: geen landbouw
					10: flower bulbs (bloembollen)


Language:	Dutch



#### PROCESSING ####
1. Polygon to raster using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

3. Change name from "LGN4_percelen_PolygonToRaster" to "lgn4_percelen_25m"



#### FINAL FILE ####
Name:			lgn4_percelen_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\raw\organism\
Format: 		GeoTiff (.tif)
Type:			raster
Gridsize:		25m x 25m
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: grass (gras)
					2: maize (mais)
					3: potatoes (aardappelen)
					4: beetroot (bieten)
					5: cereals (granen)
					6: other crops (overige gewassen)
					7: geen landbouw
					10: flower bulbs (bloembollen)


Language:	Dutch & English