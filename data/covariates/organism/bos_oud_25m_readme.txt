#### Readme for history / age of forests (NedBos) ####
By A.F. Helfenstein
2020-03-26


#### SOURCE FILE ####
Name:			Oudste_Bossen_van_Nederland_2003.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Bos)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		
Spatial 
coverage:		National
Projection:		RD_new
Content (NE):	forest age:
					1: Overig Loof Ontstaan bosareaal voor 1900 en leeftijd onbekend
					2: Eik Ontstaan bosareaal voor 1900 en leeftijd onbekend
					3: Overig Loof Ontstaan bos en bosareaal voor 1900
					4: Eik Ontstaan voor 1900
					5: Groveden Ontstaan bosareaal voor 1900 en leeftijd onbekend
					6: Groveden Ontstaan voor 1900

Content (EN):	forest age:
					1: Other foliage creation of forest area before 1900 and age unknown
					2: Oak forest area before 1900 and age unknown
					3: Other foliage creation of forest and forest area before 1900
					4: Oak originated before 1900
					5: (Pinus sylvestris?) origin of forest area before 1900 and age unknown
					6: (Pinus sylvestris?) created before 1900


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "LEGENDA") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "Oudste_Bossen_van_Nederland_2003_PolygonToRaster" to "bos_oud_25m"



#### FINAL FILE ####
Name:			bos_oud_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content (NE):	forest age:
					1: Overig Loof Ontstaan bosareaal voor 1900 en leeftijd onbekend
					2: Eik Ontstaan bosareaal voor 1900 en leeftijd onbekend
					3: Overig Loof Ontstaan bos en bosareaal voor 1900
					4: Eik Ontstaan voor 1900
					5: Groveden Ontstaan bosareaal voor 1900 en leeftijd onbekend
					6: Groveden Ontstaan voor 1900

Content (EN):	forest age:
					1: Other foliage creation of forest area before 1900 and age unknown
					2: Oak forest area before 1900 and age unknown
					3: Other foliage creation of forest and forest area before 1900
					4: Oak originated before 1900
					5: (Pinus sylvestris?) origin of forest area before 1900 and age unknown
					6: (Pinus sylvestris?) created before 1900


Language:		Dutch