#### Readme for history / age of forests (NedBos) ####
By A.F. Helfenstein
2020-03-25


#### SOURCE FILE ####
Name:			NedBos.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Bos)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		Before / after 1900
Spatial 
coverage:		National
Projection:		RD_new
Content:		forest types (variable "CAT" or "BOS"):
					0:
					1: Bos ontstaan voor 1900
					2: Bos ontstaan na 1900
					3: Bos op voormalige landbouwgronden
					4: Open Natuurgebieden
					9: Bos met onbekende voorgeschiedenis


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "BDT") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "NedBos_PolygonToRaster" to "bos_hist_25m"



#### FINAL FILE ####
Name:			bos_hist_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content (EN):	forest types:
					0:
					1: Bos ontstaan voor 1900
					2: Bos ontstaan na 1900
					3: Bos op voormalige landbouwgronden
					4: Open Natuurgebieden
					9: Bos met onbekende voorgeschiedenis


Language:		Dutch