#### Readme for forest types (AlBos-BosDoelTypen) ####
By A.F. Helfenstein
2020-03-25


#### SOURCE FILE ####
Name:			AlBos-BosDoelTypen.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Bos)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new
Content:		forest types (variable ("BDT"):
					- Grove den en berk
					- Grove den en eik
					- Grove den, douglas, eik en beuk
					- Douglas, beuk en eik
					- Douglas, beuk, eik en populier
					- Eik en beuk
					- Eik, beuk en populier
					- Es, eik, populier en els
					- Populier, els en wilg
					- Els en wilgl
					- Els en berk
					- Berk
					- Onbekend
					- Water
					- Bebouwing
					- <all other values>


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "BDT") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "AlBosBosDoelTypen_PolygonToRaster" to "bos_typen_25m"



#### FINAL FILE ####
Name:			bos_typen_25m.tif
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
					- Grove den en berk
					- Grove den en eik
					- Grove den, douglas, eik en beuk
					- Douglas, beuk en eik
					- Douglas, beuk, eik en populier
					- Eik en beuk
					- Eik, beuk en populier
					- Es, eik, populier en els
					- Populier, els en wilg
					- Els en wilgl
					- Els en berk
					- Berk
					- Onbekend
					- Water
					- Bebouwing
					- <all other values>


Language:		Dutch