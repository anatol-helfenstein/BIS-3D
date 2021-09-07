#### Readme for forest vegetation type (bosstat1_geo2000) ####
By A.F. Helfenstein
2020-03-25


#### SOURCE FILE ####
Name:			bosstat1_geo2000.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Bos)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2000 (?)
Spatial 
coverage:		National
Projection:		RD_new
Content:		forest vegetation / tree type (variable "CODE"):
					101: Naaldhout
					102: Eik
					105: Beuk
					103: Populier
					106: lep
					104: Ov. Loofhout
					109: Hakhout
					111: Grienden
					108: Heide
					112: Zand
					110: Veen
					107: Riet


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "CODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "bosstat1_geo2000_PolygonToRaster" to "bos_veg_typ_25m"



#### FINAL FILE ####
Name:			bos_veg_typ_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content (EN):	forest vegetation / tree type (variable "CODE"):
					101: Naaldhout
					102: Eik
					105: Beuk
					103: Populier
					106: lep
					104: Ov. Loofhout
					109: Hakhout
					111: Grienden
					108: Heide
					112: Zand
					110: Veen
					107: Riet


Language:		Dutch