#### Readme for management type from Informatiemodel Natuur (IMNA) beheergebied 2019 ####
By A.F. Helfenstein
2020-03-30


#### SOURCE FILE ####
Name:			IMNA20190418_BeheerGebied_2019.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\Natuur\IMNAB\IMNA_20190418_2019)
				* content of layer files in folders "IMNA_20181203_2019" and "IMNA_20190418_2019" are (almost) exactly the same.
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2019
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		- mangement type (attribute of interest "BeheerType"):
				- about 78 categories (see "imna2019_beheergebied.csv")


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "BeheerType") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "IMNA20190418_BeheerGebied_2019_PolygonToRaster" to "imna2019_beheergebied_25m"



#### FINAL FILE ####
Name:			imna2019_beheergebied_25m.tif
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

Content:		- mangement type (attribute of interest "BeheerType"):
				- about 78 categories (see "imna2019_beheergebied.csv")


Language:		Dutch