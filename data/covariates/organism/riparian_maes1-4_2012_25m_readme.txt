#### Readme for riparian zones land cover & land use data ####
By A.F. Helfenstein
2020-03-25


#### ORIGINAL ####
Name:			Riparian_Zones_Land_Cover_Land Use_2012_NL_MAES_4.lyr


Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\Copernicus_Land_Monitoring_Data\Riparian_Zones
Format: 		ESRI layer file
Type:			polygon
Gridsize:		
Availability:	?
Temporal 
coverage:		2012
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land cover and land use in riparian zones (along rivers & water bodies) in the Netherlands
				- variable "maes1":
					1: urban
					2: croplands
					3: woodland
					4: grassland
					5: heathland
					6: sparsely vegetated land
					7: wetland
					8: lagoon, coastal wetlands
					9: rivers and lakes
					10: marine (other)
				- variable "maes2":
					- e.g. 3.3
				- variable "maes3":
					- e.g. 3.3.1
				- variable "maes4":
					- about 140 categories (open layerfile for details)
					- e.g. 3.3.1.2: mixed swamp forest (T.C.D. > 80%)
					- probably too detailed for BISplus
			* I would use maes2 or maes3 (at most) as a predictor


Language:		English



#### PROCESSING ####
1. Change projection using ArcGIS tool "project" to RDnew

2. Polygon to raster using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

3. Raster to other format (.tif)

4. Change name from e.g. "Riparian_Zones_Land_Cover_LandUse_2012_NL_PolygonToRaster_maes4" to "riparian_maes4_2012_25m"



#### FINAL FILE ####
Name:			riparian_maes1_2012_25m.tif
			riparian_maes2_2012_25m.tif
			riparian_maes3_2012_25m.tif
			riparian_maes4_2012_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal 
coverage:		2012
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land cover and land use in riparian zones (along rivers & water bodies) in the Netherlands
					- variable "maes1":
						1: urban
						2: croplands
						3: woodland
						4: grassland
						5: heathland
						6: sparsely vegetated land
						7: wetland
						8: lagoon, coastal wetlands
						9: rivers and lakes
						10: marine (other)
					- variable "maes2":
						- e.g. 3.3
					- variable "maes3":
						- e.g. 3.3.1
					- variable "maes4":
						- about 140 categories (open layerfile for details)
						- e.g. 3.3.1.2: mixed swamp forest (T.C.D. > 80%)
						- probably too detailed for BISplus
				* I would use maes2 or maes3 (at most) as a predictor


Language:	English