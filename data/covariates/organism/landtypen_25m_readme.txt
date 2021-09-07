#### Readme for landscape types (land typen) ####
By A.F. Helfenstein
2020-03-30


#### SOURCE FILE ####
Name:			SGR2_landtypen.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\GRR\SGR2)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		?
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (NE):	- Land(scape) types in 9 classes:
					1: Heuvelland
					2: Zandgebied
					3: Hoogveenontginningsgebied
					4: Rivierengebied
					5: Zeekleigebied
					6: Laagveengebied
					7: Droogmakerijen
					8: Kustzone
					9: Grote wateren
					10:
					11:
					12:
					13:


Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)

3. Rename from "landtypen_PolygonToRaster.tif" to "landtypen_25m.tif"



#### FINAL FILE ####
Name:		landtypen_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (NE):	- Land(scape) types in 9 classes:
					1: Heuvelland
					2: Zandgebied
					3: Hoogveenontginningsgebied
					4: Rivierengebied
					5: Zeekleigebied
					6: Laagveengebied
					7: Droogmakerijen
					8: Kustzone
					9: Grote wateren
					10:
					11:
					12:
					13:


Language:	Dutch