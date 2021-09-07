#### Readme for landscape types (BORIS_refdata_Landschapstypen1994) ####
By A.F. Helfenstein
2020-03-11


#### SOURCE FILE ####
Name:		BORIS_refdata_Landschapstypen1994
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\BORIS_refdata\BORIS_refdata_Landschapstypen1994.lyr)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:	
Spatial 
coverage:	National
Projection:	RD_new
Content (NE):	- Landscape types in 9 classes:
					- Zandgebied
					- Heuvelland
					- Rivierengebied
					- Zeekleigebied
					- Hoogveenontginningsgebied
					- Verstedelijkt landschap
					- Kustzone
					- Droogmakerijen
					- Laagveengebied

Content (EN):	

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)

3. Rename from "Landschapstypen_PolygonToRaster.tif" to "landschapstypen_25m.tif"



#### FINAL FILE ####
Name:		landschapstypen_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	- Landscape types in 10 classes:
					- 0: water
					- 1: Zandgebied
					- 2: Heuvelland
					- 3: Rivierengebied
					- 4: Zeekleigebied
					- 5: Hoogveenontginningsgebied
					- 6: Verstedelijkt landschap
					- 7: Kustzone
					- 8: Droogmakerijen
					- 9: Laagveengebied



Language:	English