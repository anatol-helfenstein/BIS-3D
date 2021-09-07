#### Readme for dike ring areas in 2012 (Nationaal_Basisbestand_2012_Dijkringgebieden_4_0) ####
By A.F. Helfenstein
2020-03-11


#### SOURCE FILE ####
Name:		Nationaal_Basisbestand_2012_Dijkringgebieden_4_0
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\DijkringGebieden)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:	
Spatial 
coverage:	National
Projection:	RD_new
Content (NE):	- areas protected behind dikes (in polygons)

Content (EN):	

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)

3. Rename from "Nationaal_Basisbestand_2012_Dijkringgebieden_4_0_PolygonToRaster" to "dijkringgebieden2012_25m.tif"



#### FINAL FILE ####
Name:		dijkringgebieden2012_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	- areas protected behind dikes in 2012 in many categories
* All categories can be aggreated into (behind dikes / not behind dikes) in R



Language:	English