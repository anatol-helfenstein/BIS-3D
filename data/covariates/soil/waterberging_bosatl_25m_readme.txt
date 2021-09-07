#### Readme for water holding capacity (forest atlas) ####
By A.F. Helfenstein
2020-03-06


#### SOURCE FILE ####
Name:		Waterbergingsvermogen_bodemkaart250_bosatlas.lyr

Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles)
		or
		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem)

Format: 	ESRI format LayerFile
Type:		Polygons
Availability:	
Spatial 
coverage:	National
Projection:	RD_new
Content (D):	- Water holding capacity in 5 classes:
				1) groot
				2) gemiddeld
				3) gering
				4) buitendijks
				5) bebouwing

Content (EN):	- Water holding capacity in 5 classes:
				1) large
				2) medium
				3) low
				4) outside the dikes
				5) buildings/urban/concrete

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)



#### FINAL FILE ####
Name:		waterberging_bosatl_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 	geoTIFF
Type:		Raster
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	- Water holding capacity in 5 classes:
				0: supposed to be NA (no values)
				1: large
				2: medium
				3: low
				8: outside the dikes
				9: buildings/urban/concrete



Language:	English