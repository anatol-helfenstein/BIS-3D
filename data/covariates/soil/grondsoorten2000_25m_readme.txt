#### Readme for soil texture base map (grondsoorten versie 2000) ####
By A.F. Helfenstein
2020-04-03


#### SOURCE FILE ####
Name:			Grondsoortenkaart_2000.lyr
Source:			(\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:		
Spatial 
coverage:		National / Netherlands
Projection:		RD_new
Content (NE):	- Soil characteristics in 9 classes:
					- stedelijk gebied
					- leem
					- lichte klei
					- lichte zavel
					- veen
					- zoet water
					- zand
					- zware klei
					- zware zavel

Content (EN):	- Soil characteristics in 9 classes:
					- urban areas
					- loam
					- light clay
					- lichte zavel (?)
					- peat
					- fresh water
					- sand
					- heavy clay (?)
					- heavy zavel (?)

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)

3.	Change name of tif file from "Grondsoortenkaart_2000_PolygonToRaster" to "grondsoorten2000_25m"



#### FINAL FILE ####
Name:			grondsoorten2000_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content (EN):	- Soil characteristics in 9 classes:
			98	=	urban areas
			7	=	loam
			5	=	light clay
			3	=	lichte zavel (?)
			1	=	peat
			99	=	fresh water
			2	=	sand
			6	=	heavy clay (?)
			4	=	heavy zavel (?)
			97	=	no data
			0	=	no data

Language:		English