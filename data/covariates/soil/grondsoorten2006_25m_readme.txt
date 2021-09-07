#### Readme for soil characteristics map (grondsoortenkaart 2006) ####
By A.F. Helfenstein
2020-03-06


#### SOURCE FILE ####
Name:			Grondsoortenkaart 2006	*
			or
			Grondsoortenkaart_2006.lyr	**
			or
			Grondsoortenkaart_2006_50x50Grid.lyr (as raster)	**

Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles)	*
			or
			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem)	**

Format: 		ESRI format LayerFile
Type:			Polygons
Availability:		
Spatial 
coverage:		National / Netherlands
Projection:		RD_new
Content (NE):		- Soil characteristics in 10 classes:
					- bebouwing, enz
					- leem
					- lichte klei
					- lichte zavel
					- moerig op zand
					- veen
					- water
					- zand
					- zware klei
					- zware zavel

Content (EN):		- Soil characteristics in 10 classes:
					- buildings
					- loam
					- light clay
					- lichte zavel (?)
					- peaty sand (?)
					- peat
					- water
					- sand
					- heavy clay (?)
					- heavy zavel (?)

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)



#### FINAL FILE ####
Name:			grondsoorten2006_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content (EN):	- Soil characteristics in 10 classes:
					8	=	buildings
					10	=	loam
					5	=	lichte klei (light clay?)
					1	=	lichte zavel (?)
					9	=	moerig op zand (peat on sand ?)
					7	=	veen (peat)
					2	=	water
					3	=	sand
					6	=	zware klei (heavy clay (?))
					4	=	zware zavel (heavy zavel (?))



Language:		English