#### Readme for main soil types in the Netherlands ####
By A.F. Helfenstein
2020-04-03


#### SOURCE FILE ####
Name:			Bodem_250_hoofdklasses.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		?
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		main soil types:
				1	=	Z + R
				2	=	Veen-gronden  (V)
				3	=	Bebouwd
				4	=	Oude klei  (K)
				5	=	Jonge Rivierklei  (R)
				6	=	Water
				7	=	Zeeklei-gronden  (M)
				8	=	Zand-gronden  (Z)
				9	=	Leem-gronden  (L)
				10	=	Opgehoogd
				11	=	Z + K
				12	=	Stenige gronden  (S)
				13	=	Z + V
				14	=	Afgegraven

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "CODE2") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to GeoTIFF-format (conversion)

3. Change name from "Bodem_250_hoofdklasses_PolygonToRaster" to "bodem_hoofdklasses_25m"



#### FINAL FILE ####
Name:			bodem_hoofdklasses_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
Coverage:		?
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		main soil types:
				1	=	Z + R
				2	=	Veen-gronden  (V)
				3	=	Bebouwd
				4	=	Oude klei  (K)
				5	=	Jonge Rivierklei  (R)
				6	=	Water
				7	=	Zeeklei-gronden  (M)
				8	=	Zand-gronden  (Z)
				9	=	Leem-gronden  (L)
				10	=	Opgehoogd
				11	=	Z + K
				12	=	Stenige gronden  (S)
				13	=	Z + V
				14	=	Afgegraven

Language:		Dutch