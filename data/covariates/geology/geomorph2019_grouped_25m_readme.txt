#### Readme for basic geomorphological class groups of the Netherlands (2019 version)) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			GMK2019-GeoMorfologie.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Geomorfologie\v2019)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- see website with legend info: https://legendageomorfologie.wur.nl/
				- basic geomorphological groups:
					1	=	B: Geisoleerde heuvels, heuvelruggen en dijken
					2	=	F: Plateau-achtige vormen
					3	=	L: Heuvels en heuvelruggen met bijbehorende vlakten en laagten
					4	=	M: Vlakten
					5	=	R: Dalen
					6	=	A: Wanden
					7	=	N: Niet- dalvormige laagten
					8	=	G: Waaiervormige glooingen
					9	=	E: Terrasvormen
					10	=	H: Niet-waaiervormige glooiingen
					11	=	D: Plateaus

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "Vormgroep_code") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)

4. Raster to other format (.tif)

5. Change name from "GMK2019GeoMorfologie_PolygonToRaster_vormgroep" to "geomorph2019_grouped_25m"



#### FINAL FILE ####
Name:			geomorph2019_grouped_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- see website with legend info: https://legendageomorfologie.wur.nl/
				- basic geomorphological groups:
					1	=	B: Geisoleerde heuvels, heuvelruggen en dijken
					2	=	F: Plateau-achtige vormen
					3	=	L: Heuvels en heuvelruggen met bijbehorende vlakten en laagten
					4	=	M: Vlakten
					5	=	R: Dalen
					6	=	A: Wanden
					7	=	N: Niet- dalvormige laagten
					8	=	G: Waaiervormige glooingen
					9	=	E: Terrasvormen
					10	=	H: Niet-waaiervormige glooiingen
					11	=	D: Plateaus

Language:		Dutch