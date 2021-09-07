#### Readme for paleogeographic maps of the Netherlands ####
By A.F. Helfenstein
2021-03-23


#### SOURCE FILE ####
Name:			paleogeografie[year].lyr (link source to geodatabase)
Source:			Rijksdienst voor het Cultureel Erfgoed, TNO, Deltares
			https://www.cultureelerfgoed.nl/onderwerpen/bronnen-en-kaarten/documenten/publicaties/2019/01/01/paleogeografische-kaarten-zip
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:		Free download of GIS files via website
Temporal
coverage:		9000 v.Chr. - 2000 n.Chr
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		Paleogeographic zones of the Netherlands over time. 13 Maps from 9000 B.C. - 2000 A.D.
			See categories/classes in attribute tables (paleogeografie[year]_25m_attributes.csv)

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "omschrijving") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

3. Change name from "paleogeografie[year]_PolygonToRaster.tif" to "paleogeografie[year]_25m.tif"



#### FINAL FILE ####
Name:			paleogeografie[year]_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		9000 v.Chr. - 2000 n.Chr
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		Paleogeographic zones of the Netherlands over time. 13 Maps from 9000 B.C. - 2000 A.D.
			See categories/classes in attribute tables (paleogeografie[year]_25m_attributes.csv)

			* For 1250, 1500 and 1850 n.C maps, no occurances of "Stedelijk gebied" class, so "stedelijk gebied" was reclassified to
			"Bedijkte kwelders en riviervlakten", because almost all cities were on on riviervlakten (near rivers)

Language:		Dutch