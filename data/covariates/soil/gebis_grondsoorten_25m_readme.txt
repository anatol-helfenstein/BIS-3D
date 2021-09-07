#### Readme for soil texture in areas with nature goals (naturdoelen GEBIS) ####
By A.F. Helfenstein
2020-03-31


#### SOURCE FILE ####
Name:			GEBIS2003_beheersgebieden.lyr
				GEBIS2003_landschap.lyr
				GEBIS2003_natuur.lyr
				GEBIS2004_beheersgebieden.lyr
				GEBIS2004_landschap.lyr
				GEBIS2004_natuur.lyr
				GEBIS2005_beheersgebieden.lyr
				GEBIS2005_landschap.lyr
				GEBIS2005_natuur.lyr
				GEBIS2006_beheersgebieden.lyr
				GEBIS2006_landschap.lyr
				GEBIS2006_natuur.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\Natuur\RIS_GEBIS_QUBIS\GEBIS)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2003 - 2006
Spatial 
coverage:		National
Projection:		RD_new

Content:		- GEBIS areas and soil texture type (grondsoorten).
				- attribute of interest "GRD_SOORT"
					- blank
					- "-"
					- zand
					- veen
					- zeeklei
					- klei

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Union" geoprocessing tool to aggregate all GEBIS areas of years 2003 - 2006 (see files above) together. New layerfile (polygon) called "GEBIS2003_2006_union".

2. Add new field that we name "grondsoorten" in attribute table of "GEBIS2003_2006_union". Use field calculator with python code to concatenate all "GRD_SOORTEN" fields in table together in new "grondsoorten" field. This is not the best solution but works for now (aggregate categoris in R later). Save this layer as shapefile feature under \BISplus_GIS\Preprocessing.
VERY CHAOTIC, needs to be aggregated in R...

3. Polygon to raster (target attribute = "grondsoorten") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "GEBIS2003_2006_union_PolygonToRaster" to "gebis_grondsoorten_25m"



#### FINAL FILE ####
Name:			gebis_grondsoorten_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\soil
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		2003 - 2006
Spatial 
coverage:		National
Projection:		RD_new

Content:		- GEBIS areas and soil texture type (grondsoorten):
					- blank
					- "-"
					- zand
					- veen
					- zeeklei
					- klei
					- OR ANY COMBINATION OF THESE CATEGORIES (MOSTLY REPETITIONS)
						- e.g. "Zand zand - zand", which can be aggregated in R to "sand"
						***IMPORTANT: further processing (aggregating categories necessary), because currently there are over 2000 categories


Language:	Dutch