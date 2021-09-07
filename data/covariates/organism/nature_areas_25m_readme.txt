#### Readme for nature areas data from 1999, 2001, 2003-2005, 2009, 2010, 2011, 2013 (e.g. Terreinen Natuurmonumenten [year]) ####
By A.F. Helfenstein
2020-03-17


#### ORIGINAL ####
Names:		Eigendommen_van_de_Vereniging_Natuurmonumenten_1999.lyr
		Terreinen_Natuurmonumenten_1999.lyr
		Terreinen_Natuurmonumenten_2001.lyr
		Terreinen_Natuurmonumenten_2003.lyr
		Terreinen_Natuurmonumenten_2004.lyr
		Terreinen_Natuurmonumenten_2005.lyr
		Terreinen_Natuurmonumenten_feb2009.lyr
		Terreinen_Natuurmonumenten_jan2010.lyr
		Terreinen_Natuurmonumenten_jan2011.lyr
		Terreinen_Natuurmonumenten_jan2013.lyr
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\NatuurTerrein\NatuurMonumenten
Format: 	ESRI layerfiles (.lyr)
Type:		polygon
Gridsize:	
Availability:	?
Temporal 
coverage:	1999 - 2013 (not of every year)
Spatial 
coverage:	Netherlands
Projection:	RD_new

Content (EN):	Essentially, we are only interested in whether it is a nature area or not.
		Therefore, none of the variables in the attribute table are of interest.

Language:	Dutch & English



#### PROCESSING ####
1. Use the ArcGIS "Union" geoprocessing tool to aggregate nature areas of all years (see files above) together since there were only very small differences between years. New layerfile (polygon) called "nm_union".

2. Add new field that we name "NM" in attribute table of "nm_union". Use field calculator with python code "str("nature_areas")" to add a common attribute (string text) to all objects.

3. Polygon to raster (target attribute = "NM") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "nm_union_PolygonToRaster" to "nature_areas_25m"



#### FINAL FILE ####
Name:		nature_areas_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	geoTiff (.tif)
Type:		raster
Gridsize:	
Temporal 
coverage:	1999 - 2013 (not of every year)
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (EN):	nature areas (1)

Language:	English