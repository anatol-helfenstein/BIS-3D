#### Readme for state forests (e.g. Staatsbosbeheer_[year]_Terreinen.lyr) ####
By A.F. Helfenstein
2020-03-17


#### SOURCE FILE ####
Name:		Staatsbosbeheer_eigendommen_1999.lyr
			Staatsbosbeheer_2001_terreinen.lyr
			Staatsbosbeheer 2004 Terreinen
			Staatsbosbeheer_2005_Terreinen.lyr
			Staatsbosbeheer_2006_terreinen
			Staatsbosbeheer2007 terreinen
			Staatsbosbeheer_2008_terreinenInbeheer.lyr
			Staatsbosbeheer_2009_terreinenInbeheer.lyr
			Staatsbosbeheer 2010 SBB Beheer
			ADMIN.TER_BEHEER_2015
			Staatsbosbeheer20190101
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\NatuurTerrein\StaatsBosBeheer)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:
Temporal
coverage:	1999, 2001, 2004-2009, 2010, 2015, 2019	
Spatial 
coverage:	National
Projection:	RD_new
Content (NE):

Content (EN):	- areas with state forest management (in polygons)

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Union" geoprocessing tool to aggregate state forest areas of all years (see files above) together. New layerfile (polygon) called "bos_terreinen_union".

2. Add new field that we name "staatsbosbeheer_terreinen" in attribute table of "bos_terreinen_union". Use field calculator with python code "str("staatsbosbeheer_terreinen")" to add a common attribute (string text) to all objects.

3. Polygon to raster (target attribute = "staatsbosbeheer_terreinen") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "bos_terreinen_union_PolygonToRaster" to "bos_terreinen_25m"



#### FINAL FILE ####
Name:		bos_terreinen_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Temporal
coverage:	1999, 2001, 2004-2009, 2010, 2015, 2019	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	state forest area (1) / not state forest area (binary)



Language:	English