#### Readme for forest areas in the Netherlands ####
By A.F. Helfenstein
2020-03-26


#### SOURCE FILE ####
Name:			bosstat4_geo2000_topo.lyr
			bosstat4_cover_export.lyr
			bos2000_geo2000_topo.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Bos)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2000 (?)
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):		areas with forest: binary (forest / no forest)

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Union" geoprocessing tool to aggregate state forest areas of all 3 layers (see files above) together. New layerfile (polygon) called "bosstat4_geo2000_topo_Union".

2. Add new field that we name "bos" in attribute table of "bosstat4_geo2000_topo_Union". Use field calculator with python code "str("bos")" to add a common attribute (string text) to all objects.

3. Polygon to raster (target attribute = "bos") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "bosstat4_geo2000_topo_Union_PolygonToRaster" to "bosstat4_2000_25m"



#### FINAL FILE ####
Name:			bosstat4_2000_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		2000 (?)
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):		areas with forest: binary (forest / no forest)


Language:		Dutch