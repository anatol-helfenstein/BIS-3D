#### Readme for drought prone areas (?) ####
By A.F. Helfenstein
2020-03-27


#### SOURCE FILE ####
Name:			Verdroging_TopLijst_2009-05.lyr
			Gebiedenatlas_2003_verdrogingskaart_2000.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Milieu)
Format: 		ESRI format LayerFile
Type:			polygon
Gridsize:		
Availability:
Temporal
coverage:		2000, 2003, 2009 (?)
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		drought prone areas during droughts in 2000, 2003 and 2009
				binary (drought prone / not drought prone)
				* Note: did not find out how these areas were determined (linked with nature reserves?)


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Union" geoprocessing tool to aggregate drought prone areas of all years (see files above) together. New layerfile (polygon) called "TopLijstVerdroging200905_Uni".

2. Add new field that we name "verdroging" in attribute table of "TopLijstVerdroging200905_Uni". Use field calculator with python code "str("verdroging")" to add a common attribute (string text) to all objects.

3. Polygon to raster (target attribute = "verdroging") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "TopLijstVerdroging200905_Uni_PolygonToRaster" to "verdroging_25m"



#### FINAL FILE ####
Name:			verdroging_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		drought prone areas during droughts in 2000, 2003 and 2009
				binary (drought prone / not drought prone)
				* Note: did not find out how these areas were determined (linked with nature reserves?)


Language:		Dutch