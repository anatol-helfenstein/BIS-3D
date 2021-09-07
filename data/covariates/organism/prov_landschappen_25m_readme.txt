#### Readme for provincial landscapes (e.g. Eigendommen_van_de_stichting_[province]_Landschap_1999, Provinciale_landschappen_terreinen_[year]) ####
By A.F. Helfenstein
2020-03-17


#### SOURCE FILE ####
Name:		Eigendommen_van_de_stichting_Het_Brabants_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Gelders_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Groninger_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Limburgs_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Overijssels_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Utrechts_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Zeeuws_Landschap_1999.lyr
		Eigendommen_van_de_stichting_Het_Zuid-Hollands_Landschap_1999.lyr
		Eigendommen_van_het_Drents_Landschap_1999.lyr
		Eigendommen_van_It_Fryske_Gea_1999.lyr
		Provinciale_landschappen_terreinen_2005.lyr
		Provinciale_Landschappen_terreinen_2006.lyr
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\NatuurTerrein\ProvincialeLandschappen)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:	
Spatial 
coverage:	National
Projection:	RD_new
Content (NE):

Content (EN):	- areas with provincial landscapes in 1999, 2005 and 2006 (in polygons)

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Union" geoprocessing tool to aggregate nature areas of all years (see files above) together since there were only very small differences between years. New layerfile (polygon) called "landschapp_union".
* problem with "Provinciale_Landschappen_terreinen_2006.lyr" file: when using any geoprocessing tool, output is empty (blank)

2. Add new field that we name "landschapp" in attribute table of "landschapp_union". Use field calculator with python code "str("provinciale_landschapp")" to add a common attribute (string text) to all objects.

3. Polygon to raster (target attribute = "landschapp") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "landschapp_union_PolygonToRaster1" to "prov_landschappen_25m"




#### FINAL FILE ####
Name:		prov_landschappen_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	- provincial landscapes in 2005:
					- 0
					- 1
* Note: can aggregate all these categories into binary (prov. landscape / not prov. landscape) in R



Language:	English