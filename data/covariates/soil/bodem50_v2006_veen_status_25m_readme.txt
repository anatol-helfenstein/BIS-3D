#### Readme for peat status from soil map version 2006 ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			Status_Veen_2006.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Bod-Gwt-50_2006)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		2006
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		peat status of peat soils in the Netherlands:
					1:	Minerale bodem
					2:	Overig
					3:	Geen informatie over status veengrond
					4:	Onzeker over status moerige grond
					5:	Wel veengrond (Veenkartering, 2004)
					6:	Gedeformeerde veengrond (Veenkartering, 2004)
					7:	Vrij zeker over status moerige grond

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "ACT_VEENM") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to geoTIFF-format (conversion)

3. Change name from "Status_Veen_2006_PolygonToRaster" to "bodem50_v2006_veen_status_25m"



#### FINAL FILE ####
Name:			bodem50_v2006_veen_status_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
Coverage:		2006
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		peat status of peat soils in the Netherlands:
					1:	Minerale bodem
					2:	Overig
					3:	Geen informatie over status veengrond
					4:	Onzeker over status moerige grond
					5:	Wel veengrond (Veenkartering, 2004)
					6:	Gedeformeerde veengrond (Veenkartering, 2004)
					7:	Vrij zeker over status moerige grond

Language:		Dutch