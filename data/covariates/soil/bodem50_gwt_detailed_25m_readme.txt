#### Readme for (detailed) groundwater table in soil profile (?) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			bodem50_gwt_tekst.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Bod-Gwt-50)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		?
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- (detailed) groundwater table height in soil profile (?):
					1:	III =  H <40  L 80-120
					2:	VI =  H 40-80  L >120
					3:	VII* =  H >140  L >160
					4:	-
					5:	I =  H <40  L <50 
					6:	II =  H <40  L 50-80
					7:	VII =  H 80-140  L >120
					8:	V =  H <40  L >120
					9:	V* =  H 25-40  L >120
					10:	III* =  H 25-40  L 80-120 
					11:	IV =   H >40  L 80-120
					12:	II* =  H 25-40  L 50-80
					13:	IIIb =  H 25-40  L 80-120
					14:	Vb =  H 25-40  L >120
					15:	VIII =  H >140  L >160
					16:	sVII =  H 80-140  L >120  schijnspiegels 
					17:	IVu =  H 40-80  L 80-120
					18:	IIIa =  H <25  L 80-120
					19:	sVa =  H <25  L >120  schijnspiegels
					20:	Va =  H <25  L >120
					21:	IIb =  H 25-40  L 50-80
					22:	sV =  H <40  L >120  schijnspiegels
					23:	sVb =  H 25-40  L >120  schijnspiegels
					24:
					25:	bVII =  H 80-140  L >120  buitendijks
					26:	bVI =  H 40-80  L >120  buitendijks

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "EERSTE_GWT") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to geoTIFF-format (conversion)

3. Change name from "bodem50_eerste_gwt_PolygonToRaster" to "bodem50_gwt_detailed_25m"



#### FINAL FILE ####
Name:			bodem50_gwt_detailed_25m.tif
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

Content:		- (detailed) groundwater table height in soil profile (?):
					1:	III =  H <40  L 80-120
					2:	VI =  H 40-80  L >120
					3:	VII* =  H >140  L >160
					4:	-
					5:	I =  H <40  L <50 
					6:	II =  H <40  L 50-80
					7:	VII =  H 80-140  L >120
					8:	V =  H <40  L >120
					9:	V* =  H 25-40  L >120
					10:	III* =  H 25-40  L 80-120 
					11:	IV =   H >40  L 80-120
					12:	II* =  H 25-40  L 50-80
					13:	IIIb =  H 25-40  L 80-120
					14:	Vb =  H 25-40  L >120
					15:	VIII =  H >140  L >160
					16:	sVII =  H 80-140  L >120  schijnspiegels 
					17:	IVu =  H 40-80  L 80-120
					18:	IIIa =  H <25  L 80-120
					19:	sVa =  H <25  L >120  schijnspiegels
					20:	Va =  H <25  L >120
					21:	IIb =  H 25-40  L 50-80
					22:	sV =  H <40  L >120  schijnspiegels
					23:	sVb =  H 25-40  L >120  schijnspiegels
					24:
					25:	bVII =  H 80-140  L >120  buitendijks
					26:	bVI =  H 40-80  L >120  buitendijks

Language:		Dutch