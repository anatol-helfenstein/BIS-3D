#### Readme for unique soil parent material from soil map (Bodemkaart50000 Versie 2014) ####
By A.F. Helfenstein
2020-04-01


#### SOURCE FILE ####
Name:			Bodemkaart50000_v2014_grouped.lyr
				layer: bijzonderheden in de bovengrond
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Bodem-50_2014)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		2014
Spatial 
coverage:		Netherlands (not full national coverage)
Projection:		RD_new

Content:		- soil parent material unique areas:
					1: b. kruinige percelen
					2: d. plaatselijk verdrogende lagen in de bovengrond
					3: e. zoete getijdenafzettingen
					4: f. plaatselijk ijzerrijk, binnen 50 cm beginnend en tenminste 10 cm dik
					5: fk. plaatselijk ijzerrijk, binnen 50 cm beginnend en tenminste 10 cm dik en een zavel- of kleidek
					6: g. grind in de bovengrond, 15 a 40 cm dik
					7: h. kolenslik in de bovengrond, 15 a 40 cm dik
					8: k. zavel- of klei-dek, 15 a 40 cm dik
					10: l. loessdek, 15 a 40 cm dik
					11: m. stenen in de bovengrond
					12: z. zanddek, 15 a 40 cm dik
					13: n. plaatselijk zout
					14: o. opgebracht zandig moerig dek (toemaakdek)
					15: nk. plaatselijk zout en een zavel- of klei-dek, 15 a 40 cm dik
					16: s. zanddekje, 5 a 15 cm dik
					17: u. kleiig uiterst fijn silt- of zanddek, 15 a 40 cm dik
					18: zg. zanddek, 15 a 40 cm dik en grind in de bovengrond, ondieper dan 40 cm beginnend

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "VOOR1") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to GeoTIFF-format (conversion)

3. Change name from "Bijzonderhedenindebovengrond_PolygonToRaster" to "bodem2014_bovengrond_25m"



#### FINAL FILE ####
Name:			bodem2014_bovengrond_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
Coverage:		2014
Spatial 
coverage:		Netherlands (not full national coverage)
Projection:		RD_new

Content:		- soil parent material unique areas:
					1: b. kruinige percelen
					2: d. plaatselijk verdrogende lagen in de bovengrond
					3: e. zoete getijdenafzettingen
					4: f. plaatselijk ijzerrijk, binnen 50 cm beginnend en tenminste 10 cm dik
					5: fk. plaatselijk ijzerrijk, binnen 50 cm beginnend en tenminste 10 cm dik en een zavel- of kleidek
					6: g. grind in de bovengrond, 15 a 40 cm dik
					7: h. kolenslik in de bovengrond, 15 a 40 cm dik
					8: k. zavel- of klei-dek, 15 a 40 cm dik
					10: l. loessdek, 15 a 40 cm dik
					11: m. stenen in de bovengrond
					12: z. zanddek, 15 a 40 cm dik
					13: n. plaatselijk zout
					14: o. opgebracht zandig moerig dek (toemaakdek)
					15: nk. plaatselijk zout en een zavel- of klei-dek, 15 a 40 cm dik
					16: s. zanddekje, 5 a 15 cm dik
					17: u. kleiig uiterst fijn silt- of zanddek, 15 a 40 cm dik
					18: zg. zanddek, 15 a 40 cm dik en grind in de bovengrond, ondieper dan 40 cm beginnend

Language:		Dutch