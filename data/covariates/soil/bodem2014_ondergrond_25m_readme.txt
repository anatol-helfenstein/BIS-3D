#### Readme for Netherlands special feature in the substrate / subsurface from the soil type map (Bodemkaart50000 Versie 2014) ####
By A.F. Helfenstein
2020-04-01


#### SOURCE FILE ####
Name:			Bodemkaart50000_v2014_grouped.lyr
				layer: "Bijzonderheden in de ondergrond"
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Bodem-50_2014)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:	
Temporal
Coverage:		2014
Spatial 
coverage:		Netherlands (not full national coverage)
Projection:		RD_new

Content:		1:	..p, pleistoceen zand beginnend tussen 40 en 120 cm
				2:	..t, (loess-)leem of gerijpte oude klei, anders dan keileem of potklei beginnend tussen 40 en 120cm, tenminste 20 cm dik
				3:	..x, keileem of potklei beginnend tussen 30 en 120 cm, tenminste 20 cm dik
				4:	..g, grof zand en of grind beginnend tussen 40 en 120 cm, tenminste 40 cm dik
				5:	..v, moerig materiaal beginnend dieper dan 80 cm en doorgaand tot dieper dan 120 cm
				6:	..m, oude rivierklei (zavel en klei) beginnend tussen 40 en 120 cm, tenminste 20 cm dik
				7:	..l, plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik
				8:	..r, meestal niet geheel gerijpt zavel en klei beginnend tussen 40 en 120 cm
				9:	..w, moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm
				10:	..lv, plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik en moerig materiaal beginnend dieper dan 80 cm
				11:	..lw, plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik en moerig materiaal tussen 40 en 80 cm
				12:	..lwp, Plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik en moerig materiaal tussen 40 en 80 cm op pleistoceen zand
				13:	..s, Vuursteeneluvium beginnend tussen 40 en 120 cm
				14:	..k, Kalksteen of kleefaarde beginnend tussen 40 en 120 cm
				15:	..wx, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm en keileem beginnend tussen 40 en 120 cm
				16:	..wp, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm op pleistoceen zand beginnend tussen 40 en 120 cm
				17:	..wg, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm en grof zand of grind beginnend tussen 40 en 120 cm
				18:	..wt, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm en (löss-)leem of gerijpte oude klei beginnend tussen 40 en 120 cm
				19:	..tg, (löss-)leem of gerijpte oude klei, anders dan keileem of potklei beginnend tussen 40 en 120 cm, tenminste 20 cm dik
				20:	..px, Pleistoceen zand en keileem of potklei beginnend tussen 40 en 120 cm
				21:	..gx, Grof zand of grind en keileem of potklei beginnend tussen 40 en 120 cm
				22:	..c, Spalterveen, tenminste 50 cm dik
				23:	..a, Glauconiethoudende klei beginnend tussen 40 en 120 cm

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "ACHTER1") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2.	Raster to GeoTIFF-format (conversion)

3. Change name from "Bodemtypen_PolygonToRaster1" to "bodem2014_ondergrond_25m"



#### FINAL FILE ####
Name:			bodem2014_ondergrond_25m.tif
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

Content:		1:	..p, pleistoceen zand beginnend tussen 40 en 120 cm
				2:	..t, (loess-)leem of gerijpte oude klei, anders dan keileem of potklei beginnend tussen 40 en 120cm, tenminste 20 cm dik
				3:	..x, keileem of potklei beginnend tussen 30 en 120 cm, tenminste 20 cm dik
				4:	..g, grof zand en of grind beginnend tussen 40 en 120 cm, tenminste 40 cm dik
				5:	..v, moerig materiaal beginnend dieper dan 80 cm en doorgaand tot dieper dan 120 cm
				6:	..m, oude rivierklei (zavel en klei) beginnend tussen 40 en 120 cm, tenminste 20 cm dik
				7:	..l, plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik
				8:	..r, meestal niet geheel gerijpt zavel en klei beginnend tussen 40 en 120 cm
				9:	..w, moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm
				10:	..lv, plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik en moerig materiaal beginnend dieper dan 80 cm
				11:	..lw, plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik en moerig materiaal tussen 40 en 80 cm
				12:	..lwp, Plaatselijk katteklei binnen 80 cm, tenminste 10 cm dik en moerig materiaal tussen 40 en 80 cm op pleistoceen zand
				13:	..s, Vuursteeneluvium beginnend tussen 40 en 120 cm
				14:	..k, Kalksteen of kleefaarde beginnend tussen 40 en 120 cm
				15:	..wx, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm en keileem beginnend tussen 40 en 120 cm
				16:	..wp, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm op pleistoceen zand beginnend tussen 40 en 120 cm
				17:	..wg, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm en grof zand of grind beginnend tussen 40 en 120 cm
				18:	..wt, Moerig materiaal, 15 a 40 cm dik en beginnend tussen 40 en 80 cm en (löss-)leem of gerijpte oude klei beginnend tussen 40 en 120 cm
				19:	..tg, (löss-)leem of gerijpte oude klei, anders dan keileem of potklei beginnend tussen 40 en 120 cm, tenminste 20 cm dik
				20:	..px, Pleistoceen zand en keileem of potklei beginnend tussen 40 en 120 cm
				21:	..gx, Grof zand of grind en keileem of potklei beginnend tussen 40 en 120 cm
				22:	..c, Spalterveen, tenminste 50 cm dik
				23:	..a, Glauconiethoudende klei beginnend tussen 40 en 120 cm

Language:		Dutch