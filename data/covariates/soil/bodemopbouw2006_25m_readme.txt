#### Readme for soil structure map (bodemopbouw 2006) ####
By A.F. Helfenstein
2020-04-03


#### SOURCE FILE ####
Name:		Bodemopbouw_pawn_2006.lyr
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:		
Temporal
coverage:	(2006 version)
Spatial 
coverage:	National / Netherlands
Projection:	RD_new
Content:	soil structure classes:
			0		
			1	=	Veraarde bovengrond op diep veen
			2	=	Veraarde bovengrond op veen op zand
			3	=	Kleidek op veen
			4	=	Kleidek op veen op zand
			5	=	Zanddek op veen op zand
			6	=	Veen op ongerijpte klei
			7	=	Stuifzand
			8	=	Leemarm zand
			9	=	Zwaklemig fijn zand
			10	=	Zwaklemig fijn zand op grof zand
			11	=	Sterk lemig fijn zand op (kei-)leem
			12	=	Enkeerdgronden
			13	=	Sterk lemig fijn zand
			14	=	Grof zand
			15	=	Zavel met homogeen profiel
			16	=	Licht klei met homogeen profiel
			17	=	Klei met zware tussenlaag of ondergrond
			18	=	Klei op veen
			19	=	Klei op fijn zand
			20	=	Klei op grof zand
			21	=	Leem
			22	=	Water
			23	=	bebouwing, enz

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion). Attribute of interest: "BFYS_CODE" or "PAWN_CODE" or "BODEMOPBOU"
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)

3.	Rename from "Bodemopbouw_pawn_2006_PolygonToRaster" to "bodemopbouw2006_25m"



#### FINAL FILE ####
Name:		bodemopbouw2006_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\soil
Format: 	GeoTIFF
Type:		Raster
Gridsize:	25m x 25m
Spatial 
coverage:	National / Netherlands
Projection:	RD_new

Content:	soil structure classes:
			0		
			1	=	Veraarde bovengrond op diep veen
			2	=	Veraarde bovengrond op veen op zand
			3	=	Kleidek op veen
			4	=	Kleidek op veen op zand
			5	=	Zanddek op veen op zand
			6	=	Veen op ongerijpte klei
			7	=	Stuifzand
			8	=	Leemarm zand
			9	=	Zwaklemig fijn zand
			10	=	Zwaklemig fijn zand op grof zand
			11	=	Sterk lemig fijn zand op (kei-)leem
			12	=	Enkeerdgronden
			13	=	Sterk lemig fijn zand
			14	=	Grof zand
			15	=	Zavel met homogeen profiel
			16	=	Licht klei met homogeen profiel
			17	=	Klei met zware tussenlaag of ondergrond
			18	=	Klei op veen
			19	=	Klei op fijn zand
			20	=	Klei op grof zand
			21	=	Leem
			22	=	Water
			23	=	bebouwing, enz



Language:	Dutch