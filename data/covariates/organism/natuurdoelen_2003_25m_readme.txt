#### Readme for "nature" types ####
By A.F. Helfenstein
2020-03-31


#### SOURCE FILE ####
Name:			Landelijke_Natuurdoelenkaart_dec2003.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Policy_Beleid\Natuur\Landelijke NDT kaart)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		2003
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		nature types categorized in the following groups:
					1: bloemrijk grasland
					2: multifunctioneel bos
					3: overig stromend en stilstaand water
					4: nat schraalland
					5: nat, matig voedselrijk grasland
					6: multifunctioneel grasland
					7: bos van arme gronden
					8: bos van laagveen en klei
					9: reservaatsakker
					10: droog schraalgrasland
					11: beek
					12: duinlandschap
					13: zoekgebied
					14: zilt grasland
					15: moeras
					16: bos van rijke gronden
					17: natte heide en ihoogveen
					18: overige natuur
					19: brak water
					20: ven en duinplas
					21: middenbos, hakhout en griend
					22: droge heide
					23: veen- en zeekleilandschap
					24: zandverstuiving
					25: bos van bron en beek
					26: beek- en zandboslandschap
					27: discussie
					28: getijdengebied en zee
					29: begeleid getijdengebied
					31: rivierenlandschap
					32: meer
					33: multifunctionele grote wateren
					35: kalkgrasland



Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "NDKLEUR") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "LandelijkeNatuurdoelenkaartdec_PolygonToRaster" to "natuurdoelen_2003_25m"



#### FINAL FILE ####
Name:			natuurdoelen_2003_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content:		nature types categorized in the following groups:
					1: bloemrijk grasland
					2: multifunctioneel bos
					3: overig stromend en stilstaand water
					4: nat schraalland
					5: nat, matig voedselrijk grasland
					6: multifunctioneel grasland
					7: bos van arme gronden
					8: bos van laagveen en klei
					9: reservaatsakker
					10: droog schraalgrasland
					11: beek
					12: duinlandschap
					13: zoekgebied
					14: zilt grasland
					15: moeras
					16: bos van rijke gronden
					17: natte heide en ihoogveen
					18: overige natuur
					19: brak water
					20: ven en duinplas
					21: middenbos, hakhout en griend
					22: droge heide
					23: veen- en zeekleilandschap
					24: zandverstuiving
					25: bos van bron en beek
					26: beek- en zandboslandschap
					27: discussie
					28: getijdengebied en zee
					29: begeleid getijdengebied
					31: rivierenlandschap
					32: meer
					33: multifunctionele grote wateren
					35: kalkgrasland


Language:		Dutch