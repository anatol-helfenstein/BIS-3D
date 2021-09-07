#### Readme for state forests categorized by nature aims / use (e.g. Staatsbosbeheer_[year]_natuur_doelstellingen.lyr) ####
By A.F. Helfenstein
2020-03-18


#### SOURCE FILE ####
Name:		Staatsbosbeheer 2005 natuur doelstellingen.lyr
			Staatsbosbeheer 2006 natuur doelstellingen.lyr
			Staatsbosbeheer 2007 natuurve doelstellingen.lyr
			Staatsbosbeheer_2008_natuur_doelstellingen.lyr
			Staatsbosbeheer_2009_natuur_doelstellingen.lyr
			Staatsbosbeheer 2010 natuur.lyr
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\NatuurTerrein\StaatsBosBeheer)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:
Temporal
coverage:	2005-2010
Spatial 
coverage:	National
Projection:	RD_new
Content (NE):

Content (EN):	- areas with state forest management (in polygons) categorized by natural aims/use
				- variables of interest in attribute tables: "SPE_CODE_T" or "SDT_CODE" or "BT" and "SDT_OMS" or "BT_OMS"

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Merge" geoprocessing tool to merge common attributes of interest. First merge "SPE_CODE_T" attributes from layerfiles of years 2005 and 2007.

2. In new merged layer named "bos_natuur_05_07", rename "SPE_CODE_T" to "SDT_CODE" (by creating new field and using the field calculator to assign all rows the values of variable "SPE_CODE_T")

3. Merge "bos_natuur_05_07" with layerfiles from years (2006, 2008-2009) by attribute "SDT_CODE".

4. Union new file from preceding step (e.g. "bos_natuur_merge") with remaining year (2010) by attribute "BT". We use union instead of merge because the code sytem in the 2010 layerfile is different (e.g. "L03.01"). This results that about 4269 polygons out of 770'953 remain without description. Can be aggregated to others. According to the map, these are small polygons...

5. Polygon to raster (target attribute = "SDT_CODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

6. Raster to other format (.tif)

7. Change name from "bos_natuur_merge_union_PolygonToRaster" to "bos_natuur_25m"



#### FINAL FILE ####
Name:		bos_natuur_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Temporal
coverage:	2005-2010	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	Areas with state forest management (in polygons) categorized by natural aims/use ("SDT_CODE") and vegetation / forest type

Doeltypen:
					1. nagenoeg natuurlijke eenheid
					2. begeleid natuurlijke eenheid
					3. bosgemeenschap
					4. bosvervangingsgemeenschap
					5. natte heide, hoogveen en natte duinvalleien
					6. droge heide en duinen
					7. verlandingsvegetaties
					8. moerashooiland
					9. bloemrijk grasland en droog schraalgrasland
					10. vochtig schraalgrasland
					11. open water met functie natuur
					12. kleinschalig begeleid natuurlijke eenheid3
					13. multifunctioneel bos
					14. rietland
					15. weidevogel grasland
					16. akker
					17. open water multifunctioneel
					18. landschapselementen met natuurfunctie
					19. korte vegetaties
					20. overige landschappelijke elementen
					99. niet doelstellingsgericht
					overige waarden

Subdoeltypen:
					1.1	Kwelder
					1.2	Zee
					1.3	Stuivend duin
					2.3	Moeras
					2.4	Afgesloten zoete zeearmen
					2.5	Iepen-Essenwoud
					2.7	Dynamisch duinlandschap
					3.1	Dennenbossen op kalkarme (land)duinen
					3.1	Broekbossen op laagveen
					3.2	Broekbossen op zure venen
					3.3	Loofbossen op arme zandgronden
					3.4	Loofbossen op lemige zandgronden
					3.5	Loofbossen op kalkrijke (zee)duinen
					3.6	Loofbossen op lemen en kalkrijke zandgronden
					3.7	Loofbossen op kalkrijke bodems
					3.8	Loofbossen op klei- en zavelgronden
					3.9	Beekbegeleidende loofbossen
					4.1	Eiken-hakhout
					4.2	Grienden & essen/elzenhakhout
					4.3	Middenbos
					4.4	Park- en stinse bos
					4.5	Overige Natuurbossen
					5.1	Natte duinvalleien
					5.2	Hoogveen
					5.3	Hoogveenvennen
					5.4	Natte heide
					6.1	Open duin
					6.2	Duinstruweel
					6.3	Droge, open heide
					6.4	Stuifzanden
					6.5	Heide met struweel en bos
					6.6	Overige heiden
					7.1	Jonge verlanding
					7.2	Oude riet ruigten
					7.3	Brakwater-(verlanding)
					8.1	Veenmosrietlanden en trilvenen
					8.2	Natte schraallanden
					9.1	Glanshaverhooiland
					9.2	Kamgrasweiden en zilverschoongraslanden
					9.3	Kalkgraslanden
					9.4	Zilte graslanden
					9.5	Droge schraallanden
					9.6	Overige bloemrijke graslanden
					10.1	Vochtig schraal grasland
					10.2	Veenweide
					11.1	Zoet watergemeenschappen in laagveen en kleigebieden
					11.2	Vennen en plassen op zand, zwak gebufferd
					11.3	Beken en rellen
					12.1	Complex van ooibos, pionier- en watervegetaties in uiterwaarden
					12.2	Complex van bos, rietruigten, gras en water op laagveen en klei
					12.4	Complex van bos, ruigten, gras op kalkrijke gronden
					12.5	Complex van bos, ruigten en water op zandgronden
					13.1	Grove dennen-Berkenbos op zuur, arm zand
					13.1	Eiken-Haagbeukenbos op natte lemige gronden
					13.11	Essen-Iepenbos op vochtige klei en zavel
					13.12	Essen-Iepenbos met exoten op vochtige klei en zavel
					13.13	Elzen-Wilgenbos op nat veen en klei met cultuurinvloed
					13.14	Essenbos met exoten op nat veen en klei met cultuurinvloed
					13.15	Elzenbos op zeer nat veen met cultuurinvloed
					13.2	Grove dennen-Eikenbos op droog, leemarm zand
					13.3	Grove dennen-Eikenbos op vochtig tot nat, leemarm zand
					13.4	Grove dennen-Eikenbos op zand met cultuur-invloed
					13.5	Grove dennen-Eikenbos met exoten op zand met cultuur-invloed
					13.6	Wintereiken-Beukenbos op leemhoudend zand
					13.7	Wintereiken-Beukenbos met exoten op leemhoudend zand
					13.8	Vochtig Wintereiken-Beukenbos op leemhoudend zand
					13.9	Vochtig Wintereiken-Beukenbos met exoten op leemhoudend zand
					14.1	Rietcultuur
					15.1	Weidevogelgrasland
					15.2	Wintergastenweide
					16.1	Akker
					17.1	Overig water
					18.1	Houtwallen, brede singels en graften
					18.2	Bloemdijken
					19.2	Overige korte vegetaties
					20.1	Knotwilgen en heggen
					20.1	Hoogstamboomgaarden
					20.2	Landschappelijke beplantingen en overige bossen
					20.3	Dijken
					20.4	Demonstratie kooien
					20.5	Rustende eendekooien
					20.6	Forten
					20.7	Historische gebouwen
					20.8	Historische tuinen
					20.9	Lanen en singels
					99.1	Erven en ondergrond gebouwen in eigen beheer
					99.11	Beheer uitbesteed aan andere overheden
					99.12	Verpachte erven en ondergrond van gebouwen
					99.2	Ondergrond hunebed e.d.
					99.3	Wegen, paden en parkeerterreinen
					99.4	Recreatieweide
					99.5	Zandwinputten
					99.6	Vuilstorten en vervuilde gronden
					99.7	In erfpacht gegeven aan Natuurmonumenten
					99.8	In erfpacht gegeven aan Provinciale Landschappen
					99.9	In erfpacht gegeven aan Recreatieschappen



Language:	English