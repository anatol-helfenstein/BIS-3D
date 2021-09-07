#### Readme for landscape type ####
By A.F. Helfenstein
2020-03-26
updated 2021-01-07


#### SOURCE FILE ####
Name:			Landschapstypologie (hlt500).lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Landschap)
Format: 		ESRI format LayerFile
Type:			raster
Gridsize:		500m x 500m
Availability:
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content:		landscape types:
				0:
				98: Grote wateren, zoet water
				99: stedelijk gebied HISTLAND
				110: Veenkolonien, Fries-Gronings type
				120: Veenkolonien, West-Brabantstype
				130: Veenkolonien, Utrechts-Gelders type
				140: Veenkolonien, Fries type
				210: Laagveengebied, Hollandveenontginning
				211: Laagveengebied, Hollandveenontginning
				212: Laagveengebied, Hollandveenontginning
				220: Laagveengebied, Hollandveenontginning
				230: Laagveengebied, Hollandveenontginning
				240: Laagveengebied, Hollandveenontginning
				250: Laagveengebied, Hollandveenontginning
				260: Zandgebied, Hollandveenontginning
				270: Laagveengebied, Hollandveenontginning
				310: Rivierengebied, stroomrugontginningen
				311: Rivierengebied, stroomrugontginningen
				320: Rivierengebied, komontginningen
				330: Rivierengebied, uiterwaarden
				340: Rivierengebied, rivierdeltaontginningen
				350: Rivierengebied, riviervlakteontginningen
				410: Rivierengebied, hogere terrasontginningen
				420: Rivierengebied, lagere terrasontginningen
				510: Zeekleigebied, kwelderwalontginningen 
				520: Zeekleigebied, knikkleiontginningen
				530: Zeekleigebied, kwelderwal- en knikkleiontginningen
				540: Zeekleigebied, kreekrug- en poelontginningen
				610: Zeekleigebied, nieuwlandpolders
				611: Zeekleigebied, landaanwinningswerken
				622: Zeekleigebied, grienden
				700: Droogmakerijen, oud onbepaald
				710: Droogmakerijen, oud natuurlijke meren
				720: Droogmakerijen, oud veenplassen
				740: Droogmakerijen, Zuiderzeepolders
				741: Droogmakerijen, Zuiderzeepolders bos
				810: Kustzone, strandwalontginningen
				812: Kustzone, strandwalontginningen afgevlakt
				820: Kustzone, strandvlakteontginningen
				830: Kustzone, duinbebossingen
				910: Heuvelland, lossontginningen
				911: Heuvelland, oud bos
				1010: Laag-middelhoog zandgebied, kampontginningen
				1030: Laag-middelhoog zandgebied, esontginningen
				1040: Laag-middelhoog zandgebied, beekdalontginningen
				1120: Laag-middelhoog zandgebied, natte heide- en veenontginningen
				1130: Laag-middelhoog zandgebied, droge en natte heideontginningen
				1131: Laag-middelhoog zandgebied, heidebossingen
				1140: Laag-middelhoog zandgebied, oude bossen
				1150: Laag-middelhoog zandgebied, zandverstuivingen
				1210: Kustzone, duinen en strand
				1220: Laag-middelhoog zandgebied, natte heide en veen
				1230: Laag-middelhoog zandgebied, droge en hatte heide
				1260: Grote wateren, kwelders
				1261: Grotewateren, vooroevers
				1300: Noordzee
				1310: Waddenzee, Eems, Dollard
				1350: Wester- en Oosterschelde
				1500: overig zoet water
				1600: platen
				11010: Hoog zandgebied, kampontginningen
				11040: Hoog zandgebied, beekdalontginningen
				11120: Hoog zandgebied, natte heide- en veenontginningen
				11130: Hoog zandgebied, droge en natte heidetginningen
				11131: Hoog zandgebied, heidebebossingen
				11140: Hoog zandgebied, oud bos
				11150: Hoog zandgebied, zandverstuivingen
				11220: Hoog zandgebied, natte heide en veen
				11230: Hoog zandgebied, droge en hatte heide
				110030: Hoog zandgebied, esontginningen


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Export raster as GeoTiff (Variables = "Value" & "Omschrijvi"), resampling to new resolution
2. Fill in missing values (nodata) using QGIS "Fill NoData cells" raster function. Set values = 1.
GDAL "fill nodata" function did not work because was unable to keep the same number of classes without immensely increasing...



#### FINAL FILE ####
Name:			landschapstypologie_hlt_filled_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\geology
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content:		landscape types:
				0: unknown
				1: unknown (filled in value, previously NA)
				98: Grote wateren, zoet water
				99: stedelijk gebied HISTLAND
				110: Veenkolonien, Fries-Gronings type
				120: Veenkolonien, West-Brabantstype
				130: Veenkolonien, Utrechts-Gelders type
				140: Veenkolonien, Fries type
				210: Laagveengebied, Hollandveenontginning
				211: Laagveengebied, Hollandveenontginning
				212: Laagveengebied, Hollandveenontginning
				220: Laagveengebied, Hollandveenontginning
				230: Laagveengebied, Hollandveenontginning
				240: Laagveengebied, Hollandveenontginning
				250: Laagveengebied, Hollandveenontginning
				260: Zandgebied, Hollandveenontginning
				270: Laagveengebied, Hollandveenontginning
				310: Rivierengebied, stroomrugontginningen
				311: Rivierengebied, stroomrugontginningen
				320: Rivierengebied, komontginningen
				330: Rivierengebied, uiterwaarden
				340: Rivierengebied, rivierdeltaontginningen
				350: Rivierengebied, riviervlakteontginningen
				410: Rivierengebied, hogere terrasontginningen
				420: Rivierengebied, lagere terrasontginningen
				510: Zeekleigebied, kwelderwalontginningen 
				520: Zeekleigebied, knikkleiontginningen
				530: Zeekleigebied, kwelderwal- en knikkleiontginningen
				540: Zeekleigebied, kreekrug- en poelontginningen
				610: Zeekleigebied, nieuwlandpolders
				611: Zeekleigebied, landaanwinningswerken
				622: Zeekleigebied, grienden
				700: Droogmakerijen, oud onbepaald
				710: Droogmakerijen, oud natuurlijke meren
				720: Droogmakerijen, oud veenplassen
				740: Droogmakerijen, Zuiderzeepolders
				741: Droogmakerijen, Zuiderzeepolders bos
				810: Kustzone, strandwalontginningen
				812: Kustzone, strandwalontginningen afgevlakt
				820: Kustzone, strandvlakteontginningen
				830: Kustzone, duinbebossingen
				910: Heuvelland, lossontginningen
				911: Heuvelland, oud bos
				1010: Laag-middelhoog zandgebied, kampontginningen
				1030: Laag-middelhoog zandgebied, esontginningen
				1040: Laag-middelhoog zandgebied, beekdalontginningen
				1120: Laag-middelhoog zandgebied, natte heide- en veenontginningen
				1130: Laag-middelhoog zandgebied, droge en natte heideontginningen
				1131: Laag-middelhoog zandgebied, heidebossingen
				1140: Laag-middelhoog zandgebied, oude bossen
				1150: Laag-middelhoog zandgebied, zandverstuivingen
				1210: Kustzone, duinen en strand
				1220: Laag-middelhoog zandgebied, natte heide en veen
				1230: Laag-middelhoog zandgebied, droge en hatte heide
				1260: Grote wateren, kwelders
				1261: Grotewateren, vooroevers
				1300: Noordzee
				1310: Waddenzee, Eems, Dollard
				1350: Wester- en Oosterschelde
				1500: overig zoet water
				1600: platen
				11010: Hoog zandgebied, kampontginningen
				11040: Hoog zandgebied, beekdalontginningen
				11120: Hoog zandgebied, natte heide- en veenontginningen
				11130: Hoog zandgebied, droge en natte heidetginningen
				11131: Hoog zandgebied, heidebebossingen
				11140: Hoog zandgebied, oud bos
				11150: Hoog zandgebied, zandverstuivingen
				11220: Hoog zandgebied, natte heide en veen
				11230: Hoog zandgebied, droge en hatte heide
				110030: Hoog zandgebied, esontginningen


Language:		Dutch