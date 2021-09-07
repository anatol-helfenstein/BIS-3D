#### Readme for land use map (LGN7) ####
By A.F. Helfenstein
2020-03-23


#### ORIGINAL ####
Name:			LGN7.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-7
Format: 		ESRI layer file (.lyr)
Type:			raster
Gridsize:		25m x 25m
Availability:	?
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: agricultural grassland (agrarisch gras)
					2: maize (mais)
					3: potatoes (aardappelen)
					4: beetroot (bieten)
					5: cereals (granen)
					6: other agricultural crops (overige landbouwgewassen)
					8: greenhouses (glastuinbouw)
					9: fruits / orchard (boomgaard)
					10: flower bulbs (bloembollen)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: continuous urban areas (bebouwing in primair bebouwd gebied)
					19: buildings in rural areas (bebouwing in secundair bebouwd gebied)
					20: bos in primair bebouwd gebied
					22: bos in secundair bebouwd gebied
					23: grass in built-up area (gras in primair bebouwd gebied)
					24: bare soil in built-up rural area (kale grond in bebouwd buitengebied)
					25: main roads and railways (hoofdwegen en spoorwegen)
					26: bebouwing in het buitengebied
					28: gras in secundair bebouwd gebied
					30: salt marshes (kwelders)
					31: beaches and dunes (open zand in kustgebied)
					32: duinen met lage vegetatie
					33: duinen met hoge vegetatie
					34: heath on dunes (duin-heide)
					35: open stuifzand en/ of rivierzand
					36: heath (heide)
					37: heathlands with minor grass infl (matig vergraste heide)
					38: heathlands with major grass infl (sterk vergraste heide)
					39: raised bog (hoogveen)
					40: forest in raised bog (bos in hoogveengebied)
					41: miscellaneous swamp vegetation (overige moerasvegetatie)
					42: reed swamp (rietvegetatie)
					43: forest in swamp area (bos in boerasgebied)
					45: natuurgraslanden
					61: boomwekerijen
					62: fruitwekerijen
					


Language:	Dutch



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn7_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\raw\organism\
Format: 		GeoTiff (.tif)
Type:			raster
Gridsize:		25m x 25m
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: agricultural grassland (agrarisch gras)
					2: maize (mais)
					3: potatoes (aardappelen)
					4: beetroot (bieten)
					5: cereals (granen)
					6: other agricultural crops (overige landbouwgewassen)
					8: greenhouses (glastuinbouw)
					9: fruits / orchard (boomgaard)
					10: flower bulbs (bloembollen)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: continuous urban areas (bebouwing in primair bebouwd gebied)
					19: buildings in rural areas (bebouwing in secundair bebouwd gebied)
					20: bos in primair bebouwd gebied
					22: bos in secundair bebouwd gebied
					23: grass in built-up area (gras in primair bebouwd gebied)
					24: bare soil in built-up rural area (kale grond in bebouwd buitengebied)
					25: main roads and railways (hoofdwegen en spoorwegen)
					26: bebouwing in het buitengebied
					28: gras in secundair bebouwd gebied
					30: salt marshes (kwelders)
					31: beaches and dunes (open zand in kustgebied)
					32: duinen met lage vegetatie
					33: duinen met hoge vegetatie
					34: heath on dunes (duin-heide)
					35: open stuifzand en/ of rivierzand
					36: heath (heide)
					37: heathlands with minor grass infl (matig vergraste heide)
					38: heathlands with major grass infl (sterk vergraste heide)
					39: raised bog (hoogveen)
					40: forest in raised bog (bos in hoogveengebied)
					41: miscellaneous swamp vegetation (overige moerasvegetatie)
					42: reed swamp (rietvegetatie)
					43: forest in swamp area (bos in boerasgebied)
					45: natuurgraslanden
					61: boomwekerijen
					62: fruitwekerijen
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	English