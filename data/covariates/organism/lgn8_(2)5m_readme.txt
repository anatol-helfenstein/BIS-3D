#### Readme for land use map (LGN8) ####
By A.F. Helfenstein
2020-03-23


#### ORIGINAL ####
Name:			lgn2018(en).lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN2018
Format: 		ESRI layer file (.lyr)
Type:			raster
Gridsize:		5m x 5m
Availability:	?
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: pasture
					2: maize (mais)
					3: potatoes (aardappelen)
					4: sugar beet
					5: cereals (granen)
					6: other agricultural crops (overige landbouwgewassen)
					8: greenhouses (glastuinbouw)
					9: fruits / orchard (boomgaard)
					10: flower bulbs (bloembollen)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: urban built-up areas (bebouwing in primair bebouwd gebied)
					19: semi-urban built-up areas (bebouwing in secundair bebouwd gebied)
					20: forest in built-up areas
					22: forest in semi built-up areas
					23: grass in built-up areas (gras in primair bebouwd gebied)
					24: bare soil in built-up areas (kale grond in bebouwd buitengebied)
					25: main roads and railways (hoofdwegen en spoorwegen)
					26: built-up areas outside urban areas (bebouwing in het buitengebied)
					27: other land use outside urban areas
					28: grass in semi built-up areas (gras in secundair bebouwd gebied)
					30: salt marshes (kwelders)
					31: costal sands (open zand in kustgebied)
					32: coastal dune areas with low vegetation (duinen met lage vegetatie)
					33: coastal dune areas with high vegetation (duinen met hoge vegetatie)
					34: heathland in coastal areas (duin-heide)
					35: drifting sands/river sandbanks (open stuifzand en/ of rivierzand)
					36: heathland (heide)
					37: grassy heathland (matig vergraste heide)
					38: very grassy heathland (sterk vergraste heide)
					39: peat bogs (hoogveen)
					40: forest in peat bogs (bos in hoogveengebied)
					41: other swamp vegetation (overige moerasvegetatie)
					42: reeds (rietvegetatie)
					43: forest in swamp areas (bos in boerasgebied)
					45: natural grasslands (natuurgraslanden)
					46: Grass in coastal areas
					47: other grass
					61: tree nurseries (boomwekerijen)
					62: fruit cultivation (fruitwekerijen)
					321: bush/shrub vegetation in peat bogs (low)
					322: bush/shrub vegetation in swamp areas (low)
					323: other bush/shrub vegetation (low)
					331: bush/shrub vegetation in peat bogs (high)
					332: bush/shrub vegetation in swamp areas (high)
					333: other bush/shrub vegetation (high)

					


Language:	English



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)

(2. Export as Tiff with 25m x 25m resolution (I presume ArcGIS uses some kind of resampling function))



#### FINAL FILE ####
Name:			lgn8_(2)5m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\raw\organism\
Format: 		GeoTiff (.tif)
Type:			raster
Gridsize:		5m x 5m (25m x 25m)
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: pasture
					2: maize (mais)
					3: potatoes (aardappelen)
					4: sugar beet
					5: cereals (granen)
					6: other agricultural crops (overige landbouwgewassen)
					8: greenhouses (glastuinbouw)
					9: fruits / orchard (boomgaard)
					10: flower bulbs (bloembollen)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: urban built-up areas (bebouwing in primair bebouwd gebied)
					19: semi-urban built-up areas (bebouwing in secundair bebouwd gebied)
					20: forest in built-up areas
					22: forest in semi built-up areas
					23: grass in built-up areas (gras in primair bebouwd gebied)
					24: bare soil in built-up areas (kale grond in bebouwd buitengebied)
					25: main roads and railways (hoofdwegen en spoorwegen)
					26: built-up areas outside urban areas (bebouwing in het buitengebied)
					27: other land use outside urban areas
					28: grass in semi built-up areas (gras in secundair bebouwd gebied)
					30: salt marshes (kwelders)
					31: costal sands (open zand in kustgebied)
					32: coastal dune areas with low vegetation (duinen met lage vegetatie)
					33: coastal dune areas with high vegetation (duinen met hoge vegetatie)
					34: heathland in coastal areas (duin-heide)
					35: drifting sands/river sandbanks (open stuifzand en/ of rivierzand)
					36: heathland (heide)
					37: grassy heathland (matig vergraste heide)
					38: very grassy heathland (sterk vergraste heide)
					39: peat bogs (hoogveen)
					40: forest in peat bogs (bos in hoogveengebied)
					41: other swamp vegetation (overige moerasvegetatie)
					42: reeds (rietvegetatie)
					43: forest in swamp areas (bos in boerasgebied)
					45: natural grasslands (natuurgraslanden)
					46: Grass in coastal areas
					47: other grass
					61: tree nurseries (boomwekerijen)
					62: fruit cultivation (fruitwekerijen)
					321: bush/shrub vegetation in peat bogs (low)
					322: bush/shrub vegetation in swamp areas (low)
					323: other bush/shrub vegetation (low)
					331: bush/shrub vegetation in peat bogs (high)
					332: bush/shrub vegetation in swamp areas (high)
					333: other bush/shrub vegetation (high)
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	English