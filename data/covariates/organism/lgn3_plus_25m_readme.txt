#### Readme for land use map (LGN3) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN3plus_EN.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-3
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
					0: no data
					1: grass (gras)
					2: maize (mais)
					3: potatoes (aardappelen)
					4: beetroot (bieten)
					5: cereals (granen)
					6: other agricultural crops (overige gewassen)
					7: bare soil (kale (landbouw)grond)
					8: greenhouses (glastuinbouw)
					9: fruits / orchard (boomgaard)
					10: flower bulbs (bollen)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: continuous urban areas (stedelijk bebouwd gebied)
					19: buildings in rural areas (bebouwing in buitengebied)
					20: deciduous forest in urban areas (loofbos in bebouwd gebied)
					21: coniferous forest in urban area (naaldbos in bebouwd gebied)
					22: forest with high density of buildings (bos met dichte bebouwing)
					23: grass in built-up area (gras in bebouwd gebied)
					24: bare soil in built-up rural area (kale grond in bebouwd buitengebied)
					25: main roads and railways (hoofdwegen en spoorwegen)
					26: buildings in rural area
					30: salt marshes
					31: beaches and dunes
					32: sparsely vegetated dunes
					33: vegetated dunes
					34: heath on dunes
					35: shifting sands
					36: heath
					37: heathlands with minor grass infl
					38: heathlands with major grass infl
					39: raised bog
					40: forest in raised bog
					41: miscellaneous swamp vegetation
					42: reed swamp
					43: forest in swamp area
					44: swampy pastures in peat areas
					45: sparse natural vegetation
					46: bare soil in natural vegetation


Language:	Dutch & English



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn3_plus_25m.tif
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
					0: no data
					1: grass (gras)
					2: maize (mais)
					3: potatoes (aardappelen)
					4: beetroot (bieten)
					5: cereals (granen)
					6: other agricultural crops (overige gewassen)
					7: bare soil (kale (landbouw)grond)
					8: greenhouses (glastuinbouw)
					9: fruits / orchard (boomgaard)
					10: flower bulbs (bollen)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: continuous urban areas (stedelijk bebouwd gebied)
					19: buildings in rural areas (bebouwing in buitengebied)
					20: deciduous forest in urban areas (loofbos in bebouwd gebied)
					21: coniferous forest in urban area (naaldbos in bebouwd gebied)
					22: forest with high density of buildings (bos met dichte bebouwing)
					23: grass in built-up area (gras in bebouwd gebied)
					24: bare soil in built-up rural area (kale grond in bebouwd buitengebied)
					25: main roads and railways (hoofdwegen en spoorwegen)
					26: buildings in rural area
					30: salt marshes
					31: beaches and dunes
					32: sparsely vegetated dunes
					33: vegetated dunes
					34: heath on dunes
					35: shifting sands
					36: heath
					37: heathlands with minor grass infl
					38: heathlands with major grass infl
					39: raised bog
					40: forest in raised bog
					41: miscellaneous swamp vegetation
					42: reed swamp
					43: forest in swamp area
					44: swampy pastures in peat areas
					45: sparse natural vegetation
					46: bare soil in natural vegetation
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	English