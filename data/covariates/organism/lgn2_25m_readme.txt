#### Readme for land use map (LGN2) data ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN2.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-2
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
					6: other crops (overige gewassen)
					7: bare soil (kale (landbouw)grond)
					8: greenhouses (glastuinbouw)
					9: fruits (boomgaard)
					10: flower bulbs (bollen)
					11: loofbos
					12: naaldbos
					13: heath (droge heide)
					14: overig open begroeid natuurgebied
					15: kale grond in natuurgebied
					16: zoet water
					17: zout water
					18: stedelijk bebouwd gebied
					19: bebouwing in buitengebied
					20: loofbos in bebouwd gebied
					21: naaldbos in bebouwd gebied
					22: bos met dichte bebouwing
					23: gras in bebouwd gebied
					24: kale grond in bebouwd buitengebied
					25: hoofdwegen en spoorwegen
					30: akkerbouw
					34: aardappelen/graan
					35: aardappelen/mais
					36: aardappelen/bieten
					37: aardappelen/bieten/overig
					38: bieten/mais
					39: aardappelen/bieten/mais
					40: aardappelen/bieten/mais/overig
					41: graan/overig
					42: kaal/aardappelen/graan/overig
					43: kaal/aardappelen/bollen/graan/overig
					44: kaal/graan
					45: bollen/overig
					46: kaal/bollen/overig
					47: kaal/graan/overig
					48: kaal/overig
					49: akkerbouw/tuinbouw
					50: mais/overig
					51: aardappelen/mais/overig
					52: kaal/aardappelen/graan


Language:	Dutch & English



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn2_25m.tif
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
					6: other crops (overige gewassen)
					7: bare soil (kale (landbouw)grond)
					8: greenhouses (glastuinbouw)
					9: fruits (boomgaard)
					10: flower bulbs (bollen)
					11: loofbos
					12: naaldbos
					13: heath (droge heide)
					14: overig open begroeid natuurgebied
					15: kale grond in natuurgebied
					16: zoet water
					17: zout water
					18: stedelijk bebouwd gebied
					19: bebouwing in buitengebied
					20: loofbos in bebouwd gebied
					21: naaldbos in bebouwd gebied
					22: bos met dichte bebouwing
					23: gras in bebouwd gebied
					24: kale grond in bebouwd buitengebied
					25: hoofdwegen en spoorwegen
					30: akkerbouw
					34: aardappelen/graan
					35: aardappelen/mais
					36: aardappelen/bieten
					37: aardappelen/bieten/overig
					38: bieten/mais
					39: aardappelen/bieten/mais
					40: aardappelen/bieten/mais/overig
					41: graan/overig
					42: kaal/aardappelen/graan/overig
					43: kaal/aardappelen/bollen/graan/overig
					44: kaal/graan
					45: bollen/overig
					46: kaal/bollen/overig
					47: kaal/graan/overig
					48: kaal/overig
					49: akkerbouw/tuinbouw
					50: mais/overig
					51: aardappelen/mais/overig
					52: kaal/aardappelen/graan
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)

Language:	English