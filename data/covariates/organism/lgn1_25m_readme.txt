#### Readme for land use map (LGN1) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN1_en.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-1
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
					1: grass
					2: maize
					3: potatoes
					4: beetroot
					5: cereals
					6: other crops
					7: bare soil
					8: greenhouses
					9: fruits
					10: flower bulbs
					11: tree nursery
					12: heath
					13: deciduous forest
					14: pine forest
					15: other natural situation
					16: water
					17: buildings and roads


Language:	Dutch & English



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn1_25m.tif
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
					1: grass
					2: maize
					3: potatoes
					4: beetroot
					5: cereals
					6: other crops
					7: bare soil
					8: greenhouses
					9: fruits
					10: flower bulbs
					11: tree nursery
					12: heath
					13: deciduous forest
					14: pine forest
					15: other natural situation
					16: water
					17: buildings and roads
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	English