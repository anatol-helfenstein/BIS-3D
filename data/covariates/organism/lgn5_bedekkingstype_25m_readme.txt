#### Readme for land use map (LGN5) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN5_BedekkingsType.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-5
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
					1: grass (grasland)
					7: akker
					9: fruits / orchard (boomgaard)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					14: overig open vegetatie
					15: kale bodem
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: bebouwing
					25: infrastructuur


Language:	Dutch & English



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn5_bedekkingstype_25m.tif
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
					1: grass (grasland)
					7: akker
					9: fruits / orchard (boomgaard)
					11: deciduous forest (loofbos)
					12: coniferous forest (naaldbos)
					14: overig open vegetatie
					15: kale bodem
					16: fresh water (zoet water)
					17: salt water (zout water)
					18: bebouwing
					25: infrastructuur
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	English