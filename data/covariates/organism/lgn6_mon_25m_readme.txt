#### Readme for land use map (LGN6) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN6_mon.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-6
Format: 		ESRI layer file (.lyr)
Type:			raster
Gridsize:		25m x 25m
Availability:	?
Temporal d
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					7: agrarisch gebied
					8: glastuinbouw
					9: boomgaarden
					11: bos
					12: water
					18: bebouwd gebied
					25: infrastructuur
					30: natuur


Language:	Dutch



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn6_mon_25m.tif
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
					7: agrarisch gebied
					8: glastuinbouw
					9: boomgaarden
					11: bos
					12: water
					18: bebouwd gebied
					25: infrastructuur
					30: natuur
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	Dutch