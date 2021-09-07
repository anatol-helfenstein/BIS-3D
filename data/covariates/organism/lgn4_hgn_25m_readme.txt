#### Readme for land use map (LGN4) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN4 als HGN.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\LGN-4
Format: 		ESRI layer file (.lyr)
Type:			raster
Gridsize:		50m x 50m
Availability:	?
Temporal 
coverage:		
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	land use ordered in the following categories:
					1: grasland (grass)
					2: akker en kale grond
					3: heide en hoogveen
					4+5: bos
					6: bebouwing en wegen
					7: water
					8: rietmoeras
					9: zand


Language:	Dutch



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif). Change from 50m x 50m resolution to 25m x 25m resolution (I'm assuming ArcGIS uses resampling function for this...)



#### FINAL FILE ####
Name:			lgn4_hgn_25m.tif
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
					1: grasland (grass)
					2: akker en kale grond
					3: heide en hoogveen
					4+5: bos
					6: bebouwing en wegen
					7: water
					8: rietmoeras
					9: zand
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	Dutch