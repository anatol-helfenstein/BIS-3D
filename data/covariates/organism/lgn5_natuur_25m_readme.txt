#### Readme for land use map (LGN5) ####
By A.F. Helfenstein
2020-03-20


#### ORIGINAL ####
Name:			LGN5_Natuur.lyr
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
					7: agrarisch gebied
					11: loofbos
					12: naaldbos
					16: zoet water
					17: zout water
					18: bebouwd gebied
					30: kustgebied
					35: heidegebied
					39: hoogveen
					41: moeras
					44: veenweidegebied
					45: overige natuur
					46: kale grond in natuurgebied


Language:	Dutch



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif)



#### FINAL FILE ####
Name:			lgn5_natuur_25m.tif
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
					11: loofbos
					12: naaldbos
					16: zoet water
					17: zout water
					18: bebouwd gebied
					30: kustgebied
					35: heidegebied
					39: hoogveen
					41: moeras
					44: veenweidegebied
					45: overige natuur
					46: kale grond in natuurgebied
				* note: if there are other categories (e.g. up to 255, they are all "no data" and should be eliminated when cropping all covariates to common DEM before predicting soil properties)


Language:	Dutch