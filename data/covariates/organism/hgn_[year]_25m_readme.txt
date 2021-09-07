#### Readme for historical land use maps (HGN) ####
By A.F. Helfenstein
2020-03-25
updated: 2021-01-07


#### ORIGINAL ####
Name:			HGN_1900.lyr
			HGN_1960.lyr
			HGN_1970.lyr
			HGN_1980.lyr
			HGN_1990.lyr
Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\HGN
Format: 		ESRI layer file (.lyr)
Type:			raster
Gridsize:		50m x 50m or 25m x 25m
Availability:	?
Temporal 
coverage:		1900, 1960, 1970, 1980, 1990
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	historical land use ordered in the following categories:
			- HGN_1900:
				1: grasland
				2: akker en kale grond
				3: heide en hoogveen
				4: loofbos
				5: naaldbos
				6: bebouwing en wegen
				7: water
				8: rietmoeras
				9: stuifzanden en zandplaten
				10: overig
			- all others (1960 - 1990):
				1: grasland
				2: akker en kale grond
				3: heide en hoogveen
				4(+5): bos
				6: bebouwing en wegen
				7: water
				8: rietmoeras
				9: stuifzanden en zandplaten
				12: bebouwd gebied
				13: kassen
				98: baarle nassau
				99: noordzee en buitenland


Language:	Dutch



#### PROCESSING ####
1. Open .lyr file in ArcGIS Pro and export as Tiff (.tif). If neccesary, change from 50m x 50m resolution to 25m x 25m resolution (I'm assuming ArcGIS uses resampling function for this...)
2. For hgn_1900, edge of NL borders contains missing values. These were filled using GDAL "Fill nodata" function in QGIS and name changed to "hgn_1900_filled_25m.tif"



#### FINAL FILE ####
Name:			hgn_1900_filled_25m.tif
			hgn_1960_25m.tif
			hgn_1970_25m.tif
			hgn_1980_25m.tif
			hgn_1990_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\raw\organism\
Format: 		GeoTiff (.tif)
Type:			raster
Gridsize:		25m x 25m
Temporal 
coverage:		1900, 1960, 1970, 1980, 1990
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	historical land use ordered in the following categories:
			- HGN_1900:
				1: grasland
				2: akker en kale grond
				3: heide en hoogveen
				4: loofbos
				5: naaldbos
				6: bebouwing en wegen
				7: water
				8: rietmoeras
				9: stuifzanden en zandplaten
				10: overig
			- all others (1960 - 1990):
				1: grasland
				2: akker en kale grond
				3: heide en hoogveen
				4(+5): bos
				6: bebouwing en wegen
				7: water
				8: rietmoeras
				9: stuifzanden en zandplaten
				12: bebouwd gebied
				13: kassen
				98: baarle nassau
				99: noordzee en buitenland


Language:	Dutch