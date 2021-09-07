#### Readme for all raw AHN[] and hillshade data at 25m resolution data ####
By A.F. Helfenstein
2020-03-19


#### ORIGINAL ####
Name:		AHN_25.lyr
			AHN_25_Hillshade.lyr
			Ahn2_5m_filled_tif.lyr
			AHN2 5m HillShade (Zx5).lyr
			AHN2 5m NoData.lyr
			Water en Buitenland.lyr
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Elevation_Hoogte
Format: 	ESRI layerfile (.lyr)
Type:		raster
Gridsize:	25m x 25m, 5m x 5m
Availability:	?
Temporal 
coverage:	
Spatial 
coverage:	Netherlands
Projection:	RD_new

Content (EN):	- Downloaded DEM and hillshade raster files from AHN at 25m resolution
						* Note: For AHN, 5m resolution would also be available
				- Downloaded DEM and hillshade raster files from AHN2 at 5m resolution
						* Note: For AHN2, 25m resolution was not available on Geodesk
				- The "AHN2 5m NoData.lyr" file contains all buildings and water bodies. Might be useful to clip all covariates by this map...
				- The "Water en Buitenland.lyr" file contains all water bodies and neighboring countries. Might be useful to clip all covariates by this map...

Language:	



#### PROCESSING ####
1. Open ArcGIS layerfiles and export rasters (DEM, hillshade and others) as Tiffs (.tif)

2. Rename files, e.g. "AHN_25.tif" to "ahn_25m.tif" and move to BISplus project folder (W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus)



#### FINAL FILE ####
Name:		ahn_25m.tif
			ahn_hillshade_25m.tif
			ahn2_5m.tif
			ahn2_hillshade_5m.tif
			ahn2_nodata_5m.tif
			ahn2_water_buitenland_5m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\raw\relief
Format: 	GeoTiff files (.tif)
Type:		raster
Gridsize:	25m x 25m, 5m x 5m
Temporal 
coverage:	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	- Downloaded DEM and hillshade raster files from AHN at 25m resolution
						* Note: For AHN, 5m resolution would also be available
				- Downloaded DEM and hillshade raster files from AHN2 at 5m resolution
						* Note: For AHN2, 25m resolution was not available on Geodesk
				- The "AHN2 5m NoData.lyr" file contains all buildings and water bodies. Might be useful to clip all covariates by this map...
				- The "Water en Buitenland.lyr" file contains all water bodies and neighboring countries. Might be useful to clip all covariates by this map...

Language:	