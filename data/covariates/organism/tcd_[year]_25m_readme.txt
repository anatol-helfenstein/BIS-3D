#### Readme for tree cover density (TCD) data from Copernicus Land Monitoring Data (High Resolution Layers) ####
By A.F. Helfenstein
2020-03-24


#### ORIGINAL ####
Name:		Tree_Cover_Density_2012_020m_version2
		Tree_Cover_Density_2015_020m_version2
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\Copernicus_Land_Monitoring_Data\Copernicus_High_Resolution_Layers\Forest
Format: 	ESRI layer file
Type:		raster tiff 
Gridsize:	20m x 20m
Availability:	?
Temporal 
coverage:	2012 & 2015
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (EN):	forest area in the following categories:
					0: all non-tree covered areas
					1-100: % tree cover density
					255: outside area (outside of Netherlands?)

Language:	English



#### PROCESSING ####
1. Export raster as GeoTiff (.tif), changing the resolution from 20m x 20m to 25m x 25m



#### FINAL FILE ####
Name:		tcd_2012_25m.tif
		tcd_2015_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\raw\organism
Format: 	geoTIFF
Type:		Raster
Gridsize:	25m x 25m
Temporal 
coverage:	2012, 2015
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (EN):	forest area in the following categories:
					0: all non-tree covered areas
					1-100: % tree cover density
					255: outside area (outside of Netherlands?)

Language:	English