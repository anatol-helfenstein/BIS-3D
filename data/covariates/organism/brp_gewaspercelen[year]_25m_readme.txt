#### Readme for agricultural parcels data from 2005 - 2019 (brp_gewaspercelen[year]) data ####
By A.F. Helfenstein
2020-03-13


#### ORIGINAL ####
Name:		BRP GewasPercelen [year]
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Agriculture_Landbouw
		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\BRP (Parcels)
Format: 	QGIS layer file (.qlr) or ArcGIS layer file (.lyr)
Type:		polygon
Gridsize:	
Availability:	?
Temporal 
coverage:	2005 - 2019
Spatial 
coverage:	Netherlands
Projection:	RD_new

Content (EN):	There are two variables / columns of interest in attribute tables:
			- "GWS_GEWAS" (gewaspercelen e.g. agricultural parcel crop type specific)
					- about 90 category levels
			- "CAT_GEWASC" (gewaspercelen category e.g. bouwland, grasland, etc. (arable, grasland, etc.))
					- about 4 category levels

Language:	Dutch & English



#### PROCESSING ####
(1. Open .qlr file in QGIS and save as / export as shapefile (e.g. brp_gewaspercelen[year].shp) for further processing in ArcGIS (or R))
*Note: In "\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\BRP (Parcels)", ArcGIS layerfiles are there and so this first step can be skipped.

1. Polygon to raster using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

* Do steps 1 & 2 for both variables "GWS_GEWAS" and "CAT_GEWASC"

3. Change name from "brp_gewaspercelen2005_PolygonToRaster" to "brp_gewaspercelen[year]_25m"
   Change name from "brp_gewaspercelen2005_PolygonToRaster_cat" to "brp_gewaspercelen[year]_cat_25m"



#### FINAL FILE ####
Name:		brp_gewaspercelen[year]_25m.tif
		brp_gewaspercelen[year]_cat_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus\data\covariates\processed\organism
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\BISplus_GIS\Preprocessing
Format: 	geoTiff (.tif)
Type:		raster
Gridsize:	
Temporal 
coverage:	2005 - 2019
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (EN):	There are two variables / columns of interest in attribute tables:
					- "GWS_GEWAS" (gewaspercelen e.g. agricultural parcel crop type specific)
							- about 90 category levels
					- "CAT_GEWASC" (gewaspercelen category e.g. bouwland, grasland, etc. (arable, grasland, etc.))
							- about 4 category levels

Language:	English