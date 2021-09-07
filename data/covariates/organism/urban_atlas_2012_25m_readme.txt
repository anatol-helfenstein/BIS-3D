#### Readme for land cover & land use zones in urban areas (Urban Atlas 2012) data ####
By A.F. Helfenstein
2020-03-25


#### ORIGINAL ####
Name:			Urban_Atlas_2012_NL.lyr


Source:			\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\Copernicus_Land_Monitoring_Data\Urban_Atlas
Format: 		ESRI layer file
Type:			polygon
Gridsize:		
Availability:	?
Temporal 
coverage:		2012
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	- land cover and land use in urban areas in the Netherlands
			- variable of interest: "CODE2012" (number) & "ITEM2012" (description)
				11100: Continuous urban fabric (S.L. > 80%)
				11210: Discontinuous dense urban fabric (S.L. 50% - 80%)
				11220: Discontinuous medium density urban fabric (S.L. 30% - 50%)
				11230: Discontinuous low density urban fabric (S.L. 10% - 30%)
				11230: Discontinuous very low density urban fabric (S.L. < 10%)
				11300: Isolated structures
				12100: Industrial, commercial, public, military and private units
				12210: Fast transit roads and associated land
				12220: Other roads and associated land
				12230: Railways and associated land
				12300: Port areas
				12400: Airports
				13100: Mineral extraction and dump sites
				13300: Construction sites
				13400: Land without current use
				14100: Green urban areas
				14200: Sports and leisure facilities
				21000: Arable land (annual crops)
				22000: Permanent crops
				23000: Pastures
				24000: Complex and mixed cultivation patterns
				25000: Orchards
				31000: Forests
				32000: Herbaceous vegetation associations
				33000: Open spaces with little or no vegetations
				40000: Wetlands
				50000: Water
				91000: No data


Language:		English



#### PROCESSING ####
1. Change projection using ArcGIS tool "project" to RDnew

2. Polygon to raster using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

3. Raster to other format (.tif)

4. Change name from e.g. "Urban_Atlas_2012_NL_PolygonToRaster" to "urban_atlas_2012_25m"



#### FINAL FILE ####
Name:			urban_atlas_2012_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal 
coverage:		2012
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new

Content (EN):	- land cover and land use in urban areas in the Netherlands
			- variable of interest: "CODE2012" (number) & "ITEM2012" (description)
				11100: Continuous urban fabric (S.L. > 80%)
				11210: Discontinuous dense urban fabric (S.L. 50% - 80%)
				11220: Discontinuous medium density urban fabric (S.L. 30% - 50%)
				11230: Discontinuous low density urban fabric (S.L. 10% - 30%)
				11230: Discontinuous very low density urban fabric (S.L. < 10%)
				11300: Isolated structures
				12100: Industrial, commercial, public, military and private units
				12210: Fast transit roads and associated land
				12220: Other roads and associated land
				12230: Railways and associated land
				12300: Port areas
				12400: Airports
				13100: Mineral extraction and dump sites
				13300: Construction sites
				13400: Land without current use
				14100: Green urban areas
				14200: Sports and leisure facilities
				21000: Arable land (annual crops)
				22000: Permanent crops
				23000: Pastures
				24000: Complex and mixed cultivation patterns
				25000: Orchards
				31000: Forests
				32000: Herbaceous vegetation associations
				33000: Open spaces with little or no vegetations
				40000: Wetlands
				50000: Water
				91000: No data


Language:	English