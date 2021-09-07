#### Readme for landcover database CORINE NL data ####
By A.F. Helfenstein
2020-03-24


#### ORIGINAL ####
Name:		CORINE_NL_Landcover_database_voor_het_jaar_1986.lyr
		CORINE_NL_Landcover_database_voor_het_jaar_2000.lyr
		CORINE_NL_Landcover_database_voor_het_jaar_2006.lyr
		CORINE_NL_Landcover_2012.lyr
		CORINE_NL_Landcover_2018.lyr


Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Landuse_Landgebruik\Copernicus_Land_Monitoring_Data\Corine_NL
Format: 	ESRI layer file
Type:		polygon
Gridsize:		
Availability:	?
Temporal 
coverage:	1986, 2000, 2006, 2012, 2018
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (EN):	land use in Netherlands in the following categories (variables of interest in attribute table are often called "code_18" e.g. for the year 2018):
					112: Discontinuous urban fabric
					121: Industrial and commercial units
					122: Road and rail network
					123: Port areas
					124: Airports
					131: Mineral extraction sites
					132: Dump sites
					133: Construction sites
					141: Green urban areas
					142: Sport and leisure facilities
					211: Non-irrigated arable land
					222: Fruit trees and berry plantations
					231: Pastures
					242: Complex cultivation patterns
					243: Land principally occupied by agriculture with siginficant areas of natural vegetation
					311: Broad leaved forest
					312: Coniferous forest
					313: Mixed forest
					321: Natural grasslands
					322: Moors and heathland
					324: Transitional woodland-shrub
					331: Beaches, dunes and sands
					411: Inland marshes
					412: Peat bogs
					421: Salt marshes
					423: Intertidal flats
					511: Water courses
					512: Water bodies
					522: Estuaries
					523: Sea and ocean


Language:		English



#### PROCESSING ####
1. Polygon to raster using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

2. Raster to other format (.tif)

3. Change name from e.g. "CORINE_NL_Landcover_2018_PolygonToRaster.tif" to "corineNL_[year]_25m"



#### FINAL FILE ####
Name:		corineNL_[year]_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Gridsize:	25m x 25m
Temporal 
coverage:	1986, 2000, 2006, 2012, 2018
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content (EN):	land use in Netherlands in the following categories:
					112: Discontinuous urban fabric
					121: Industrial and commercial units
					122: Road and rail network
					123: Port areas
					124: Airports
					131: Mineral extraction sites
					132: Dump sites
					133: Construction sites
					141: Green urban areas
					142: Sport and leisure facilities
					211: Non-irrigated arable land
					222: Fruit trees and berry plantations
					231: Pastures
					242: Complex cultivation patterns
					243: Land principally occupied by agriculture with siginficant areas of natural vegetation
					311: Broad leaved forest
					312: Coniferous forest
					313: Mixed forest
					321: Natural grasslands
					322: Moors and heathland
					324: Transitional woodland-shrub
					331: Beaches, dunes and sands
					411: Inland marshes
					412: Peat bogs
					421: Salt marshes
					423: Intertidal flats
					511: Water courses
					512: Water bodies
					522: Estuaries
					523: Sea and ocean


Language:	English