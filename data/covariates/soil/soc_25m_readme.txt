#### Readme for the Dutch contribution to the Global Soil Organic Carbon (GSOC) map ####
By A.F. Helfenstein
2020-04-03


#### SOURCE FILE ####
Name:		soil_organic_carbon_concentration.lyr
		soil_organic_carbon_stock.lyr
		standard_deviation_of_soil_organic_carbon_concentration.lyr
		standard_deviation_of_soil_organic_carbon_stock.lyr

Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Soil_Organic_Carbon)
Format: 	ESRI format LayerFile
Type:		raster
Gridsize:	1km x 1km
Availability:	
Temporal
Coverage:	(map completed in 2017, soil samples originating from many decades back to 1960s)	
Spatial 
coverage:	Netherlands / National
Projection:	GCS WGS 1984

Content:	soil organic carbon (SOC) concentrations, the standard deviations (STD) thereof,
		SOC stocks and the STD's thereof. For complete content details see "gsoc-nl-metadata.pdf"
		in the same GeoDesk subfolder.

Language:	English
Terms-of use:	



#### PROCESSING ####
1. Convert to / save as GeoTIFF, changing the projection to RD_new and resampling to 25m resolution



#### FINAL FILE ####
Name:		soc_conc_25m.tif
		soc_conc_std_25m.tif
		soc_stock_25m.tif
		soc_stock_std_25m.tif

Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\raw\soil
Format: 	GeoTIFF
Type:		Raster
Gridsize:	25m x 25m
Temporal
Coverage:	(map completed in 2017, soil samples originating from many decades back to 1960s)
Spatial 
coverage:	Netherlands / National
Projection:	RD_new

Content:	soil organic carbon (SOC) concentrations, the standard deviations (STD) thereof,
		SOC stocks and the STD's thereof. For complete content details see "gsoc-nl-metadata.pdf"
		in the same GeoDesk subfolder.

Language:	English