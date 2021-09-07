#### Readme for drainage units ####
By A.F. Helfenstein
2020-03-06


#### SOURCE FILE ####
Name:		Afwateringseenheden_nlb_2008_03.lyr
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:	
Spatial 
coverage:	National
Projection:	RD_new
Content (D):	- Drainage units in 9 classes:
				1) sloten zoet
				2) sloten zout
				3) stromend water langzaam
				4) stromend water snel
				5) grote rivieren
				6) grote rivieren, snelstromend
				7) boezemwater
				10) zout en brak
				11) grote meren 

Content (EN):	- Drainage units in 9 classes:
				1) closed freshwater
				2) closed saltwater
				3) flowing water slow
				4) flowing water fast
				5) large rivers
				6) large rivers, fast flowing
				7) stored water before discharging through dike
				10) salty and brackish
				11) large seas (ocean?)

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Polygon to raster (conversion)
	- cell size and snap raster using existing AHN2 (25 x 25 m resolution)

2.	Raster to geoTIFF-format (conversion)



#### FINAL FILE ####
Name:		afwatering_nlb_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\organism\
Format: 	geoTIFF
Type:		Raster
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	- Drainage units in 10 classes:
				0) areas outside of the Netherlands
				1) closed freshwater
				2) closed saltwater
				3) flowing water slow
				4) flowing water fast
				5) large rivers
				6) large rivers, fast flowing
				7) stored water before discharging through dike
				10) salty and brackish
				11) large seas (ocean?)



Language:	English