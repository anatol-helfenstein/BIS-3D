#### Readme for clay percentages BOFEK ####
By T.L. van Orsouw
October 3rd, 2019


#### SOURCE FILE ####
Name:		BOdemFysische EenhedenKaart 2012 (BOFEK12)
		(Soil-physical map 2012)
Source:		https://www.wur.nl/nl/show/Bodemfysische-Eenhedenkaart-BOFEK2012.htm
Format: 	ESRI format File-Geodatabase
		Excel-datasheets
Type:		Polygons
Availability:	2012
Spatial 
coverage:	National
Projection:	RD_new
Content (D):	- Bodemfysische eenheden.
		- Representatieve profielen per eenheid.
		- Representatieve hydraulische eigenschappen per profiel.

Content (EN):	- Soil-physical mapping units.
		- Representative soil profiles per mapping unit.
		- Representative hydraulic properties per soil profile.

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1.	Generalization of the clay content. The weighted mean clay percentage was calculated for the following depths:
	- 0-5 cm
	- 0-10 cm
	- 10-15 cm
	- 15-30 cm
	- 30-60 cm
	- 60-100 cm

2.	Conversion to raster of 25mx25m, using existing DEM (AHN2) to snap/match the raster.

3.	Conversion to multi-band geoTIFF-format. 



#### FINAL FILE ####
Name:		clay_percentages_BOFEK.tif
Location:	W:\ESG\DOW_SGL\Projects\GRS33306development\spatial_data\DutchSoilData
Format: 	geoTIFF (6-band)
Type:		Raster
Spatial 
coverage:	National, NaN at urban locations.
Projection:	RD_new

Content (EN):	Clay percentages in Dutch soil.

		The file consists of 6 bands, namely representations of the soil profile up to 100 cm depth:
		- Band01:	0-5 cm
		- Band02:	5-10 cm
		- Band03:	10-15 cm
		- Band04: 	15-30 cm
		- Band05:	30-60 cm
		- Band06:	60-100 cm


Language:	English