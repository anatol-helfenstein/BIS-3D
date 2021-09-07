#### Readme for crop data ####
By T.L. van Orsouw
October 3rd, 2019


#### SOURCE FILE ####
Name:		Basisregistratie Gewaspercelen (BRP)
		(Registered crops in fields)
Source:		https://www.pdok.nl/introductie/-/article/basisregistratie-gewaspercelen-brp-
Format: 	ESRI format File-Geodatabase (versions 9.3 through 10.2)
Type:		Polygons
Availability:	2009 - 2019 (updated yearly)
Spatial 
coverage:	National, only registered agricultural fields.
Projection:	RD_new
Content (D):	- CAT_GEWASCATEGORIE	: Gewascategorie van het gewas op het perceel
		- GWS_GEWAS		: Gewas op het perceel
		- GWS_GEWASCODE		: Gewascode van het gewas

Content (EN):	- CAT_GEWASCATEGORIE	: Crop category (grassland/agricultural/other)
		- GWS_GEWAS		: Crop description
		- GWS_GEWASCODE		: Crop specific code

Language:	Dutch
Terms-of use:	http://www.rijksoverheid.nl/opendata/voorwaarden



#### PROCESSING ####
1.	Selection of relevant and complete years: 2011-2018
	- 2019 not yet finalized.
	- Most crop rotations have cycles of four to eight years, 
	  using eight years of data will therefore mean most cycles will be completed at least once.

2.	Conversion from Polygon-based file to ESRI raster
	- 25 m x 25m resolution
	- Snapped to extent of existing dataset (AHN2)

3.	Conversion of 413 unique crop descriptions/codes to 18 general categories (in English, based on interpretation of descriptions!!)
	- Potatoes	
	- Beets
	- Cereals
	- Onions
	- Legumes
	- Vegetables
	- Maize
	- Grassland
	- Flowers (incl. bulbs)
	- Seeds
	- Fruits
	- Forestry
	- Forest (not production forest!)
	- Cover crops
	- Buffer strips
	- Nature (e.g. heather)
	- Bare ground
	- Other (e.g. unknown, no longer agriculture)

4.	Summation of the occurence of specific crop categories over eight year timespan
	- e.g.
		- Potatoes	2x
		- Onions	1x
		- Beets		2x
		- Cereals	1x
		- Root crops	2x
		- Cole crops	1x

5.	Removal of raster cells for which a full 8 year record is unavailable
	- Caused by redrawing of some field margins, or by removal of fields from database (not used as agricultural land).

6.	Combination of all crop classes to single multi-band geoTIFF-file.
	- Band01:	Potatoes
	- Band02:	Beets
	- Band03:	Cereals
	- Band04: 	Onions
	- Band05:	Legumes
	- Band06:	Vegetables
	- Band07: 	Maize
	- Band08:	Grassland
	- Band09:	Flowers
	- Band10:	Seeds
	- Band11:	Fruits
	- Band12:	Forestry
	- Band13:	Forest
	- Band14:	Cover crops
	- Band15:	Buffer strips
	- Band16:	Nature
	- Band17:	Bare ground/fallow
	- Band18:	Other

6.	Coversion to geoTIFF



#### FINAL FILE ####
Name:		crop_rotation_2011_2018.tif
Location:	W:\ESG\DOW_SGL\Projects\GRS33306development\spatial_data\DutchCropData
Format: 	geoTIFF (18-band)
Type:		Raster
Temporal 
coverage:	2011 - 2018
Spatial 
coverage:	National, only registered agricultural fields for which an eight year record is available. NaN for other locations.
Projection:	RD_new

Content (EN):	Occurrence of crop classes over a timespan of eight years (2011-2018).
		- Value of 0 = not in rotation
		- Value of 1 = once in 8 years
		- Value of 2 = twice in 8 years (e.g. every fourth year)
		- value of 3 = thrice in 8 years
		- Value of 4 = four times in 8 years (e.g. every other year) 
		- etc. until value 8 = permaculture

		The file consists of 18 bands, namely:
		- Band01:	Potatoes
		- Band02:	Beets
		- Band03:	Cereals
		- Band04: 	Onions
		- Band05:	Legumes
		- Band06:	Vegetables
		- Band07: 	Maize
		- Band08:	Grassland
		- Band09:	Flowers
		- Band10:	Seeds
		- Band11:	Fruits
		- Band12:	Forestry
		- Band13:	Forest
		- Band14:	Cover crops
		- Band15:	Buffer strips
		- Band16:	Nature
		- Band17:	Bare ground/fallow
		- Band18:	Other

Language:	English