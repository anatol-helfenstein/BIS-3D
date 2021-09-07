#### Readme for generalized crop type ####
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
1.	Conversion to crop rotation file (see crop ratation readme).

2.	Clustering of crop types based on expected effects on soil carbon stock.
	- Taxing crops (potatoes, beets, onions & flowers) are harvested through intensive digging, which disturbs the soil and 
	  speeds up breakdown of organic matter.
	- Moderately taxing crops (maize, vegetables) are all crops which do not belong in any other category. These crops are
	  often harvested through mowing of aboveground biomass or light digging, meaning the soil is disturbed less through 
	  harvesting.
	- Lightly restorative crops (cereals & legumes) have deep root systems and/or fixate nutrients, promoting the formation
	  and storage of organic matter in the soil.
	- Restorative crops (grassland, green manure & nature) are perrenial by nature and promote a stable soil with minimal
	  disturbance. Some crops may be mowed or harvested, meaning some nutrients and carbon is removed from the local system.
	- Trees (forest, plantation, orchards) are a separate category due to their differing nature.
	- Other (undefined, unknown, discontinued agricultural fields)

3. 	Conversion to multi-band geoTIFF-file.



#### FINAL FILE ####
Name:		general_croptype_2011_2018.tif
Location:	W:\ESG\DOW_SGL\Projects\GRS33306development\spatial_data\DutchCropData
Format: 	geoTIFF (6-band)
Type:		Raster
Temporal 
coverage:	2011 - 2018
Spatial 
coverage:	National, only registered agricultural fields for which an eight year record is available. NaN for other locations.
Projection:	RD_new

Content (EN):	Occurrence of general crop classes over a timespan of eight years (2011-2018).
		- Value of 0 = not in rotation
		- Value of 1 = once in 8 years
		- Value of 2 = twice in 8 years (e.g. every fourth year)
		- value of 3 = thrice in 8 years
		- Value of 4 = four times in 8 years (e.g. every other year) 
		- etc. until value 8 = permaculture

		The file consists of 6 bands, namely:
		- Band01:	Taxing crops (potatoes, beets, onions, flowers)
		- Band02:	Moderately taxing crops (maize, vegetables)
		- Band03:	Lightly restorative crops (cereals, legumes)
		- Band04: 	Restorative crops (grassland, green manure, nature)
		- Band05:	Trees (plantation, orchard)
		- Band06:	Other (unknown, fallow)


Language:	English