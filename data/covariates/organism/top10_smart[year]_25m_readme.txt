#### Readme for land cover / land use data from Top10 SMART topographic layers ####
By A.F. Helfenstein
2020-04-08


#### ORIGINAL ####
Name:		All layerfiles belonging to one year have the same attributes
		(only the variable visualized on the maps differ but data is the same)
		So it does not matter which layer is chosen for the processing step (below 4 examples):
		Top10_Smart_2004_010.lyr
		Top10_Smart_2006.lyr
		Top10_Smart_2009.lyr
		Top10_Smart_2013_Schalend.lyr
Source:		\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Topography_Topografie\Top10-Smart
Format: 	ESRI layer file
Type:		raster
Gridsize:	2.5m x 2.5m
Availability:	?
Temporal 
coverage:	2004, 2006, 2009 & 2013
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content:	- land cover / land use data from Top10 SMART topographic maps
		- 2004:
			4	=	naaldbos
			5	=	loof/gemengd bos
			6	=	heide
			7	=	zand en duinen
			10	=	stedelijk
			11	=	huizen binnen stedelijk
			12	=	snelwegen binnen stedelijk
			13	=	doorgaande wegen binnen stedelijk
			14	=	overige wegen binnen stedelijk
			16	=	landschappelijke elementen binnen stedelijk
			17	=	water binnen stedelijk
			18	=	sloten binnen stedelijk
			19	=	spoorwegen binnen stedelijk
			20	=	natuur (natuurlijke graslanden en insluitsels < 1 ha)
			21	=	huizen binnen natuur
			22	=	snelwegen binnen natuur
			23	=	doorgaande wegen binnen natuur
			24	=	overige wegen binnen natuur
			27	=	water binnen natuur
			28	=	sloten binnen natuur
			29	=	spoorwegen binnen natuur
			30	=	water
			31	=	huizen over water
			32	=	snelwegen over water
			33	=	doorgaande wegen over water
			34	=	overige wegen over water
			36	=	landschappelijke elementen binnen water
			40	=	akkers en overig agrarisch
			41	=	huizen binnen agrarisch en stedelijk groen
			42	=	snelwegen binnen agrarisch en stedelijk groen
			43	=	doorgaande wegen binnen agrarisch en stedlijk groen
			44	=	landbouwwegen
			45	=	agrarisch grasland
			46	=	landschappelijke elementen binnen agrarisch en stedelijke groen
			47	=	water binnen agrarisch en stedelijk groen
			48	=	sloten binnen agrarisch en stedelijk groen
			49	=	spoorwegen binnen agrarisch en stedelijk groen
			50	=	erf (overig bodemgebruik agrarisch gebied)
			51	=	erf (20% bebouwing in agrarisch grasland)
		- 2006 (full band 250 values that can be aggregated in R into the following classes):
			0	=	foutief
			1	=	gebouw/huis
			2	=	bebouwd gebied / huizenblok
			3	=	groot gebouw
			4	=	hoogbouw
			6	=	warenhuizen/kassen
			7	=	tank
			8	=	Auto(snel)weg
			9	=	Hoofdverbindingsroute
			10	=	Lokale weg
			11	=	Verbindingsroute
			12	=	overige weg >2m
			13	=	Ged. verharde weg
			14	=	onverharde weg
			15	=	voetgangersgebied
			16	=	rijwielpad
			17	=	parkeerterrein
			18	=	spoor
			19	=	loofbos
			20	=	naaldbos
			21	=	gemengd bos
			22	=	griend
			23	=	populierenopstand
			24	=	bouwland
			25	=	weiland
			26	=	boomgaard
			27	=	boomkwekerij
			28	=	heide
			29	=	zand
			30	=	overig bodemgebruik
			31	=	begraafplaats
			32	=	fruitwekerij
			33	=	enkele sloot*
			34	=	gerenforceerde sloot
			35	=	water (grote oppervlakte)
			36	=	oeverlijn / water (kleine opp)
			37	=	laagwaterlijn / droogvallend gr
			38	=	steenglooiing/krib
			39	=	aanlegsteiger
			40	=	Dok
			41	=	extensief beheerde graslanden
			42	=	natuurlijk water
			43	=	Rietmoeras
* need to remove the "enkele sloot" cells in the map to their respective region (e.g. designate as grasland...)
because at a 25m resolution, this creates undesired "spots" of water on the parcels.
		- 2009:
			1	=	bebouwing
			8	=	Auto(snel)weg
			9	=	Hoofdverbinding
			10	=	Lokale weg
			11	=	Verbindingsroute
			12	=	overige weg >2m
			13	=	Ged. verharde weg
			14	=	onverharde weg
			15	=	voetgangersgebied
			16	=	rijwielpad
			17	=	parkeerterrein
			18	=	spoor
			19	=	loofbos
			20	=	naaldbos
			21	=	gemengd bos
			22	=	griend
			23	=	populierenopstand
			24	=	bouwland
			25	=	weiland
			26	=	boomgaard
			27	=	boomkwekerij
			28	=	heide
			29	=	zand
			30	=	overig bodemgebruik
			31	=	begraafplaats
			32	=	fruitwekerij
			35	=	water (grote oppervlakte)
			36	=	water (kleine opp)
			37	=	droogvallend
			38	=	steenglooiing/krib
			39	=	aanlegsteiger
			42	=	Rietmoeras(W)
			43	=	Rietmoeras(G)
		- 2013:
			0	=	buitenland
			1	=	bebouwing
			2	=	Auto(snel)weg
			3	=	Hoofdverbindingsroute
			4	=	verbindingsroute
			5	=	Lokale weg
			6	=	Ged. verharde weg
			7	=	onverharde weg
			8	=	voetgangersgebied
			9	=	rijwielpad
			10	=	voetpad
			11	=	parkeerterrein
			12	=	spoorweg
			13	=	verharding vliegveld
			14	=	spoorbaanlichaam
			15	=	kassen	
			19	=	loofbos
			20	=	naaldbos
			21	=	bos divers
			24	=	bouwland
			25	=	grasland
			26	=	boomgaard
			27	=	boomkwekerij
			28	=	heide
			29	=	zand
			30	=	overig bodemgebruik
			31	=	begraafplaats
			32	=	fruitwekerij
			36	=	water
			37	=	droogvallende grond
			41	=	grasland met riet
			42	=	overig terrein met riet
			43	=	water met riet
			61	=	agrarisch bebouwing
			62	=	agrarische erven

Language:	Dutch



#### PROCESSING ####
2004 file:
1. Export raster as GeoTiff (.tif), changing the resolution from 2.5m x 2.5m to 25m x 25m

2006, 2009 & 2013 files:
1. Use "Lookup" Spatial Analysis tools to create a new raster, assigning each cell the value of the variable
"CODE_KORT" in the original raster. "CODE_KORT" is more condense (approx. 36 - 40 classes) compared to the "CLASS_NAME" variable,
which contained about 200 classes and was therefore too exhaustive and contained irrelevant information.
This new temporary intermediate raster we name "top10_smart[year]_kort".

2. Export raster "top10_smart[year]_kort" as GeoTiff, changing the resolution (resampling?) from 2.5m x 2.5m to 25m x 25m


#### FINAL FILE ####
Name:		top10_smart[year]_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BIS_plus\data\covariates\raw\organism
Format: 	GeoTiff
Type:		Raster
Gridsize:	25m x 25m
Temporal 
coverage:	2004, 2006, 2009 & 2013
Spatial 
coverage:	National (Netherlands)
Projection:	RD_new

Content:	- land cover / land use data from Top10 SMART topographic maps
		- 2004:
			4	=	naaldbos
			5	=	loof/gemengd bos
			6	=	heide
			7	=	zand en duinen
			10	=	stedelijk
			11	=	huizen binnen stedelijk
			12	=	snelwegen binnen stedelijk
			13	=	doorgaande wegen binnen stedelijk
			14	=	overige wegen binnen stedelijk
			16	=	landschappelijke elementen binnen stedelijk
			17	=	water binnen stedelijk
			18	=	sloten binnen stedelijk
			19	=	spoorwegen binnen stedelijk
			20	=	natuur (natuurlijke graslanden en insluitsels < 1 ha)
			21	=	huizen binnen natuur
			22	=	snelwegen binnen natuur
			23	=	doorgaande wegen binnen natuur
			24	=	overige wegen binnen natuur
			27	=	water binnen natuur
			28	=	sloten binnen natuur
			29	=	spoorwegen binnen natuur
			30	=	water
			31	=	huizen over water
			32	=	snelwegen over water
			33	=	doorgaande wegen over water
			34	=	overige wegen over water
			36	=	landschappelijke elementen binnen water
			40	=	akkers en overig agrarisch
			41	=	huizen binnen agrarisch en stedelijk groen
			42	=	snelwegen binnen agrarisch en stedelijk groen
			43	=	doorgaande wegen binnen agrarisch en stedlijk groen
			44	=	landbouwwegen
			45	=	agrarisch grasland
			46	=	landschappelijke elementen binnen agrarisch en stedelijke groen
			47	=	water binnen agrarisch en stedelijk groen
			48	=	sloten binnen agrarisch en stedelijk groen
			49	=	spoorwegen binnen agrarisch en stedelijk groen
			50	=	erf (overig bodemgebruik agrarisch gebied)
			51	=	erf (20% bebouwing in agrarisch grasland)
		- 2006 (full band 250 values that can be aggregated in R into the following classes):
			0	=	foutief
			1	=	gebouw/huis
			2	=	bebouwd gebied / huizenblok
			3	=	groot gebouw
			4	=	hoogbouw
			6	=	warenhuizen/kassen
			7	=	tank
			8	=	Auto(snel)weg
			9	=	Hoofdverbindingsroute
			10	=	Lokale weg
			11	=	Verbindingsroute
			12	=	overige weg >2m
			13	=	Ged. verharde weg
			14	=	onverharde weg
			15	=	voetgangersgebied
			16	=	rijwielpad
			17	=	parkeerterrein
			18	=	spoor
			19	=	loofbos
			20	=	naaldbos
			21	=	gemengd bos
			22	=	griend
			23	=	populierenopstand
			24	=	bouwland
			25	=	weiland
			26	=	boomgaard
			27	=	boomkwekerij
			28	=	heide
			29	=	zand
			30	=	overig bodemgebruik
			31	=	begraafplaats
			32	=	fruitwekerij
			33	=	enkele sloot*
			34	=	gerenforceerde sloot
			35	=	water (grote oppervlakte)
			36	=	oeverlijn / water (kleine opp)
			37	=	laagwaterlijn / droogvallend gr
			38	=	steenglooiing/krib
			39	=	aanlegsteiger
			40	=	Dok
			41	=	extensief beheerde graslanden
			42	=	natuurlijk water
			43	=	Rietmoeras
* need to remove the "enkele sloot" cells in the map to their respective region (e.g. designate as grasland...)
because at a 25m resolution, this creates undesired "spots" of water on the parcels.
		- 2009:
			1	=	bebouwing
			8	=	Auto(snel)weg
			9	=	Hoofdverbinding
			10	=	Lokale weg
			11	=	Verbindingsroute
			12	=	overige weg >2m
			13	=	Ged. verharde weg
			14	=	onverharde weg
			15	=	voetgangersgebied
			16	=	rijwielpad
			17	=	parkeerterrein
			18	=	spoor
			19	=	loofbos
			20	=	naaldbos
			21	=	gemengd bos
			22	=	griend
			23	=	populierenopstand
			24	=	bouwland
			25	=	weiland
			26	=	boomgaard
			27	=	boomkwekerij
			28	=	heide
			29	=	zand
			30	=	overig bodemgebruik
			31	=	begraafplaats
			32	=	fruitwekerij
			35	=	water (grote oppervlakte)
			36	=	water (kleine opp)
			37	=	droogvallend
			38	=	steenglooiing/krib
			39	=	aanlegsteiger
			42	=	Rietmoeras(W)
			43	=	Rietmoeras(G)
		- 2013:
			0	=	buitenland
			1	=	bebouwing
			2	=	Auto(snel)weg
			3	=	Hoofdverbindingsroute
			4	=	verbindingsroute
			5	=	Lokale weg
			6	=	Ged. verharde weg
			7	=	onverharde weg
			8	=	voetgangersgebied
			9	=	rijwielpad
			10	=	voetpad
			11	=	parkeerterrein
			12	=	spoorweg
			13	=	verharding vliegveld
			14	=	spoorbaanlichaam
			15	=	kassen	
			19	=	loofbos
			20	=	naaldbos
			21	=	bos divers
			24	=	bouwland
			25	=	grasland
			26	=	boomgaard
			27	=	boomkwekerij
			28	=	heide
			29	=	zand
			30	=	overig bodemgebruik
			31	=	begraafplaats
			32	=	fruitwekerij
			36	=	water
			37	=	droogvallende grond
			41	=	grasland met riet
			42	=	overig terrein met riet
			43	=	water met riet
			61	=	agrarisch bebouwing
			62	=	agrarische erven

Language:	Dutch