#### Readme for state forests categorized by recreational use (e.g. Staatsbosbeheer_[year]_recreatie_doelstellingen.lyr) ####
By A.F. Helfenstein
2020-03-18


#### SOURCE FILE ####
Name:		Staatsbosbeheer 2005 recreatie doelstellingen.lyr
			Staatsbosbeheer 2006 recreatie doelstellingen.lyr
			Staatsbosbeheer 2007 recreatieve doelstellingen.lyr
			Staatsbosbeheer_2008_recreatie_doelstellingen.lyr
			Staatsbosbeheer_2009_recreatie_doelstellingen.lyr
			Staatsbosbeheer 2010 Recreatie.lyr
Source:		GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\AdministrativeDivisions_Administratief\NatuurTerrein\StaatsBosBeheer)
Format: 	ESRI format LayerFile
Type:		Polygons
Availability:
Temporal
coverage:	2005-2010
Spatial 
coverage:	National
Projection:	RD_new
Content (NE):

Content (EN):	- areas with state forest management (in polygons) categorized by recreational aims/use
				- variables of interest in attribute tables: "SPE_CODE_T" or "STDR_CODE" and "SRD_OMS"

Language:	Dutch
Terms-of use:	



#### PROCESSING ####
1. Use the ArcGIS "Merge" geoprocessing tool to merge common attributes of interest. First merge "SPE_CODE_T" attributes from layerfiles of years 2005 and 2007.

2. In new merged layer named "bos_recreatie_05_07", rename "SPE_CODE_T" to "STDR_CODE" (by creating new field and using the field calculator to assign all rows the values of variable "SPE_CODE_T")

3. Merge "bos_recreatie_05_07" with layerfiles from remaining years (2006, 2008-2010) by attribute "STDR_CODE".

3. Polygon to raster (target attribute = "STDR_CODE") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "bos_recreatie_Merged_PolygonToRaster" to "bos_recreatie_25m"



#### FINAL FILE ####
Name:		bos_recreatie_25m.tif
Location:	W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 	geoTIFF
Type:		Raster
Temporal
coverage:	2005-2010	
Spatial 
coverage:	National
Projection:	RD_new

Content (EN):	Areas with state forest management (in polygons) categorized by recreational aims/use ("STDR_CODE")
					blank = <overige waarden> or <all other values>
					0 = <overige waarden> or <all other values>
					0.1 = "niet doelstellingsgericht m.b.t. recreatie"
					1.1 = "Afgesloten, geen recreatieve betekenis"
					10.1 = "Natuur- en activiteitencentrum (NAC)"
					11.1 = "Speelbos"
					2.1 = "Beleefbaar"
					3.1 = "Tijdelijk opengesteld, laag niveau"
					4.1 = "Opengesteld, laag niveau"
					5.1 = "Opengesteld, basisniveau"
					6.1 = "Opengesteld, niveau plus"
					6.2 = "Opengesteld, niveau druk"
					7.1 = "Waterrecreatie, kleine recreatievaart"
					7.2 = "Waterrecreatie, grote recreatievaart"
					8.1 = "Natuurkampeerterrein"
					9.1 = "Groepskampeerterrein"
					R00.01 = "Afgesloten"
					R00.02 = "Beleefbaar"
					R01.01 = "Beleefbaar"
					R02.01 = "Tijdelijk Afgesloten"
					R02.02 = "Opengesteld, laag niveau"
					R02.03 = "Waterrecreatie, kleine recreatievaart"
					R02.04 = "Waterrecreatie, grote recreatievaart"
					R03.01 = "Opengesteld, basis"
					R03.02 = "Opengesteld, plus"
					R03.03 = "Opengesteld, druk"
					R03.04 = "Waterrecreatie, kleine recreatievaart"
					R03.05 = "Waterrecreatie, grote recreatievaart"
					R04.01 = "Beleefbaar"
					R04.02 = "Opengesteld, laag"
					R04.03 = "Opengesteld, basis"
					R04.04 = "Opengesteld, plus"
					R04.05 = "Opengesteld, druk"
					R05.01 = "Natuurkampeerterrein"
					R05.02 = "Groepskampeerterrein"
					R05.03 = "Natuur- en activiteitencentrum (NAC)"
					R05.04 = "Speelbos"
					R99.00 = "niet doelstellingsgericht m.b.t. recreatie"



Language:	English