#### Readme for topsoil (bovengrond ?) based on geomorphological map of the Netherlands (2019 version)) ####
By A.F. Helfenstein
2020-04-02


#### SOURCE FILE ####
Name:			GMK2019-GeoMorfologie.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\Soil_Bodem\Geomorfologie\v2019)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- see website with legend info: https://legendageomorfologie.wur.nl/
				- topsoil classes (categorical) based on geomorphological map
					1	=	<blank>
					2	=	yl
					3	=	dl
					4	=	ydl
					5	=	ydc
					6	=	yov
					7	=	yd
					8	=	x
					9	=	yo
					10	=	ys
					11	=	ykd
					12	=	v
					13	=	d
					14	=	gd
					15	=	yms
					16	=	ov
					17	=	k
					18	=	w
					19	=	i
					20	=	dyc
					21	=	kv
					22	=	tyd
					23	=	yc
					24	=	dw
					25	=	ms
					26	=	s
					27	=	r
					28	=	rd
					29	=	ytd
					30	=	l

Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "Bovengrond_code") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)

4. Raster to other format (.tif)

5. Change name from "GMK2019GeoMorfologie_PolygonToRaster_bovengrond" to "geomorph2019_bovengrond_25m"



#### FINAL FILE ####
Name:			geomorph2019_bovengrond_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\geology
Format: 		GeoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		(2019 version)
Spatial 
coverage:		National / Netherlands
Projection:		RD_new

Content:		- see website with legend info: https://legendageomorfologie.wur.nl/
				- topsoil classes (categorical) based on geomorphological map
					1	=	<blank>
					2	=	yl
					3	=	dl
					4	=	ydl
					5	=	ydc
					6	=	yov
					7	=	yd
					8	=	x
					9	=	yo
					10	=	ys
					11	=	ykd
					12	=	v
					13	=	d
					14	=	gd
					15	=	yms
					16	=	ov
					17	=	k
					18	=	w
					19	=	i
					20	=	dyc
					21	=	kv
					22	=	tyd
					23	=	yc
					24	=	dw
					25	=	ms
					26	=	s
					27	=	r
					28	=	rd
					29	=	ytd
					30	=	l

Language:		Dutch