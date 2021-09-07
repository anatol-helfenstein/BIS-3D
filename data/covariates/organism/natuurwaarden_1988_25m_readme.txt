#### Readme for land cover / vegetation type (Natuurwaardenkaart 1988 vlakken) ####
By A.F. Helfenstein
2020-03-27


#### SOURCE FILE ####
Name:			Natuurwaardenkaart_1988_vlakken.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Natuur)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		1988 (?)
Spatial 
coverage:		National (Netherlands)
Projection:		RD_new
Content:		land cover / vegetation type:
					1: Schor of kwelder
					2: Moeras (excl. Moerasbos)
					3: Nat schraalland
					4: Droog schraalland
					5: Droge of natte heide
					6: Levend rustend, ontwaterd of vergraven hoogveen
					7: bos of struweel
					8: Natte grond, overw. grasland
					9: zout water
					10: Zoet of brak water
					11: combi. 2, 3
					12: combi. 3, 8
					13: combi. 2, 8
					14: combi. 3, 4
					15: combi. 4, 5
					16: combi. 7, 5
					17: combi. 7, 4
					18: combi. 4, 10
					19: combi. 3, 4, 5
					20: combi. 4, 6
					21: combi. 3, 5
					22: combi. 3, 7
				* very few areas are one of the combinations, so I suggest aggregating all of them into one group or to other groups



Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "type") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "Natuurwaardenkaart_1988_vlakken_PolygonToRaster" to "natuurwaarden_1988_25m"



#### FINAL FILE ####
Name:			natuurwaarden_1988_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
			W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Gridsize:		25m x 25m
Temporal
coverage:		1988 (?)
Spatial 
coverage:		National
Projection:		RD_new

Content:		land cover / vegetation type:
					1: Schor of kwelder
					2: Moeras (excl. Moerasbos)
					3: Nat schraalland
					4: Droog schraalland
					5: Droge of natte heide
					6: Levend rustend, ontwaterd of vergraven hoogveen
					7: bos of struweel
					8: Natte grond, overw. grasland
					9: zout water
					10: Zoet of brak water
					11: combi. 2, 3
					12: combi. 3, 8
					13: combi. 2, 8
					14: combi. 3, 4
					15: combi. 4, 5
					16: combi. 7, 5
					17: combi. 7, 4
					18: combi. 4, 10
					19: combi. 3, 4, 5
					20: combi. 4, 6
					21: combi. 3, 5
					22: combi. 3, 7
				* very few areas are one of the combinations, so I suggest aggregating all of them into one group or to other groups


Language:		Dutch