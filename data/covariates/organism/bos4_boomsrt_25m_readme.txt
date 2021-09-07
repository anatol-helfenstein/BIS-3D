#### Readme for tree species (boom soort) in forest areas ####
By A.F. Helfenstein
2020-03-25


#### SOURCE FILE ####
Name:			bos4_hb_soort.lyr
Source:			GeoDesk (\\WUR\dfs-root\ESG\Shares\Arc\LayerFiles\NatureEnvironment_NatuurMilieu\Bos)
Format: 		ESRI format LayerFile
Type:			Polygons
Availability:
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new
Content:		tree species in forest areas in the Netherlands:
					0:
					11: Grove den - Pinus sylvestris
					12: Corsicaanse den - Pinus nigra laricio
					13: Oostenrijkse den Pinus nigra nigra
					14: Weymouth den - Pinus strobus
					15: Draaiden - Pinus contorta
					16: Doornden - Pinus rigida
					17: Zeeden - Pinus pinaster
					19: Overigve dennen - Pinus spec.
					21: Douglas spar - pseudotsuga menziesii
					31: Japanse lariks - Larix kaempferi
					32: Europese lariks - Larix decidua
					41: Fijnspar - Picea abies
					42: Sitkaspar - Picea sitchensis
					43: Omorikaspar - Picea omorika
					49: Overige sparren - Picea spec.
					51: Tsuga heterophylla
					52: Thuja spec.
					53: Reuzenzilverspar - Abies grandis
					54: Gewone zilverspar - Abies alba
					55: Schijncypres - Chamecyparis spec.
					56: Jeneverbes - Juniperus communis
					59: Overige naaldbomen - conifers
					61: Inlandse eik - Quercus robur
					62: Amerikaanse eik - Quercus rubra
					63: Overige eiken - Quercus spec.
					70: Overige inheemse loofboomsoorten
					71: Beuk - Fagus sylvatica
					81: Populier - Populus spec.
					82: Wilg - Salix spec.
					91: Berk - Betula spec.
					92: Es - Fraxinus spec.
					93: Zwarte els - Alnus glutinosa
					94: Gewone esdoorn (Acer pseudoplatanus) en Spaanse aak (Acer campestre)
					95: Acacia - Robinia pseudoacacia
					96: Zoete kers - Prunus avium
					97: Haagbeuk - Carpinus betulus
					98: lep - Ulmus spec.
					99: Overige uitheemse loofboomsoorten
					<all other values>


Language:		Dutch
Terms-of use:	



#### PROCESSING ####
1. Polygon to raster (target attribute = "BOOMSRT") using existing DEM (AHN2) to snap the raster to (25m x 25m resolution)
* snap raster = all cells are designated to same geographical location

4. Raster to other format (.tif)

5. Change name from "bos4_hb_PolygonToRaster" to "bos4_boomsrt_25m"



#### FINAL FILE ####
Name:				bos4_boomsrt_25m.tif
Location:		W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus_GIS\Preprocessing
				W:\ESG\DOW_SGL\Research_PhD\AnatolHelfenstein\project\BISplus\data\covariates\processed\organism
Format: 		geoTIFF
Type:			Raster
Temporal
coverage:		?
Spatial 
coverage:		National
Projection:		RD_new

Content (EN):	tree species in forest areas in the Netherlands:
					0:
					11: Grove den - Pinus sylvestris
					12: Corsicaanse den - Pinus nigra laricio
					13: Oostenrijkse den Pinus nigra nigra
					14: Weymouth den - Pinus strobus
					15: Draaiden - Pinus contorta
					16: Doornden - Pinus rigida
					17: Zeeden - Pinus pinaster
					19: Overigve dennen - Pinus spec.
					21: Douglas spar - pseudotsuga menziesii
					31: Japanse lariks - Larix kaempferi
					32: Europese lariks - Larix decidua
					41: Fijnspar - Picea abies
					42: Sitkaspar - Picea sitchensis
					43: Omorikaspar - Picea omorika
					49: Overige sparren - Picea spec.
					51: Tsuga heterophylla
					52: Thuja spec.
					53: Reuzenzilverspar - Abies grandis
					54: Gewone zilverspar - Abies alba
					55: Schijncypres - Chamecyparis spec.
					56: Jeneverbes - Juniperus communis
					59: Overige naaldbomen - conifers
					61: Inlandse eik - Quercus robur
					62: Amerikaanse eik - Quercus rubra
					63: Overige eiken - Quercus spec.
					70: Overige inheemse loofboomsoorten
					71: Beuk - Fagus sylvatica
					81: Populier - Populus spec.
					82: Wilg - Salix spec.
					91: Berk - Betula spec.
					92: Es - Fraxinus spec.
					93: Zwarte els - Alnus glutinosa
					94: Gewone esdoorn (Acer pseudoplatanus) en Spaanse aak (Acer campestre)
					95: Acacia - Robinia pseudoacacia
					96: Zoete kers - Prunus avium
					97: Haagbeuk - Carpinus betulus
					98: lep - Ulmus spec.
					99: Overige uitheemse loofboomsoorten
					<all other values>



Language:		Dutch