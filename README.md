# BIS-3D

This repository contains scripts and metadata used in the manuscript:

Helfenstein, A., Mulder, V.L., Heuvelink, G.B.M., Okx, J.P. Tier 4 maps of soil pH at 25 m resolution for the Netherlands. Submitted, 2021.



## Dataset of model outputs (soil pH maps [25m])

The published dataset of the soil pH maps (model outputs) produced using the workflow in this repository are available for download: [Helfenstein et al. 2021, dataset](https://doi.org/10.4121/16451739.v1). The GeoTIFFs can e.g. be opened in a GIS software. The dataset includes mean predictions and estimates of the 5^th, 50^th (median), and 95^th quantile, as well as the 90% prediction interval (PI90) and a categorical map of the accuracy thresholds ("none", A, AA, AAA) based on *GlobalSoilMap* specifications for Tier 4 products. These maps are available for six standard depth layers from 0 cm to 5 cm, 5 cm to 15 cm, 15 cm to 30 cm, 30 cm to 60 cm, 60 cm to 100 cm and 100 cm to 200 cm, although the calibrated model can be used to predict at any user-defined depth layer between 0 m and 2 m.



## Model workflow (R scripts)


### Soil data preparation

* [10_db_connect_ODBC.R](10_db_connect_ODBC.R) -
* [11_soil_CCNL_prep.R](11_soil_CCNL_prep.R) - 
* [11_soil_PFB_BPK_LSK_prep.R](11_soil_PFB_BPK_LSK_prep.R) - 
* [12_soil_BIS_master_tibble.R](12_soil_BIS_master_tibble.R) - 
* [15_soil_BIS_expl_analysis_BPK.Rmd](15_soil_BIS_expl_analysis_BPK.Rmd) - 
* [15_soil_BIS_expl_analysis_LSK_CCNL.Rmd](15_soil_BIS_expl_analysis_LSK_CCNL.Rmd) - 
* []() - 


### Covariate preparation


* []() - 


### Model calibration (training)

* []() - 
* []() - 
* []() - 


### Model evaluation (validation or test)

* []() - 
* []() - 
* []() - 


### Soil maps

* []() - 
* []() - 
* []() - 



## Summary of other scripts, files and directories

* [R/other](R/other/) - R scripts that are not part of core modelling workflow, i.e. do not need to be re-run when adding new data or changing model parameters.
  + [QRF_comparison_approaches.R](R/other/QRF_comparison_approaches.R) - Benchmark and test different QRF training and predict functions from different packages
  + [color_schemes.R](R/other/color_schemes.R) - Explore different color schemes for continuous and categorical covariates and soil maps
  + [compare_AHN.R](R/other/compare_AHN.R) - Compare different AHN (digital elevation model (DEM) of the Netherlands) versions by subtracting rasters
  + [extract_from_raster_comparison.R](R/other/extract_from_raster_comparison.R) - Benchmark and compare different extracting methods (i.e. extracting covariate data at soil observation locations)
  + [make_recl_tbl.R](R/other/make_recl_tbl.R) - Make reclassifying metadata table for each categorical covariate based on ID values in raster S4 attribute table. This script was only run once to create a unique template for each categorical covariate reclassification table **prior** to manually describing and aggregating each class ID. ***CAUTION: Running this script will overwrite all the classes of each categorical covariate that were manually inserted based on expert knowledge!***
  + [merge_dbl_cols.R](R/other/merge_dbl_cols.R) - Tests how to merge double columns in soil point data tabular data in a sensible way
  + [predict_QRF_soil_maps_tiles.R](R/other/predict_QRF_soil_maps_tiles.R) - Predict soil property for each tile of tiled raster stack and then merge back together to get one map of all of the Netherlands (***Note:*** tiling may be useful for users with RAM issues)
  + [predict_qrf_fun.R](R/other/predict_qrf_fun.R) - Modify tweaked version of ranger function so that mean can also be computed directly from QRF (slightly altered from [ISRIC's tweaked QRF predict function](https://git.wur.nl/isric/soilgrids/soilgrids/-/blob/master/models/ranger/predict_qrf_fun.R))
  + [target_prediction_depth_GSM.R](R/other/target_prediction_depth_GSM.R) - Create target *GSM* prediction depth layers
  + [tiling_rasters.R](R/other/tiling_rasters.R) - Functions (sequential and parallel) to tile rasters (***Note:*** tiling may be useful for users with RAM issues; see also [split raster into tiles](https://stackoverflow.com/questions/52484216/split-a-raster-into-5-pixel-x-5-pixel-tiles-in-r))

* [data](data/) - Input data for modelling workflow. ***Note:*** Due to data restrictions, we can only provide metadata (e.g. README and reclassification table files), and not the soil data and covariates themselves. For accessing soil point data, please see the [Dutch National Key Registry of the Subsurface (BRO, in Dutch)](https://basisregistratieondergrond.nl/)) and more detailed information about the [BIS database from Wageningen Environmental Research](https://www.wur.nl/nl/Onderzoek-Resultaten/Onderzoeksinstituten/Environmental-Research/Faciliteitentools/Bodemkundig-Informatie-Systeem-BIS-Nederland.htm). Most of the covariates were attained from the [GeoDesk of Wageningen University and Research](https://www.wur.nl/en/research-results/research-institutes/environmental-research/facilities-tools/geodesk.htm).
  + [covariates](data/covariates/) - metadata (e.g. README and reclassification table files) of covariates, which are based on the soil forming factors climate, geology, organism and relief (***Note:*** The soil forming factor time is incorporated indirectly in the other soil forming factors since the covariates represent data from different times).
    ++ [climate](data/covariates/climate/) - README files ([...]_readme.txt) of covariates related to soil forming factor climate
    ++ [geology](data/covariates/geology/) - README, reclassification table ([...]_reclassify.csv and [...]_reclassify.xlsx) and attribute table of original classes ([...]_attributes.csv) files of covariates related to soil forming factor geology/parent material
    ++ [organism](data/covariates/organism/) - README, reclassification table, attribute table of original classes and any other metadata files of covariates related to soil forming factor organism (including land cover and land use)
    ++ [relief](data/covariates/relief/) - README and reclassification table files of covariates related to soil forming factor relief/topography
    ++ [soil](data/covariates/soil/) - README and any other files of soil maps (***Note:*** These were not used in modelling framework since we did not want to model/map soil properties with existing soil maps and instead only used the soil forming factors)
    ++ [covariates_metadata.csv](data/covariates/covariates_metadata.csv) - Summary metadata table of all covariates used
    ++ [covariates_metadata.xlsx](data/covariates/covariates_metadata.xlsx) - Summary metadata table of all covariates used
  + [covariates](data/other/) - Other spatial data not used as covariates (e.g. table of land use classes as designated in BIS database, shapefiles of provincial and country borders and probability sample strata and mapping mask used to assign "no data" values)

* [out](out/) - Intermediary (e.g. changes made to input data) and final model outputs
  + [out/data/covariates/DEM_derivatives](out/data/covariates/DEM_derivatives/) - DEM derivatives produced from AHN2 using [SAGA GIS](http://www.saga-gis.org/) functions in script [21_cov_dem_deriv_saga.R](21_cov_dem_deriv_saga.R), based on approach of [Hengl & MacMillan, 2019](www.soilmapper.org)
  + [out/data/model](out/data/model/) - Model evaluation and hypertuning outputs
  + [out/figs/explorative](out/figs/explorative/) - Exploratory analysis and descriptive plots of modelling input soil point data
  + [out/figs/models/pH_KCl](out/figs/models/pH_KCl/) - Model evaluation plots: accuracy plots (i.e. predicted vs. observed)) and metrics (mean error (ME), mean squared error (MSE), root mean squared error (RMSE), model efficiency coefficient (MEC), prediction interval coverage probability (PICP)) of different accuracy assessment (statistical validation) strategies^[4 external accuracy assessment, i.e. statistical validation strategies were computed: PFB = "Profielbeschrijving" dataset, LSK = "Landelijke Steekproef Kaarteenheden" dataset, OOB = out-of-bag, CV = cross-validation, SRS = simple stratified random sample. For more information see [Helfenstein et al., submitted]()], model residuals over space per depth layer, and variable importance measures.
  + [out/maps/explorative](out/maps/explorative/) - Exploratory analysis maps of BIS soil point data, AHN and AHN derivatives
  + [out/maps/other/SoilGrids_v2.0/SoilGrids_phh2o_model_evaluation_LSK_SRS_d.csv](out/maps/other/SoilGrids_v2.0/SoilGrids_phh2o_model_evaluation_LSK_SRS_d.csv) - Model evaluation accuracy metrics of SoilGrids version 2.0 soil pH [H~2~O] maps of the Netherlands ([Poggio et al., 2021](https://doi.org/10.5194/soil-7-217-2021)).
  + [out/maps/target/pH_KCl/GeoTIFFs](out/maps/target/pH_KCl/GeoTIFFs/) - tif.aux.xml files of pH [KCl] model output maps (***Note:*** Actual maps (GeoTIFFs) can be downloaded here: [Helfenstein et al. 2021, dataset](https://doi.org/10.4121/16451739.v1))
  + [out/maps/target/pH_KCl/pdf](out/maps/target/pH_KCl/pdf/) - PDFs of pH [KCl] maps



## References

Helfenstein, A., Mulder, V.L., Heuvelink, G.B.M., Okx, J.P. Tier 4 maps of soil pH at 25 m resolution for the Netherlands. Submitted, 2021.

Helfenstein, Anatol; Mulder, Vera L.; Heuvelink, Gerard B.M.; Okx, Joop P. (2021): Tier 4 maps of soil pH at 25 m resolution for the Netherlands. 4TU.ResearchData. Dataset. [https://doi.org/10.4121/16451739.v1](https://doi.org/10.4121/16451739.v1)

Hengl, T., MacMillan, R.A., 2019. Predictive Soil Mapping with R. OpenGeoHub foundation, Wageningen, the Netherlands. [www.soilmapper.org](www.soilmapper.org).

[ISRIC's tweaked QRF predict function](https://git.wur.nl/isric/soilgrids/soilgrids/-/blob/master/models/ranger/predict_qrf_fun.R)

Poggio, L., de Sousa, L.M., Batjes, N.H., Heuvelink, G.B.M., Kempen, B., Ribeiro, E., Rossiter, D., 2021. SoilGrids 2.0: Producing soil information for the globe with quantified spatial uncertainty. SOIL 7, 217–240. doi:[https://doi.org/10.5194/soil-7-217-2021](https://doi.org/10.5194/soil-7-217-2021).

[Project website](https://www.wur.nl/en/project/Developing-a-high-resolution-4-dimensional-soil-modelling-and-mapping-platform-for-the-Netherlands-BIS-4D.htm)

[split raster into tiles](https://stackoverflow.com/questions/52484216/split-a-raster-into-5-pixel-x-5-pixel-tiles-in-r)

[Video supplement](https://www.youtube.com/watch?v=ENCYUnqc-wo)



## Funding

This project ([WOT-04-013-010](https://research.wur.nl/en/projects/soil-property-mapping-wot-04-013-010)) was financed by the Dutch Ministry of Agriculture, Nature and Food Quality.