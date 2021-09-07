#------------------------------------------------------------------------------
# Name:     11_soil_CCNL_prep.R
#
# Content:  - compile relevant soil property data and geographic locations of
#             sampling locations of the CCNL dataset
#           - export table of soil property data and 3D coordinates of the CCNL
#             dataset
#           
# Inputs:   - CCNL soil point raw data: data/soil/ccnl/CCNL_dataset.csv
#
# Output:   - CCNL soil point data re-structured and targeted towards soil
#             property target variables: out/data/soil/tbl_CCNL.Rds
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  November 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages ----------------------
gc()
rm(list=ls())

library(tidyverse)



### Read in CCNL data -------------------------------------------------------

# read in CCNL soil point data
tbl_CCNL <- read_csv("data/soil/ccnl/CCNL_dataset.csv")

# Most soil properties in CCNL dataset were predicted using Near Infrared Spectroscopy (NIRS)
# For each site/location: 1 topsoil (0-30 cm) and 1 subsoil (30-100) sample was taken
# see data/soil/ccnl/CCNL_dataset_Summary.xlsx Exel file for overview of CCNL dataset

# rename columns
tbl_CCNL <- tbl_CCNL %>% 
  rename(sample_id = X,            # what is difference between X and ID_EA?
         # = ID_EA,
         site_id = ID,
         site_id_WUR = ID_WUR,
         d_upper = bemdiepte_BOV,
         d_lower = bemdiepte_OND,
         clay_NIR_gkg = Lutum,         # NIR = Near Infrared Spectroscopy
         sand_NIR_gkg = Zand,          # gkg = g/kg (units)
         silt_NIR_gkg = Silt,
         SOM_NIR_gkg = SOM_NIRS,
         SOM_per = SOM_glv,            # per = % (units);
         # measurement method: "gloeiverlies" = Gluehverlust (in GER) = loss on ignition
         # = os_cor_glv,               # ???
         clay_gkg = Lutum_min,
         sand_gkg = zand_min,
         silt_gkg = silt_min,
         lime_NIR_gkg = KZK,           # *KZK = “Koolzure Kalk”, ~ carbonated lime
         SOC_NIR_gkg = SOC,
         TOC_NIR_gkg = TOC,
         N_tot_NIR_gkg = Ntotal,
         CaCEC_NIR_mmolkg = CaCEC,     # CaCEC = Ca cation exchange capacity
         CEC_NIR_mmolkg = CEC,         # mmolkg = mmol/kg (units)
         C_inorg_NIR_gkg = C_inorg,
         KCEC_NIR_mmolkg = KCEC,
         # = K_status,
         grain_size_NIR_microm = M50,  # microm = micrometers
         MgCEC_NIR_mmolkg = MgCEC,
         NaCEC_NIR_mmolkg = NaCEC,
         pH_NIR = pH,                  # predictions using NIR of pH in water, KCl or CaCl2 suspension?
         ASL_NIR_mgkg = Active_Soil_life, # ASL = Active Soil Life (potential N mineralization)
         # mgkg = mg/kg (units)
         S_tot_NIR_gkg = Stotal,
         N_dis_org_NIR_mgkg = DissolvedOrganicNitrogen,
         BD_gcm3 = Dichtheid_eind,     # BD = bulk density (is it really BD though?)
         # gcm3 = g/cm^3 (units)
         PLFA_tot_NIR_microgCg = PLFA_Total, # PLFA = ???
         # microgCg = microgram C / g (units)
         PLFA_fun_NIR_microgCg= PFLA_Fungi,
         PLFA_bac_NIR_microgCg= PFLA_Bacteria,
         N_tot_mgNkg = N_Totaal,       # measured using wet chemistry method (Dumas method)
         # mgNkg = mg N/kg (units)
         C_tot_per = C_Totaal,         # measured using wet chemistry method (Dumas method)
         C_org_per = C_Org,            # measured using wet chemistry method (Dumas method)
         weight_fresh_g = Gewicht_vers, # g = g (grams; units)
         weight_dry_g = Gewicht_droog,
         weight_diff_g = Gewicht_Verschil, # weight difference
         P_oxal_mmolPkg = P_oxalaat,   # mmolPkg = mmol P kg^-1 (units)
         Fe_oxal_mmolFekg = Fe_oxalaat,        # mmolPkg = mmol Fe kg^-1 (units)
         Al_oxal_mmolAlkg= Al_oxalaat,        # mmolPkg = mmol Al kg^-1 (units)
         P_Al = P_Al_klassiek,         # measured in P2O5 100g^-1 (units)
         P_CaCl2_mgPkg = P_CaCl2,      # mg P kg^-1
         FBV = Bindend_vermogen_FBV,   # binding capacity of the soil FBV
         # measured using conventional method in mmol P kg^-1 grond (units)
         sat_FVG_per = Verzadigingsgraad_FVG, # degree of saturation FVG (Verzadigingsgraad)
         # measured using conventional method in % (units)
         X = x,
         Y = y,
         soil_type = BC_2019,          # soil type (2019 soil map)
         stratum = steekstratum,       # ?
         subarea_soil_LU = deelgebied, # "Deelgebied" based on landuse (LU) and soil type
         # based on statistical area estimations
         strata_surface_ha = oppervlakte_ha, # in hectars (ha) (units)
         landuse_LGN18 = LG18)         # land use category based on LGN 2018 land use map

# I thought there was also penetration resistance measurements???

# nest variable columns for better overview and reorganize order of columns
tbl_CCNL <- tbl_CCNL %>% 
  # soil target properties: pH, CEC, Ctot, Corg, SOM, N, P, BD, clay, silt, sand
  nest(soil_target = c(pH_NIR, CEC_NIR_mmolkg, CaCEC_NIR_mmolkg, KCEC_NIR_mmolkg,
                       MgCEC_NIR_mmolkg, NaCEC_NIR_mmolkg, C_tot_per, C_org_per,
                       SOC_NIR_gkg, TOC_NIR_gkg, SOM_per, SOM_NIR_gkg, N_tot_mgNkg,
                       N_tot_NIR_gkg, N_dis_org_NIR_mgkg, P_oxal_mmolPkg,
                       P_CaCl2_mgPkg, P_Al, BD_gcm3, clay_gkg, clay_NIR_gkg,
                       silt_gkg, silt_NIR_gkg, sand_gkg, sand_NIR_gkg,
                       grain_size_NIR_microm),
       # order soil chemical properties in order of plant nutrient importance:
       # primary, seconday, tertiary macronutrients and then micronutrients
       soil_other = c(C_inorg_NIR_gkg, lime_NIR_gkg, K_status, S_tot_NIR_gkg,
                      Fe_oxal_mmolFekg, Al_oxal_mmolAlkg, ASL_NIR_mgkg,
                      PLFA_tot_NIR_microgCg, PLFA_fun_NIR_microgCg,
                      PLFA_bac_NIR_microgCg, FBV, sat_FVG_per, weight_fresh_g,
                      weight_dry_g, weight_diff_g),
       # environmental factors at locations from which samples originate
       env_fact = c(soil_type, stratum, subarea_soil_LU, strata_surface_ha,
                    landuse_LGN18),
       # remaining variables (some of which are unknown)
       unknown = c(ID_EA, site_id_WUR, os_cor_glv)) %>% 
       # There is almost no metadata (date, comments, methods, etc.)
  # order columns of nested tbl: ID, coordinates, horizon/depth, target soil properties,
  # other nested cols and names of unknown cols
  select(sample_id, site_id, X, Y, d_upper, d_lower, soil_target, soil_other,
         env_fact, unknown) %>% 
  # arrange by site from topsoil to increasing depth
  arrange(sample_id, d_upper & d_lower)

# save tbl to disk
write_rds(tbl_CCNL, "out/data/soil/tbl_CCNL.Rds")


