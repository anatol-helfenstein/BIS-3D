#------------------------------------------------------------------------------
# Name:     11_soil_PFB_BPK_LSK_prep.R
#
# Content:  - connect to BIS (currently version 7.4) Oracle database (DB) using
#             oracle client basic and odbc driver
#           - query DB using dplyr syntax
#           - compile relevant soil property data and geographic locations of
#             sampling locations
#           - export table of soil property data and 3D coordinates          
#
# Inputs:   - none
#
# Output:   - table (tbl) of soil property data and 3D coordinates from the 
#             PFB, BPK, and LSK datasets in BIS: out/data/soil/tbl_[dataset].Rds
#
# Project   BIS+ 
# Author:   Anatol Helfenstein
# Updated:  November 2020

# [...] removed for data privacy reasons



### install & load required packages ---------------------------------------

pkgs <- c("tidyverse", "here", "DBI", "odbc", "dbplyr") #"ROracle"
lapply(pkgs, library, character.only = TRUE)



### connect to BIS database (DB) --------------------------------------------

# Verify that odbc recognizes the installed drivers
# Details of connection are in tnsnames.ora file (host, port, etc.)
drv <- unique(odbcListDrivers()[,1])

# check if R session is running using R i386 (32bit) or 64 bit & choose Oracle client accordingly
# in Win OS, there are oracle clients for 32 bit and others for 64 bit
Sys.info()
Sys.getenv("R_ARCH")

# Connect to BIS DB using login credentials
odbc_con <- DBI::dbConnect(odbc::odbc(),
                           Driver = drv,
                           DBQ = "[...]",
                           UID = rstudioapi::askForPassword("Database user"),
                           PWD = rstudioapi::askForPassword("Database password"))



### Relevant information in BIS ------------------------------------------------

# To see how BIS DB is organized, see BIS flowchart / organigram ("./db/bis_flowchart.jpg")
# Dennis created "./db/inventory.pdf" to list all useful (primary & secondary) tables in BIS (10 yr ago)
# See overview of BIS attributes / variables/ soil properties ("./db/BISAttributen.xlsx")

# The DB can be split into projects BPK, PFB and LSK. 
# BPK = soil profile descriptions (boring) -> field observations (?)
# PFB = soil pit descriptions -> lab analysis (and field observations) (?)
# LSK = soil field observations and lab analysis; stratified random sampling design (validation set)

# Each project has subdirectories:
# 1) ALG (algemeen): general information such as sample IDs and coordinates
# 2) LAAG (horizons/layers): information on soil horizons (field observations?)
# 3) MONSTER (=sample): samples collected & lab analysis data. For PFB, these are split into:
#       a) CHE: ???
#       b) CHT: soil chemical data
#       c) GRA: soil granular data
#       d) FYS: soil physical data

# LSK lab data is all in one table "LSK_ANALYSE"
# BPK_LAAG does not contain any lab data (no MONSTER), only field observations
# For heavy metal data of the Netherlands, contact Gerben Mol (geochemistry background)

# stay in overall scheme "BISUSER" according to Hugo de Groot (?)

# PROJ contains names and information on different soil sampling projects since 1960s
tbl(odbc_con, in_schema("BISUSER", "PROJ"))



### BPK point soil data ----------------------------------------------------

# Once you figure out what data you really need from the database,
# use collect() fnc to pull data into local tbl (can take a while)

# let's first look at BPK field observation data from soil horizons
system.time(
  tbl_BPK_LAAG <- tbl(odbc_con, in_schema("BISUSER", "BPK_LAAG")) %>%
    collect() %>% 
    # re-organize columns
    select(BPK_ID, LAAG_NR, BOVENGRENS, ONDERGRENS, HOR_CODE, ORG_STOF, LUTUM,
           SILT, LEEM, GRIND, M50, MENGVERH, AARD_ORG, VEEN_C, KALK, RIJPING,
           GEO_FOR_C, OPMERKING, KVERZ_S, C, D, BOOR_METH, SCHELP, GELDIG_VAN, OBJECTID)
  
)
# should take less than a minute

# match soil property data to coordinates of locations where observations were made
tbl(odbc_con, in_schema("BISUSER", "BPK_ALG"))
# Error: nanodbc/nanodbc.cpp:1617: 00000: 
# <SQL> 'SELECT *
# FROM (BISUSER.BPK_ALG) "zzz21"
# WHERE (0 = 1)'

# This error is because there is BLOB (binary large object) data within this table:
# spatial data: SHAPE (SDO_GEOMETRY datatype) and SE_ANNO_CAD_DATA (BLOB datatype)
# so query tbl and select only necessary columns

# get column names from here
tbl(odbc_con, in_schema("BISUSER", "BPK_ALG_VERSIE"))

tbl_BPK_ALG <- dbGetQuery(odbc_con, 'SELECT BPK_ID, OBJECTID, PUNT_ID, PROJ_ID,
                          X, Y, CR_NR, TKRT_C, KARTEERDER, JAAR, MAAND, KROON,
                          HOOGTE, BODEM_C, STPC_VOOR, STPC_SUB, STPC_CIJF,
                          STPC_KALK, STPC_ACHT, STPC_VERG, GHG, GLG, STPC_GT,
                          BEW, HERKOMST, OPMERKING, GT_C, GHG_DIEPERDAN,
                          GLG_DIEPERDAN, DRAINAGE, A, B, DATUM, MVHOOGTE
                          FROM BISUSER.BPK_ALG') %>%
  as_tibble()
# check using SQL software at WUR if this is indeed all variables (we need)

# combine laag and alg tbls and rename to english, understandable names (see "db/BISAttributen.xlsx")
tbl_BPK <- full_join(tbl_BPK_LAAG, tbl_BPK_ALG, by = "BPK_ID") %>% 
  rename(site_id = BPK_ID,
         hor_nr = LAAG_NR,
         d_upper = BOVENGRENS,
         d_lower = ONDERGRENS,
         hor = HOR_CODE,
         SOM = ORG_STOF,       # organic matter (OM). Is this really SOM? or e.g. OM of also litter?
         clay = LUTUM,
         silt = SILT,
         loam = LEEM,              # mixture of clay, silt, sand
         gravel = GRIND,
         grain_size = M50,         # ??
         org_type = AARD_ORG,      # organic matter type (?)
         peat_com = VEEN_C,        # peat composition (type)
         lime = KALK,
         maturation = RIJPING,
         base_sat = KVERZ_S,       # base saturation??
         method_auger = BOOR_METH, # EDM = Edelmanbor, GUT = guts (for peat soils), KUI = kuil (pits), PUL = puls (for loose sediments, etc.)
         shells = SCHELP,          # ??
         date_valid = GELDIG_VAN,
         comment_laag = OPMERKING.x,
         comment_alg = OPMERKING.y,
         obj_id_laag = OBJECTID.x,
         obj_id_alg = OBJECTID.y,
         point_id = PUNT_ID,
         proj_id = PROJ_ID,
         surveyor = KARTEERDER,
         year = JAAR,
         month = MAAND,
         elevation = HOOGTE,
         landuse = BODEM_C,
         profile_dist = STPC_VERG,
         gw_mean_high = GHG,
         gw_mean_low = GLG,
         gw_class = STPC_GT,
         par_mat_code = GEO_FOR_C,      # code for parent material
         root_d = BEW,
         origin = HERKOMST,
         gw_mean_high_up_bound = GHG_DIEPERDAN,   # upper boundary of mean highest groundwater level?
         gw_mean_low_up_bound = GLG_DIEPERDAN,   # upper boundary of mean lowest groundwater level?
         drain = DRAINAGE,
         date = DATUM,
         # = MENGVERH,
         # = KVERZ_S,
         # = C,
         # = D,
         # = CR_NR,
         # = TKRT_C,
         # = KROON,
         # = STPC_VOOR,
         # = STPC_SUB,
         # = STPC_CIJF,
         lime_stpc = STPC_KALK,
         # = STPC_ACHT,
         # = GT_C,
         # = A,
         # = B,
         elevation2 = MVHOOGTE)   # Difference between "HOOGTE" and "MVHOOGTE"?

# nest variable columns for better overview and reorganize order of columns
tbl_BPK <- tbl_BPK %>% 
  # soil target properties: pH, CEC, Ctot, Corg, SOM, N, P, BD, clay, silt, sand
  # only a few of these are in the BPK tbl
  nest(soil_target = c(SOM, clay, silt, loam, grain_size),
       # other soil properties that we will not map
       soil_other = c(gravel, org_type, peat_com, lime, lime_stpc, maturation,
                      base_sat, shells),
       # environmental factors at locations from which samples originate
       env_fact = c(elevation, elevation2, par_mat_code, landuse, root_d, gw_class,
                    gw_mean_high, gw_mean_low, gw_mean_high_up_bound,
                    gw_mean_low_up_bound, drain, profile_dist),
       # metadata of field observations and other parameters
       metadata = c(date, year, month, date_valid, surveyor, method_auger, origin,
                    obj_id_laag, obj_id_alg, point_id, proj_id, comment_laag,
                    comment_alg),
       # group remaining (mostly unknown) variables -- check with WENR colleagues
       unknown = c(MENGVERH, C, D, CR_NR, TKRT_C, KROON, STPC_VOOR,
                   STPC_SUB, STPC_CIJF, STPC_ACHT, GT_C, A, B)) %>% 
  # order columns of nested tbl: ID, coordinates, horizon/depth, target soil properties,
  # other nested cols and names of unknown cols
  select(site_id, X, Y, hor_nr, hor, d_upper, d_lower, soil_target, soil_other,
         env_fact, metadata, unknown)

# save tbl to disk
write_rds(tbl_BPK, "out/data/soil/tbl_BPK.Rds")



### PFB lab soil point data ----------------------------------------------------

# PFB_CHE -----------------------------------------------------------------

# read PFB_CHE table into working memory
tbl_PFB_CHE <- tbl(odbc_con, in_schema("BISUSER", "PFB_CHE")) %>% 
  collect()

# Is PFB_CHE something like an overview table of all PFB measurements made for each site
# this is not tidy data: variables of lab measurements (BEP_C) in rows;
# values = WAARDE, method used to measure = METHODE_C, laboratory name = LAB_C
tbl_PFB_CHE %>% 
  select(BEP_C, WAARDE, METHODE_C, LAB_C)

# tidying: convert row grouped by soil property into column
# first make sure each value of WAARDE, METHODE_C, LAB_C are uniquely identified
duplicate_val <- tbl_PFB_CHE %>% 
  pivot_wider(names_from = BEP_C,
              values_from = c(WAARDE, METHODE_C, LAB_C),
              values_fn = c(list(WAARDE = length),
                            list(METHODE_C = length),
                            list(LAB_C = length))) %>% 
  filter_at(vars(contains(c("WAARDE", "METHODE", "LAB"))), any_vars(. > 1)) %>% # union of matches is taken
# 22 observations for which there is more than 1 value (WAARD) per variable (duplicates)
  pull(MON_VNR)
  
# Differences between values of duplicate samples per soil property sometimes large!
tbl_PFB_CHE %>%
  group_by(MON_VNR, BEP_C) %>% 
  filter(MON_VNR == duplicate_val[2])

# better leave duplicate samples out than try to harmonize (due to large differences)
tbl_PFB_CHE <- tbl_PFB_CHE %>%
  filter(!MON_VNR %in% duplicate_val) %>% 
  pivot_wider(names_from = BEP_C,
              values_from = c(WAARDE, METHODE_C, LAB_C))

# change names of variables
names(tbl_PFB_CHE) <- gsub(x = names(tbl_PFB_CHE),
                           pattern = "-",
                           replacement = "_")

# still there are duplicated samples (that don't have duplicate lab measurements)
duplicate_id <- tbl_PFB_CHE[duplicated(tbl_PFB_CHE$MON_VNR),] %>% 
  pull(MON_VNR)

# these rows can be merged only when unequal variable of repeates samples are removed:
# BEP_DATUM, BEP_GR_O, WO_BEP_GR; then merge/fill rows together
tbl_PFB_CHE <- tbl_PFB_CHE %>% 
  select(-c(BEP_DATUM, BEP_GR_O, WO_BEP_GR)) %>% 
  group_by(MON_VNR) %>% 
  fill(GELDIG_VAN:LAB_C_S_TOT, .direction = "downup") %>% # all cols except grouped IDs
  ungroup() %>% 
  distinct()

# rename value variables and nest method and lab variables
# MON_VNR is link variable to other tbls, so don't change
tbl_PFB_CHE <- tbl_PFB_CHE %>% 
  rename_all(gsub, pattern = "WAARDE_", replacement = "") %>% 
  rename(date_valid = GELDIG_VAN) %>% 
  nest(method_lab_CHE = c(contains("METHODE_C")),
       lab_CHE = c(contains("LAB_C")))


# PFB_CHT -----------------------------------------------------------------

# read PFB_CHT table into working memory
# Rename to understandable names in english
# MON_VNR is link variable to other tbls, so don't change
tbl_PFB_CHT <- tbl(odbc_con, in_schema("BISUSER", "PFB_CHT")) %>% 
  collect() %>%
  rename(date_valid = GELDIG_VAN)
# what is "ZBV"?

# combine with PFB_CHE table
tbl_PFB_lab <- full_join(tbl_PFB_CHE, tbl_PFB_CHT, by = "MON_VNR")

# check if variables represented twice are always equal (same measurement)
tbl_PFB_lab %>% 
  select(PH_KCL.x, PH_KCL.y) %>% 
  filter(!PH_KCL.y %in% NA) %>% 
  mutate(diff_x.y = PH_KCL.x - PH_KCL.y) %>% 
  filter(!diff_x.y %in% 0) # 0 cases: so all values are the same except NA's
# tested this for other variables as well: HUMUS, N_TOT, PH_KCL, C_EL, FE_OX, P_TOT,
# PW_GET, CACO3, FE_DIT, CEC, AL_OX, P_OX, K_FIX, PYR, FE_TOT, AL_UIT, FBV, FE_UIT,
# CA_UIT, K_UIT, MG_UIT, NA_UIT, H_UIT

# check summary stats and which of the double cols contain more NA's
tbl_PFB_lab %>% 
  select(contains(c(".x", ".y"))) %>% 
  select(order(colnames(.))) %>% 
  summary()
# dates from PFB_CHE table have NA's while dates from PFB_CHT don't & only old dates in CHE

# Remove all .x variables since dates are incomplete & older and soil properties
# are almost all equal, with the exception of a few samples not harmonized correctly (see above)

tbl_PFB_lab <- tbl_PFB_lab %>% 
  select(-contains(".x")) %>% 
  # rename CHT columns that were doubles
  rename_all(gsub, pattern = ".y", replacement = "")

# QUESTION FOR DB managers: Is the CHE outdated i.e. old version of CHT?
# If not, instead of dropping all CHE columns that are double, should I average
# different values from same samples


## PFB_GRA -----------------------------------------------------------------

# read PFB_GRA table into working memory
tbl_PFB_GRA <- tbl(odbc_con, in_schema("BISUSER", "PFB_GRA")) %>% 
  collect() %>% 
  rename(date_bep = BEP_DATUM,
         date_valid = GELDIG_VAN)

# join with PFB_CHE and PFB_CHT table
tbl_PFB_lab <- full_join(tbl_PFB_lab, tbl_PFB_GRA, by = "MON_VNR")

# check double variables
tbl_PFB_lab %>% 
  select(contains("date_valid")) %>% 
  summary() # so makes sense to drop validation date variable of PFB_GRA (.y)

tbl_PFB_lab <- tbl_PFB_lab %>% 
  select(-date_valid.y) %>% 
  rename(date_valid = date_valid.x)


## PFB_FYS -----------------------------------------------------------------

# read PFB_FYS table into working memory
tbl_PFB_FYS <- tbl(odbc_con, in_schema("BISUSER", "PFB_FYS")) %>% 
  collect() %>% 
  rename(date_bep = BEP_DATUM,
         date_valid = GELDIG_VAN)

# combine with other lab analysis PFB tables
tbl_PFB_lab <- full_join(tbl_PFB_lab, tbl_PFB_FYS, by = "MON_VNR")

# check double variables
tbl_PFB_lab %>% 
  select(contains("date_valid")) %>% 
  summary() # so makes sense to drop validation date variable of PFB_FYS (.y)

# combine date_bep variables with as little NAs as possible
tbl_PFB_lab <- tbl_PFB_lab %>% 
  mutate(date_bep = if_else(date_bep.x %in% NA & !date_bep.y %in% NA,
                            date_bep.y,
                            date_bep.x)) %>% 
  # remove old date_bep and date_valid variables
  select(-c(date_bep.x, date_bep.y, date_valid.y)) %>% 
  rename(date_valid = date_valid.x)


## Combine PFB lab measurements with MONSTER & ALG -----------------------------

# Link PFB soil lab measurements with horizon (PFB_LAAG) and humus (PFB_HUM) data
# via PFB_ID variable
tbl_PFB_MONST <- tbl(odbc_con, in_schema("BISUSER", "PFB_MONSTER")) %>% 
  collect()

tbl_PFB_lab <- full_join(tbl_PFB_MONST, tbl_PFB_lab, by = "MON_VNR") %>% 
  select(-GELDIG_VAN) # GELDIG VAN from MONSTER is identical to date_valid of _lab tbl

# Link soil property information to locations (coordinates) in PFB_ALG
# As with BPK_ALG, work around by querying tbl and selecting only necessary columns
# get column names from here
tbl(odbc_con, in_schema("BISUSER", "PFB_ALG_VERSIE"))

tbl_PFB_ALG <- dbGetQuery(odbc_con, 'SELECT PFB_ID, PROJ_ID, X, Y, PROJ_C, TKRT_C,
                          PB_VNR, SOORT_PB, TD_TKRT_C, PLAATS, MAAND, JAAR, OPSTEL_C,
                          STPC_VOOR, STPC_SUB, STPC_CIJF, STPC_KALK, STPC_ACHT,
                          STPC_VERG, STPC_GT, KE50_VOOR, KE50_LET, KE50_CIJF,
                          KE50_KALK, KE50_ACHT, KE50_VERG, KE50_HEL, GT_C, GHG,
                          GLG, GVG, STAMBUIS, WATERST, BODEM_C, BEW, KRIT_Z_S,
                          VEG_BAN_C, SITUATIES, A, B, OPMERKING, HERKOMST, VAK_C,
                          VAK_VNR, BOOMSOORT, VEGETATIE, GHG_DIEPERDAN, GVG_DIEPERDAN,
                          GLG_DIEPERDAN, DATUM, DRAINAGE, MVHOOGTE
                          FROM BISUSER.PFB_ALG') %>%
  as_tibble()
# check using SQL software at WUR if this is indeed all variables (we need)

# combine tbls and rename to english, understandable names
tbl_PFB_lab <- full_join(tbl_PFB_ALG, tbl_PFB_lab, by = "PFB_ID")

# Double columns [_], [_.x] and [_.y]
# check which of these are most complete
tbl_PFB_lab %>% 
  select(contains(".")) %>% 
  select(order(colnames(.))) %>% 
  summarise_all(funs(sum(is.na(.))))
# most complete: PB_VNR.x, PROJ_C.x, TKRT_C.x 

# delete columns of which there is double without NA's or where doubles bring no added values (PB_VNR.y)
tbl_PFB_lab <- tbl_PFB_lab %>% 
  select(-c(contains(".y"))) %>%
  rename(PROJ_C = PROJ_C.x,
         TKRT_C = TKRT_C.x,
         PB_VNR = PB_VNR.x)

# rename to english, understandable names (see "db/BISAttributen.xlsx")
# TO-DO: automate this using complete BIS attributes names csv file
tbl_PFB_lab <- tbl_PFB_lab %>% 
  rename(site_id = PFB_ID,
         proj_id = PROJ_ID,
         proj_c = PROJ_C,
         d_upper = MON_DIEPB,
         d_lower = MON_DIEPO,
         date = DATUM,
         date_mon = MON_DATUM,
         place = PLAATS,
         month = MAAND,
         year = JAAR,
         profile_dist = STPC_VERG,
         gw_class = STPC_GT,
         root_d = BEW,
         tree_type = BOOMSOORT,
         vegetation = VEGETATIE,
         hor = HOR_CODE,
         comment = OPMERKING,
         landuse = BODEM_C,
         gw_mean_high = GHG,
         gw_mean_low = GLG,
         origin = HERKOMST,
         gw_mean_high_up_bound = GHG_DIEPERDAN,   # upper boundary of mean highest groundwater level?
         gw_mean_low_up_bound = GLG_DIEPERDAN,   # upper boundary of mean lowest groundwater level?
         drain = DRAINAGE,
         elevation = MVHOOGTE,
         clay_m = LUTUM_M,
         silt_50m = SILT_50M,
         silt_63m = SILT_63M,
         loam_m = LEEM_M,
         sand_m = ZAND_M,
         grain_size_m = M50_M,
         gravel_m = GRIND_M,
         lime_stpc = STPC_KALK,
         lime_ke50 = KE50_KALK,
         clay_dep_ratio = LUT_SLIB,
         BD = RHO,
         BD_d_m = RHO_D_M,
         cap_rise_s = KRIT_Z_S,
         cap_rise_b = KRIT_Z_B,
         moist_sat = TH_SAT,
         hydr_cond_sat = K_SAT,
         P_ads = FBV,          # ads = adsorption
         K_fix = K_FIX,        # fix = fixation
         P_fix = P_FIX,        # Phosphorus fixation (in BISAttributen wrongly labelled as "Phosphate"?)
         NH4_ex = NH4_UIT,     # ex = exchangeable
         # CEC_eff = SOM_KAT,    # effective CEC
         K_HCL = K_HCL,        # HCl-extractable K
         Mg_NaCl= MG_KZO,      # Mg (NaCl extract)
         N_min = N_MIN,        # mineral nitrogen content (nitrate + ammonia)
         N_min_NH4 = N_MIN_AMM,
         N_min_NO3 = N_MIN_NIT,
         P2O5_ox = P2O5_MET,   # Phosphate content after oxidation
         P2O5_unox = P2O5_ZON, # Phosphate content before oxidation
         P_tot = P_TOT,        # (in BISAttributen wrongly labelled as "Phosphate"?)
         pw_getal = PW_GET,    # ???
         Ca_con = CA_CON,      # con = concentration
         Cd_con = CD_CON,
         Cr_con = CR_CON,
         Cu_con = CU_CON,
         Fe_con = FE_CON,
         K_con = K_CON,
         Mn_con = MN_CON,
         Na_con = NA_CON,
         Ni_con = NI_CON,
         NO3_con = NO3_CON,
         Pb_con= PB_CON,
         P_con = P_CON,
         pH = PH_CON,
         SO4_con = SO4_CON,
         Zn_con = ZN_CON,
         Fe_S2O4 = FE_DIT,     # S2O4^2- (dithionite) - extractable Fe
         Fe_HCl = FE_HCL,      # 10% HCl (hydrochloric acid) - extractable Fe
         C_tot = C_EL,         # totaal koolstofgehalte (c-elementair) = total C content
         # however, Fokke said C_EL = C bound to minerals and not equal to Ctot???
         N_tot = N_TOT,
         # CN_ratio = C_N,       # elementary C / total N
         Ca_ex = CA_UIT,
         CEC = CEC,
         H_ex = H_UIT,
         Mg_ex = MG_UIT,
         Mn_ex = MN_UIT,
         Al_oxal = AL_OX,        # oxalate-extractable Al
         Fe_oxal = FE_OX,        # oxalate-extractable Fe
         CaCO3 = CACO3,
         pH_H2O = PH_H2O,
         pH_KCl = PH_KCL,
         pH_CaCl2 = PH_CACL2,
         Al_tot = AL_TOT,
         Al_tot_lit = AL_TOT_S,  # total Al content in litter
         Ca_tot_lit = CA_TOT_S,  # total Ca content in litter
         Fe_tot = FE_TOT,
         K_tot = K_TOT,
         K_tot_lit = K_TOT_S,    # total K content in litter
         Mg_tot = MG_TOT,
         Mg_tot_lit = MG_TOT_S,  # total Mg content in litter
         Mn_tot = MN_TOT,
         Mn_tot_lit = MN_TOT_S,  # total Mn content in litter
         Na_tot = NA_TOT,
         Na_tot_lit = NA_TOT_S,  # total Na content in litter
         Si_tot = SI_TOT,
         Ti_tot = TI_TOT,
         Ti_tot_lit = TI_TOT_S,  # total Ti content in litter
         # par_mat = MOEDERM,      # parent material (moedermateriaal)
         As_tot = AS_TOT,
         Cd_tot = CD_TOT,
         Cr_tot = CR_TOT,
         Cu_tot = CU_TOT,
         Ni_tot = NI_TOT,
         Pb_tot = PB_TOT,
         Zn_tot = ZN_TOT,
         Co_tot = CO_TOT,
         Ag_tot = AG_TOT,
         Be_tot = BE_TOT,
         # As_pot = AS_POT,        # potential As content
         # Cd_pot = CD_POT,
         # Cr_pot = CR_POT,
         # Cu_pot = CU_POT,
         # Ni_pot = NI_POT,
         # Pb_pot = PB_POT,
         # Zn_pot = ZN_POT,
         Ba_tot = BA_TOT,
         Hg_tot = HG_TOT,
         F_tot = F_TOT,
         Br_tot = BR_TOT,
         SO4_tot = SO4_TOT,
         Cl_tot = CL_TOT,
         Se_tot = SE_TOT,
         V_tot = V_TOT,
         Te_tot = TE_TOT,
         Sn_tot = SN_TOT,
         Sb_tot = SB_TOT,
         Mn_oxal = MN_OX,
         Al_HNO3 = AL_HNO3,
         P_H2O = P_GET,          # p-getal(p-oplosbaar in water)
         Al_con = AL_CON,
         Cl_con = CL_CON,
         EC_sat = EC_VER,        # electrical conductivity in saturation extract
         Mg_con = MG_CON,
         NH4_con = NH4_CON,
         OP_con = OP_CON,        # orthophosphate concentration
         Si_con = SI_CON,
         C_org = TOC,            # total organic carbon
         C_org2 = TOC_2,
         Al_HCl = AL_HCL,        # 10% HCl-extractable Al
         SOM_CHT = HUMUS,        # (soil???) organic matter content (%) from PFB_CHT table
         Al_ex = AL_UIT,
         Fe_ex = FE_UIT,
         K_ex = K_UIT,
         Na_ex = NA_UIT,
         NaCl = NACL,
         Fe_HNO3 = FE_HNO3,
         Mo_tot = MO_TOT,
         Co_AZ = CO_AZ,
         Cu_SZ = CU_SZ,
         P_citr = P_CITR,
         P_water = P_WATER,
         # Al = AL,
         # Fe = FE,
         # Ca = CA,
         # Mg = MG,
         # K = K,
         # Na = NA,
         # Mn = MN,
         P_oxal = P_OX,          # P-oxalate
         Pyr = PYR,
         S_tot = S_TOT,
         Ca_tot = CA_TOT,
         Fe_tot_lit = FE_TOT_S,
         num = AANTAL,
         # = TKRT_C,
         # = PB_VNR,
         # = SOORT_PB,
         # = TD_TKRT_C,
         # = OPSTEL_C,
         # = STPC_VOOR,
         # = STPC_SUB,
         # = STPC_CIJF,
         # = STPC_ACHT,
         # = VEG_BAN_C,
         # = SITUATIES,
         # = A,
         # = B,
         # = VAK_C,
         # = VAK_VNR,
         # = GVG_DIEPERDAN,
         # = KE50_VOOR,
         # = KE50_LET,
         # = KE50_CIJF,
         # = KE50_ACHT,
         # = KE50_VERG,
         # = KE50_HEL,
         # = GT_C,
         # = GVG,
         # = STAMBUIS,
         # = WATERST,
         # = MON_VNR,
         # = MONSTER_T,
         # = VOCHT,
         # = P_CITR,
         # = A_CYF,
         # = P_WATER,
         # = S_SO4,
         # = ZBV,
         # = SOORT_GRA,
         # = MOP_NR, 
         # = F2,
         # = F2_4,
         # = F4_8,
         # = F8_16,                
         # = F2_16,
         # = F16_25,
         # = F25_35,
         # = F35_50,
         # = F16_50,               
         # = F50,
         # = F50_75,
         # = F75_105,
         # = F50_105,
         # = F105_150             
         # = F150,
         # = F150_210,
         # = F210,
         # = F210_300,
         # = F300,
         # = F300_420,
         # = F420_600,
         # = F600_850,
         # = F850_1200,
         # = F12001700,
         # = F50_63,
         # = F63_75,
         # = F63_105,
         # = F210_420,
         # = F420_2000,
         # = TH_1_0,
         # = TH_1_3,
         # = TH_1_5,
         # = TH_1_7,
         # = TH_2_0,
         # = TH_2_4,
         # = TH_2_7,
         # = TH_3_0,
         # = TH_3_4,
         # = TH_3_7,
         # = TH_4_0,
         # = TH_4_2,
         # = K_1_0,
         # = K_1_3,
         # = K_1_5,
         # = K_1_7,
         # = K_2_0,
         # = K_2_4,
         # = K_2_7,
         # = K_3_0,
         # = K_3_4,
         # = K_3_7,
         # = K_4_0,
         # = K_4_2,
         PAL = P_AL)             # p-amm.lactaat-azijnzuur

# nest variable columns for better overview and reorganize order of columns
tbl_PFB_lab <- tbl_PFB_lab %>% 
  # soil target properties: pH, CEC, Ctot, Corg, SOM, N, P, BD, clay, silt, sand
  nest(soil_target = c(pH_H2O, pH, pH_KCl, pH_CaCl2, CEC, C_tot, C_org, C_org2,
                       SOM_CHT, N_tot, N_min, N_min_NH4, N_min_NO3, NO3_con,
                       NH4_con, NH4_ex, P_tot, P_con, P_fix, P_ads, P_oxal, P_H2O,
                       P2O5_ox, P2O5_unox, P_citr, P_water, PAL, OP_con, BD,
                       BD_d_m, clay_m, silt_50m, silt_63m, loam_m, sand_m,
                       grain_size_m),
       # order soil chemical properties in order of plant nutrient importance:
       # primary, seconday, tertiary macronutrients and then micronutrients
       soil_chem = c(EC_sat, EC_H2O, hydr_cond_sat, K_tot, K_con, K_fix, K_ex,
                     K_HCL, K_tot_lit, S_tot, SO4_con, SO4_tot, S_SO4, Ca_tot,
                     Ca_con, Ca_ex, CaCO3, lime_stpc, lime_ke50, Ca_tot_lit,
                     Mg_tot, Mg_con, Mg_ex, Mg_NaCl, Mg_tot_lit, Fe_tot, Fe_con,
                     Fe_ex, Fe_S2O4, Fe_oxal, Fe_HNO3, Fe_HCl, Fe_tot_lit, Mo_tot,
                     B_H2O, Cu_tot, Cu_con, Cu_SZ, Mn_tot, Mn_con, Mn_ex, Mn_oxal,
                     Mn_tot_lit, Na_tot, Na_con, Na_ex, Na_tot_lit, Zn_tot,
                     Ni_tot, Ni_con, Cl_tot, Cl_con, NaCl, Co_tot, Co_AZ, Al_tot,
                     Al_con, Al_ex, Al_HNO3, Al_oxal, Al_HCl, Al_tot_lit, Si_tot,
                     Si_con, V_tot, Se_tot, Cr_tot, Pb_tot, Cd_tot, Ti_tot,
                     Ti_tot_lit, Cr_con, Cd_con, Zn_con, Pb_con, Ag_tot, Be_tot,
                     Ba_tot, As_tot, Hg_tot, F_tot, Br_tot, Te_tot, Sn_tot,
                     Sb_tot, H_ex, Pyr, VOCHT, pw_getal),
       # group soil physical properties
       soil_phys = c(gravel_m, clay_dep_ratio, cap_rise_s, cap_rise_b,
                     moist_sat),
       # soil profile or borehole (visual) characteristics and observations
       # ordered logically (generalized) from O horizon to topsoil to subsoil characteristics
       soil_profile = c(root_d, gw_class, gw_mean_high, gw_mean_low,
                        gw_mean_high_up_bound, gw_mean_low_up_bound, profile_dist),
       # environmental factors at locations from which samples originate
       env_fact = c(elevation, landuse, vegetation, tree_type, drain),
       # metadata of field observations, lab analysis and other
       metadata = c(date, date_bep, date_mon, date_valid, year, month, place,
                    origin, proj_id, proj_c, num, method_lab_CHE, lab_CHE,
                    comment),
       # group remaining (mostly unknown) variables -- check with WENR colleagues
       unknown = c(TKRT_C, PB_VNR, SOORT_PB,
                   TD_TKRT_C, OPSTEL_C, STPC_VOOR, STPC_SUB, STPC_CIJF, STPC_ACHT, KE50_VOOR,
                   KE50_LET, KE50_CIJF, KE50_ACHT, KE50_VERG, KE50_HEL, GT_C, GVG, STAMBUIS,
                   WATERST, VEG_BAN_C, SITUATIES, A, B, VAK_C, VAK_VNR, GVG_DIEPERDAN,
                   MON_VNR, MONSTER_T, A_CYF, ZBV, SOORT_GRA, F2, F2_4, F4_8, F8_16, F2_16,
                   F16_25, F25_35, F35_50, F16_50, F50, F50_75, F75_105, F50_105, F105_150,
                   F150, F150_210, F210, F210_300, F300, F300_420, F420_600, F600_850,
                   F850_1200, F12001700, F50_63, F63_75, F63_105, F210_420, F420_2000,
                   TH_1_0, TH_1_3, TH_1_5, TH_1_7, TH_2_0, TH_2_4, TH_2_7, TH_3_0, TH_3_4,
                   TH_3_7, TH_4_0, TH_4_2, K_1_0, K_1_3, K_1_5, K_1_7, K_2_0, K_2_4, K_2_7,
                   K_3_0, K_3_4, K_3_7, K_4_0, K_4_2, MOP_NR)) %>% 
  #keep_empty = TRUE) %>% # keeps all rows, even if all are NA
  # order columns of nested tbl: ID, coordinates, horizon/depth, target soil properties,
  # other nested cols and names of unknown cols
  select(site_id, X, Y, hor, d_upper, d_lower, soil_target, soil_chem,
         soil_phys, soil_profile, env_fact, metadata, unknown) %>% 
  # arrange by site from topsoil to increasing depth
  arrange(site_id, d_upper & d_lower)

# save tbl to disk
write_rds(tbl_PFB_lab, "out/data/soil/tbl_PFB_lab.Rds")



### PFB field observations soil point data -------------------------------------

# PFB LAAG & HUM ----------------------------------------------------------

# Also collect PFB horizon information from PFB_LAAG
tbl_PFB_LAAG <- tbl(odbc_con, in_schema("BISUSER", "PFB_LAAG")) %>% 
  collect()

# Also collect PFB humus information from PFB_LA_HUM
tbl_PFB_HUM <- tbl(odbc_con, in_schema("BISUSER", "PFB_LA_HUM")) %>% 
  collect()

# combine field observation tables of O-horizon (HUM) and soil horizons (LAAG)
tbl_PFB_field <- full_join(tbl_PFB_LAAG, tbl_PFB_HUM, by = "PFB_ID")

# check which double variables are most complete
tbl_PFB_field %>% 
  select(contains(".")) %>% 
  select(order(colnames(.))) %>% 
  summary()
# most complete: GELDIG_VAN.x, HOR_DIEPB.x, HOR_DIEPO.x, LAAG_NR.x, PB_VNR.x,
# PROJ_C.x, TKRT_C.x

# delete columns of which there is a double without NA's or where they are
# identical (VAK_C and VAK_VNR)
tbl_PFB_field <- tbl_PFB_field %>% 
  select(-c(GELDIG_VAN.y, HOR_DIEPB.y, HOR_DIEPO.y, LAAG_NR.y, VAK_C.y, VAK_VNR.y)) %>%
  rename(date_valid = GELDIG_VAN.x,
         d_upper = HOR_DIEPB.x,
         d_lower = HOR_DIEPO.x,
         hor_nr = LAAG_NR.x,
         VAK_C = VAK_C.x,
         VAK_VNR = VAK_VNR.x)

# for others, combine variables with as little NAs as possible
tbl_PFB_field <- tbl_PFB_field %>% 
  mutate(PB_VNR = if_else(PB_VNR.x %in% NA & !PB_VNR.y %in% NA,
                          PB_VNR.y,
                          PB_VNR.x)) %>%
  mutate(PROJ_C = if_else(PROJ_C.x %in% NA & !PROJ_C.y %in% NA,
                          PROJ_C.y,
                          PROJ_C.x)) %>% 
  mutate(TKRT_C = if_else(TKRT_C.x %in% NA & !TKRT_C.y %in% NA,
                          TKRT_C.y,
                          TKRT_C.x)) %>%
  select(-c(PB_VNR.x, PB_VNR.y, PROJ_C.x, PROJ_C.y, TKRT_C.x, TKRT_C.y))

# combine PFB field observations with coordinates in PFB_ALG
tbl_PFB_field <- full_join(tbl_PFB_ALG, tbl_PFB_field, by = "PFB_ID")

# check which double variables are most complete
tbl_PFB_field %>% 
  select(contains(".")) %>% 
  select(order(colnames(.))) %>% 
  summarise_all(funs(sum(is.na(.))))

# remove irrelevant cols and merge
tbl_PFB_field <- tbl_PFB_field %>% 
  select(-c(PROJ_C.y, TKRT_C.y)) %>% 
  rename(comment_ALG = OPMERKING.x,
         comment_LAAG = OPMERKING.y,
         proj_c = PROJ_C.x,
         TKRT_C = TKRT_C.x) %>% 
  mutate(PB_VNR = if_else(PB_VNR.x %in% NA & !PB_VNR.y %in% NA,
                          PB_VNR.y,
                          PB_VNR.x)) %>%
  mutate(VAK_C = if_else(VAK_C.x %in% NA & !VAK_C.y %in% NA,
                         VAK_C.y,
                         VAK_C.x)) %>%
  mutate(VAK_VNR = if_else(VAK_VNR.x %in% NA & !VAK_VNR.y %in% NA,
                           VAK_VNR.y,
                           VAK_VNR.x)) %>%
  select(-c(PB_VNR.x, PB_VNR.y, VAK_C.x, VAK_C.y, VAK_VNR.x, VAK_VNR.y))

# rename to english, understandable names (see "db/BISAttributen.xlsx")
# TO-DO: automise this using complete BIS attributes names csv file
tbl_PFB_field <- tbl_PFB_field %>% 
  rename(site_id = PFB_ID,
         proj_id = PROJ_ID,
         hor = HOR_CODE,
         hor_hum = H_HOR_CODE,     # humus horizon code
         org_type = AARD_ORG,      # organic matter type (?))
         org_type = AARD_ORG,      # organic matter type (?)
         peat_com = VEEN_C,        # peat composition (type)
         maturation = RIJPING,
         base_sat = KVERZ_S,       # base saturation??
         method_auger = BOOR_METH, # EDM = Edelmanbor, GUT = guts (for peat soils), KUI = kuil (pits), PUL = puls (for loose sediments, etc.)
         par_mat_code = GEO_FOR_C,      # code for parent material
         SOM_LAAG = ORG,           # is this really soil OM? or e.g. OM of also litter?
         # organic matter content (%) from PFB_LAAG
         clay_s = LUTUM_S,
         silt_s = SILT_S,
         loam_s = LEEM_S,
         grain_size_s = M50_S,
         gravel_s = GRIND_S,
         lime = KALK,
         BD_d_s = RHO_D_S,
         hue = HUE,
         pores = PORIEN,
         root_num = WORT_AANT,      # ???
         shells_s = SCHELP_S,
         obj_id_laag = OBJECTID,
         root_num2 = WORTELS_AANT,  # ???
         root_large = WORTELS_GROOT,
         date = DATUM,
         place = PLAATS,
         month = MAAND,
         year = JAAR,
         gw_mean_high = GHG,
         gw_mean_low = GLG,
         lime_stpc = STPC_KALK,
         lime_ke50 = KE50_KALK,
         # = STPC_ACHT,
         profile_dist = STPC_VERG,
         gw_class = STPC_GT,
         root_d = BEW,
         origin = HERKOMST,
         gw_mean_high_up_bound = GHG_DIEPERDAN,   # upper boundary of mean highest groundwater level?
         gw_mean_low_up_bound = GLG_DIEPERDAN,   # upper boundary of mean lowest groundwater level?
         drain = DRAINAGE,
         tree_type = BOOMSOORT,
         vegetation = VEGETATIE,
         wat_tab_lev = WATERST,   # ??
         landuse = BODEM_C,
         elevation = MVHOOGTE,
         # = VEG_BAN_C,
         # = SITUATIES,
         # = OPSTEL_C,
         # = GVG_DIEPERDAN
         # = DUID_GRNS,
         # = VALUE,              # ????
         # = CHROMA,
         # = TST_MOER,
         # = GRND_KNIP,
         # = VERKIT,
         # = ROEST,
         # = VLEK_SRT,
         # = VLEK_AANT,
         # = VOCHTIGH,
         # = S_TYPE,
         # = S_GRO_VER,
         # = S_GRA_PAK,
         # = SLIJPPL,
         # = C,
         # = D,
         # = VORM_GRNS,
         # = WORT_VERD,
         # = HOOFDGRS_C,
         # = MENGVERH,
         # = OPM2,
         # = GRENS_VORM,
         # = GRENS_AFM,
         # = STRUCTUURTYPE,
         # = WORTELS_RICHT,
         fauna = FAUNA)

# nest variable columns for better overview and reorganize order of columns
tbl_PFB_field <- tbl_PFB_field %>% 
  # soil target properties: pH, CEC, Ctot, Corg, SOM, N, P, BD, clay, silt, sand
  nest(soil_target = c(SOM_LAAG, clay_s, silt_s, loam_s, grain_size_s, BD_d_s),
       # other soil properties that we will not map
       soil_other = c(base_sat, org_type, peat_com, gravel_s, lime, lime_stpc, lime_ke50,
                      maturation, pores, shells_s),
       # soil profile or borehole (visual) characteristics and observations
       # ordered logically (generalized) from O horizon to topsoil to subsoil characteristics
       soil_profile = c(hor_hum, root_d, root_num, root_num2, root_large, gw_class,
                        gw_mean_high, gw_mean_low, gw_mean_high_up_bound,
                        gw_mean_low_up_bound, wat_tab_lev, profile_dist, drain,
                        hue, fauna),
       # environmental factors at locations from which samples originate
       env_fact = c(par_mat_code, landuse, vegetation, tree_type, drain),
       # metadata of field observations, lab analysis and other
       metadata = c(date, date_valid, year, month, place,
                    origin, obj_id_laag, proj_id, proj_c, method_auger,
                    comment_ALG, comment_LAAG),
       # group remaining (mostly unknown) variables -- check with WENR colleagues
       unknown = c(TKRT_C, PB_VNR, SOORT_PB, TD_TKRT_C, OPSTEL_C, STPC_VOOR, STPC_SUB,
                   STPC_CIJF, STPC_ACHT, KE50_VOOR, KE50_LET, KE50_CIJF, KE50_ACHT,
                   KE50_VERG, KE50_HEL, GT_C, GVG, STAMBUIS, KRIT_Z_S, VEG_BAN_C,
                   SITUATIES, A, B, VAK_C, VAK_VNR, GVG_DIEPERDAN, MOP_NR, DUID_GRNS,
                   VALUE, CHROMA, TST_MOER, GRND_KNIP, VERKIT, ROEST, VLEK_SRT,
                   VLEK_AANT, VOCHTIGH, S_TYPE, S_GRO_VER, S_GRA_PAK, SLIJPPL,
                   C, D, VORM_GRNS, WORT_VERD, HOOFDGRS_C, MENGVERH, OPM2, GRENS_VORM,
                   GRENS_AFM, STRUCTUURTYPE, WORTELS_RICHT)) %>% 
  #keep_empty = TRUE) %>% # keeps all rows, even if all are NA
  # order columns of nested tbl: ID, coordinates, horizon/depth, target soil properties,
  # other nested cols and names of unknown cols
  select(site_id, X, Y, hor_nr, hor, d_upper, d_lower, soil_target,
         soil_other, soil_profile, env_fact, metadata, unknown) %>% 
  # arrange by site from topsoil to increasing depth
  arrange(site_id, hor_nr)

# save tbl to disk
write_rds(tbl_PFB_field, "out/data/soil/tbl_PFB_field.Rds")



### LSK soil point data -----------------------------------------------------

# LSK lab data: Combining LSK_ANALYSE with LSK_MONSTER and LSK_ALG -------------

# read LSK tables into working memory
tbl_LSK_ANALYSE <- tbl(odbc_con, in_schema("BISUSER", "LSK_ANALYSE")) %>% 
  collect()

# this is not tidy data: variables of lab measurements (VAR_NAAM) in rows;
# values = WAARDE; VAR_TEXT = very low values close to 0

# first turn VAR_TEXT into numerical variable
tbl_LSK_ANALYSE <- tbl_LSK_ANALYSE %>% 
  mutate(VAR_WAARDE_SMALL = str_replace(string = VAR_TEXT,
                                        pattern = "<",
                                        replacement = ""))

tbl_LSK_ANALYSE$VAR_WAARDE_SMALL <- as.double(tbl_LSK_ANALYSE$VAR_WAARDE_SMALL)

# add low values to VALUE column if this replaces an NA
tbl_LSK_ANALYSE <- tbl_LSK_ANALYSE %>% 
  mutate(VAR_VALUE = if_else(VAR_WAARDE %in% NA & !VAR_WAARDE_SMALL %in% NA,
                             VAR_WAARDE_SMALL,
                             VAR_WAARDE)) %>% 
  select(-c(VAR_WAARDE, VAR_TEXT, VAR_WAARDE_SMALL))

# tidying: convert row grouped by soil property into column
tbl_LSK_ANALYSE <- tbl_LSK_ANALYSE %>%
  pivot_wider(names_from = VAR_NAAM,
              values_from = c(VAR_VALUE, METHODE_C, LAB_C))

# change names of variables
tbl_LSK_ANALYSE <- tbl_LSK_ANALYSE %>% 
  rename_all(gsub, pattern = "-", replacement = "_")

# rename value variables and nest method and lab variables
# MON_VNR is link variable to other tbls, so don't change
tbl_LSK_ANALYSE <- tbl_LSK_ANALYSE %>% 
  rename_all(gsub, pattern = "VAR_VALUE_", replacement = "") %>% 
  rename(date_valid = GELDIG_VAN) %>% 
  nest(method_lab = c(contains("METHODE_C")),
       lab = c(contains("LAB_C")))

# LSK_MONSTER
tbl_LSK_MONST <- tbl(odbc_con, in_schema("BISUSER", "LSK_MONSTER")) %>% 
  collect()

# combine with LSK_ANALYSE by linking MON_VNR
tbl_LSK_lab <- full_join(tbl_LSK_MONST, tbl_LSK_ANALYSE, by = "MON_VNR")


# LSK_ALG & combine, group, rename cols & save
# As with BPK_ALG and PFB_ALG, work around by querying tbl & selecting only necessary columns
# get column names from here
tbl(odbc_con, in_schema("BISUSER", "LSK_ALG_VERSIE"))

tbl_LSK_ALG <- dbGetQuery(odbc_con, 'SELECT LSK_ID, PROJ_ID, X, Y, ST_NR, STRAT_NR,
                          BS_NR, VOLGNR, BKB_KODE, STPC_VOOR, STPC_SUB, STPC_CIJF,
                          STPC_KALK, STPC_ACHT, STPC_VERG, STPC_GT, GHG, GLG, O_STPC_GT,
                          O_GHG, O_GLG, WATERSTAND, BODEM_C, BEW, HUMUSVORM, KRIT_Z_S,
                          VEG_BAN_C, SITUATIES, OPSTEL_C, DATUM, HOOGTE, OPMERKINGEN,
                          BWST, BWST_120, BWST_GHG, MVHOOGTE
                          FROM BISUSER.LSK_ALG') %>%
  as_tibble()
# check using SQL software at WUR if this is indeed all variables (we need)

# link LSK lab analysis tbl (LSK_ANALYSE) to coordinates in LSK_ALG through MON_VNR & LSK_ID
tbl_LSK_lab <- full_join(tbl_LSK_lab, tbl_LSK_ALG, by = "LSK_ID") %>% 
  select(-GELDIG_VAN) # exactly the same as date_valid

# rename to english, understandable names (see "db/BISAttributen.xlsx")
tbl_LSK_lab <- tbl_LSK_lab %>% 
  rename(site_id = LSK_ID,
         #= MON_VNR,
         d_upper = BEM_DIEPB,
         d_lower = BEM_DIEPO,
         #= MOP_NR,
         #= MONSTER_T,
         date_bep = BEP_DATUM,
         Al_ex = AL_ONG,            # Al-uit ongeb (uitwisselbaar Al = exchangeable Al)?????
         Ca_ex = CA_ONG,            # ?
         Fe_ex = FE_ONG,            # ?
         Mg_ex = MG_ONG,            # ?
         Mn_ex = MN_ONG,            # ?
         K_ex = K_ONG,              # ?
         Na_ex = NA_ONG,            # ?
         CEC_eff = SOM_KAT,         # effective cation exchange capacity
         pH_KCl = PH_KCL,
         #= VOCHT,
         hum = HUMUS,
         CaCO3 = CACO3,
         P_oxal = P_OX,
         Al_oxal = AL_OX,
         Fe_oxal = FE_OX,
         pw_getal = PW_GET,         # ???
         PAL = P_AL,                # p-amm.lactaat-azijnzuur (???)
         N_tot = N_TOT,
         C_tot = C_EL,         # totaal koolstofgehalte (c-elementair) = total C content
         # however, Fokke said C_EL = C bound to minerals and not equal to Ctot???
         P2O5_ox = P2O5_MET,   # Phosphate content after oxidation
         P2O5_unox = P2O5_ZON, # Phosphate content before oxidation
         Ag_ex = AG_ONG,       # ?
         N_min = N_MIN,
         As_tot = AS_TOT,
         Cd_tot = CD_TOT,
         Pb_tot = PB_TOT,
         Zn_tot = ZN_TOT,
         Cr_tot = CR_TOT,
         Cu_tot = CU_TOT,
         Ni_tot = NI_TOT,
         BD = BULKD,
         Ni_pot = NI_POT,
         Pb_pot = PB_POT,
         Zn_pot = ZN_POT,
         As_pot = AS_POT,
         Cd_pot = CD_POT,
         Cr_pot = CR_POT,
         Cu_pot = CU_POT,
         proj_id = PROJ_ID,
         #= ST_NR,
         # = STRAT_NR,
         # = BS_NR,
         # = VOLGNR,
         # = BKB_KODE,
         # = STPC_VOOR,
         # = STPC_SUB,
         # = STPC_CIJF,
         lime_stpc = STPC_KALK,
         # = STPC_ACHT,
         profile_dist = STPC_VERG,
         gw_class = STPC_GT,
         gw_mean_high = GHG,
         gw_mean_low = GLG,
         gw_class_o = O_STPC_GT,
         gw_mean_high_o = O_GHG,
         gw_mean_low_o = O_GLG,
         wat_tab_lev = WATERSTAND,
         landuse = BODEM_C,
         root_d = BEW,
         hum_type = HUMUSVORM,
         cap_rise_s = KRIT_Z_S,
         # = VEG_BAN_C,
         # = SITUATIES,
         # = OPSTEL_C,
         date = DATUM,
         elevation = HOOGTE,
         comment_ALG = OPMERKINGEN,
         # = BWST,
         # = BWST_120,
         # = BWST_GHG,
         elevation2 = MVHOOGTE)

# nest variable columns for better overview and reorganize order of columns
tbl_LSK_lab <- tbl_LSK_lab %>% 
  # soil target properties: pH, CEC, Ctot, Corg, SOM, N, P, BD, clay, silt, sand
  nest(soil_target = c(pH_KCl, CEC_eff, C_tot, N_tot, N_min, P_oxal, P2O5_ox,
                       P2O5_unox, PAL, BD),
       # order soil chemical properties in order of plant nutrient importance:
       # primary, seconday, tertiary macronutrients and then micronutrients
       soil_chem = c(K_ex, Ca_ex, CaCO3, lime_stpc, Mg_ex, Fe_ex,
                     Fe_oxal, Cu_tot, Cu_pot, Mn_ex, Na_ex, Zn_tot, Zn_pot, Ni_tot,
                     Ni_pot, Al_ex, Al_oxal, Cr_tot, Cr_pot, Pb_tot, Pb_pot, Cd_tot,
                     Cd_pot, Ag_ex, As_tot, As_pot, VOCHT, pw_getal),
       # soil profile or borehole (visual) characteristics and observations
       # ordered logically (generalized) from O horizon to topsoil to subsoil characteristics
       soil_profile = c(hum, hum_type, root_d, gw_class, gw_class_o, gw_mean_high,
                        gw_mean_high_o, gw_mean_low, gw_mean_low_o, wat_tab_lev,
                        profile_dist, cap_rise_s),
       # environmental factors at locations from which samples originate
       env_fact = c(elevation, elevation2, landuse),
       # metadata of field observations, lab analysis and other
       metadata = c(date, date_bep, date_valid, proj_id,
                    method_lab, lab, comment_ALG),
       # group remaining (mostly unknown) variables -- check with WENR colleagues
       unknown = c(OPSTEL_C, STPC_VOOR, STPC_SUB, STPC_CIJF, STPC_ACHT, VEG_BAN_C,
                   SITUATIES, MONSTER_T, MON_VNR, MOP_NR, ST_NR, STRAT_NR, BS_NR,
                   VOLGNR, BKB_KODE, BWST, BWST_120, BWST_GHG)) %>% 
  #keep_empty = TRUE) %>% # keeps all rows, even if all are NA
  # order columns of nested tbl: ID, coordinates, horizon/depth, target soil properties,
  # other nested cols and names of unknown cols
  select(site_id, X, Y, d_upper, d_lower, soil_target, soil_chem, soil_profile,
         env_fact, metadata, unknown) %>% 
  # arrange by site from topsoil to increasing depth
  arrange(site_id, d_upper & d_lower)

# save tbl to disk
write_rds(tbl_LSK_lab, "out/data/soil/tbl_LSK_lab.Rds")


# LSK field data: Combining LSK_LAAG with LSK_ALG ------------------------------

tbl_LSK_LAAG <- tbl(odbc_con, in_schema("BISUSER", "LSK_LAAG")) %>% 
  collect()

# combine with LSK_ALG: link field observations to coordinates in LSK_ALG through LSK_ID
tbl_LSK_field <- full_join(tbl_LSK_ALG, tbl_LSK_LAAG, by = "LSK_ID")

# rename to english, understandable names (see "db/BISAttributen.xlsx")
tbl_LSK_field <- tbl_LSK_field %>% 
  rename(site_id = LSK_ID,
         proj_id = PROJ_ID,
         # = ST_NR,
         # = STRAT_NR,
         # = BS_NR,
         # = VOLGNR,
         # = BKB_KODE,
         # = STPC_VOOR,
         # = STPC_SUB,
         # = STPC_CIJF,
         lime_stpc = STPC_KALK,
         # = STPC_ACHT,
         profile_dist = STPC_VERG,
         gw_class = STPC_GT,
         gw_mean_high = GHG,
         gw_mean_low = GLG,
         gw_class_o = O_STPC_GT,
         gw_mean_high_o = O_GHG,
         gw_mean_low_o = O_GLG,
         wat_tab_lev = WATERSTAND,
         landuse = BODEM_C,
         root_d = BEW,
         hum_type = HUMUSVORM,
         cap_rise_s = KRIT_Z_S,
         # = VEG_BAN_C,
         # = SITUATIES,
         # = OPSTEL_C,
         date = DATUM,
         elevation = HOOGTE,
         comment_ALG = OPMERKINGEN,
         # = BWST,
         # = BWST_120,
         # = BWST_GHG,
         elevation2 = MVHOOGTE,
         hor_nr = LAAG_NR,
         d_upper = HOR_DIEPB,
         d_lower = HOR_DIEPO,
         hor = HOR_KODE,
         #= BOUWST_KODE,        # ???
         # = ALG_INFO,
         SOM_LAAG = ORG,           # is this really soil OM? or e.g. OM of also litter?
         # organic matter content (%) from PFB_LAAG
         SOM_O = O_ORG,         # ???
         org_type = AARD_ORG,      # organic matter type (?)
         clay_s = LUTUM_S,
         loam_s = LEEM_S,
         grain_size_s = M50_S,       # ??
         clay_o_s = O_LUTUM_S,       # ??
         loam_o_s = O_LEEM_S,        # ??
         grain_size_o_s = O_M50_S,   # ??
         lime = KALK,
         par_mat_code = GEO_FOR_C,   # code for parent material
         comment_LAAG = OPMERKING,
         method_auger = BOOR_METH,
         maturation = RIJPING,
         hum_type2 = HUMUSTYPE,
         #= S_TYPE,
         # = S_GRO_VER,
         # = S_GRA_PAK,
         date_valid = GELDIG_VAN)

# nest variable columns for better overview and reorganize order of columns
tbl_LSK_field <- tbl_LSK_field %>% 
  # soil target properties: pH, CEC, Ctot, Corg, SOM, N, P, BD, clay, silt, sand
  nest(soil_target = c(SOM_LAAG, SOM_O, clay_s, clay_o_s, loam_s, loam_o_s,
                       grain_size_s, grain_size_o_s),
       # order soil chemical properties in order of plant nutrient importance:
       # primary, seconday, tertiary macronutrients and then micronutrients
       soil_other = c(org_type, lime, lime_stpc,
                      maturation),
       # soil profile or borehole (visual) characteristics and observations
       # ordered logically (generalized) from O horizon to topsoil to subsoil characteristics
       soil_profile = c(hum_type, hum_type2, root_d, gw_class, gw_class_o,
                        gw_mean_high, gw_mean_high_o, gw_mean_low, gw_mean_low_o,
                        wat_tab_lev, profile_dist, cap_rise_s),
       # environmental factors at locations from which samples originate
       env_fact = c(elevation, elevation2, par_mat_code, landuse),
       # metadata of field observations, lab analysis and other
       metadata = c(date, date_valid, proj_id, comment_LAAG, comment_ALG,
                    method_auger),
       # group remaining (mostly unknown) variables -- check with WENR colleagues
       unknown = c(OPSTEL_C, STPC_VOOR, STPC_SUB, STPC_CIJF, STPC_ACHT,
                   VEG_BAN_C, SITUATIES, ST_NR, STRAT_NR, BS_NR,
                   VOLGNR, BKB_KODE, BWST, BWST_120, BWST_GHG)) %>%
  #keep_empty = TRUE) %>% # keeps all rows, even if all are NA
  # order columns of nested tbl: ID, coordinates, horizon/depth, target soil properties,
  # other nested cols and names of unknown cols
  select(site_id, X, Y, hor_nr, hor, d_upper, d_lower, soil_target, soil_other,
         soil_profile, env_fact, metadata, unknown) %>% 
  # arrange by site from topsoil to increasing depth
  arrange(site_id, d_upper & d_lower)

# save tbl to disk
write_rds(tbl_LSK_field, "out/data/soil/tbl_LSK_field.Rds")



### Close DB connection -----------------------------------------------------

dbDisconnect(odbc_con)


