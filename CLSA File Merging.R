######################################################################
###### Predicting Frailty in Older Adults with Machine Learning ######
########### Canadian Longitudinal Study of Aging #####################
######################################################################


#### 1.0 Load Packages #####
library(tidyverse) # general

#### 2.0 Baseline Dataset #####
CLSA.Com.baseline <-read_csv("2304007_UOttawa_MBoisgontier_Baseline_CoPv7.CSV")
CLSA.Tra.baseline <-read_csv("2304007_UOttawa_MBoisgontier_Baseline_Trav4.CSV")
CLSA.baseline <- merge(CLSA.Com.baseline, CLSA.Tra.baseline, all=TRUE) 

#### 3.0 Coalesce _COM and _TRM variables ####

# this code creates a dataset ("CLSA.baseline.v1; n = 51,338, vars = 1667) that
# retains all items presented to all participants (e.g., _MCQ) and coalesces
# items that were delivered at part of tracking and comprehensive assessments. 
# for example, the variables "SEX_ASK_COM" and "SEX_ASK_TRM" are coalesced into
# a single variable called "SEX_ASK". "SEX_ASK_COM" and "SEX_ASK_TRM" are then
# removed from the dataset. 

# Identify columns that end with "_COM" or "_TRM"
com_cols <- grep("_COM$", names(CLSA.baseline), ignore.case = TRUE, value = TRUE)
trm_cols <- grep("_TRM$", names(CLSA.baseline), ignore.case = TRUE, value = TRUE)

# Extract base names
com_bases <- sub("_COM$", "", com_cols, ignore.case = TRUE)
trm_bases <- sub("_TRM$", "", trm_cols, ignore.case = TRUE)

# Identify common base names
common_bases <- intersect(com_bases, trm_bases)

# Initialize a list to store data frames of coalesced columns
coalesced_dfs <- list()

# Coalesce the values for the common base names
for (base_name in common_bases) {
  com_col <- paste0(base_name, "_COM")
  trm_col <- paste0(base_name, "_TRM")
  
  # Ensure columns exist in the current dataset
  if (com_col %in% names(CLSA.baseline) && trm_col %in% names(CLSA.baseline)) {
    # Convert both columns to character type
    CLSA.baseline[[com_col]] <- as.character(CLSA.baseline[[com_col]])
    CLSA.baseline[[trm_col]] <- as.character(CLSA.baseline[[trm_col]])
    
    # Coalesce values into a new data frame and add to the list
    coalesced_dfs[[base_name]] <- CLSA.baseline %>%
      transmute(!!base_name := coalesce(!!sym(com_col), !!sym(trm_col)))
  }
}

# Combine the coalesced columns with the original data frame
CLSA.baseline.v1 <- bind_cols(CLSA.baseline, coalesced_dfs)

# Create a vector of column names that should be removed (those that have been coalesced)
cols_to_remove <- unlist(lapply(common_bases, function(base) {
  c(paste0(base, "_COM"), paste0(base, "_TRM"))
}))

# Check if the columns to remove actually exist in the data frame
existing_cols_to_remove <- cols_to_remove[cols_to_remove %in% names(CLSA.baseline.v1)]

# Remove only the coalesced "_COM" and "_TRM" columns that exist
CLSA.baseline.v1.1 <- CLSA.baseline.v1 %>%
  select(-all_of(existing_cols_to_remove))

# Export Data sets

# View and save the modified dataset (n=5318)
write.csv(CLSA.baseline, file = "CLSA.baseline.1.rawmerge.csv")
# View and save the modified dataset (n=6378)
write.csv(CLSA.baseline.v1, file = "CLSA.baseline.2.rawmerge.coalesce.csv")
# View and save the modified dataset (n=4248)
write.csv(CLSA.baseline.v1.1, file = "CLSA.baseline.3.coalesce.removed.csv")


#### 4.0 Remove redundant variables (individual items when summary item available) ####



# 4.1 Alcohol Use (ALC) n = 4258 -> n = 4243 ####
#Keep "ALC_TTM" and remove "ALC_EVER_TRM, ALC_FREQ_TRM"
alc_vars_to_remove <- names(CLSA.baseline.v1.1)[startsWith(names(CLSA.baseline.v1.1), "ALC_") & names(CLSA.baseline.v1.1) != "ALC_TTM"]
# Report the variables that will be removed
cat("Variables to be removed:", alc_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.1 %>%
  select(-all_of(alc_vars_to_remove))

# 4.2 Basic (ADL) & Instrumental Activities of Daily Living (IAL) n = 4220 ####
# Keep  (ADL_DCLS, ADL_DSUM), remove all else that begin with "ADL_"
adl_vars_to_remove <- names(CLSA.baseline.v1.1)[startsWith(names(CLSA.baseline.v1.1), "ADL_") & !names(CLSA.baseline.v1.1) %in% c("ADL_DCLS", "ADL_DSUM")]
# Report the variables that will be removed
cat("Variables to be removed:", adl_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(adl_vars_to_remove))

# 4.3 Care Giving (CAG) n = 4194 ####
#keep "CAG_FPAS" and remove all else beginning with "CAG_"
cag_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "CAG_") & names(CLSA.baseline.v1.2) != "CAG_FPAS"]
# Report the variables that will be removed
cat("Variables to be removed:", cag_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(cag_vars_to_remove))

# 4.4 Care Receiving 1/Formal Care (CR1) & Care Receiving 2/Informal Care (CR2) n = 4117 ####
# keep "CR1_FRHC, CR2_FRHC" and remove all else beginning with "CR2_ or CR1_"
cr1_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "CR1_") & names(CLSA.baseline.v1.2) != "CR1_FRHC"]
# Report the variables that will be removed
cat("Variables to be removed:", cr1_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(cr1_vars_to_remove))
# Identify all variables that start with "CR2_" except for "CR2_FRHC"
cr2_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "CR2_") & names(CLSA.baseline.v1.2) != "CR2_FRHC"]
# Report the variables that will be removed
cat("Variables to be removed:", cr2_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(cr2_vars_to_remove))


# 4.5 Chronic Conditions (CCT/CCC) n = 3971 ####

exceptions <- c("CCC_F2_COM", "CCC_F1_COM", "CCC_HEART_COM", "CCC_PVD_COM", 
                "CCC_MEMPB_COM", "CCC_ALZH_COM", "CCC_MS_COM", "CCC_EPIL_COM", 
                "CCC_MGRN_COM", "CCC_ULCR_COM", "CCC_IBDIBS_COM", "CCC_BOWINC_COM", 
                "CCC_URIINC_COM", "CCC_MACDEG_COM", "CCC_CANC_COM", "CCC_ANXI_COM", 
                "CCC_MOOD_COM", "CCC_ALLRG_COM", "CCC_BCKP_COM", "CCC_KIDN_COM",
                "CCC_DRPNEU_COM", "CCC_DRFLU_COM", "CCC_DRUTI_COM", "CCC_OAHAND_COM", 
                "CCC_OAHIP_COM", "CCC_OAKNEE_COM", "CCC_RA_COM", "CCC_CANTP_SM_COM", 
                "CCC_HBP_COM", "CCC_UTHYR_COM", "CCC_ANGI_COM", "CCC_CVA_COM", 
                "CCC_AMI_COM", "CCC_TIA_COM", "CCC_CVAFX_COM", "CCC_ASTHM_COM", 
                "CCC_OSTPO_COM", "CCC_PARK_COM", "CCC_COPD_COM", "CCC_OAHAND_DCS",
                "CCC_OAHIP_DCS", "CCC_OAKNEE_DCS", "CCC_RA_DCS", "CCC_ARTOT_DCS", 
                "CCC_OSTPO_DCS", "CCC_PARK_DCS")

# Identify all variables that start with "CCC_" except for the exceptions
ccc_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "CCC_") & !names(CLSA.baseline.v1.2) %in% exceptions]

# Report the variables that will be removed
cat("Variables to be removed:", ccc_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ccc_vars_to_remove))


# and remove all that start with "OSA_, OSH, OSK_, OAR_, OST_, DPR_, PKD_",

# Define the prefixes to remove
prefixes_to_remove <- c("OSA_", "OSH_", "OSK_", "OAR_", "OST_", "DPR_", "PKD_")

# Identify variables to remove based on the prefixes
vars_to_remove <- names(CLSA.baseline.v1.2)[Reduce(`|`, lapply(prefixes_to_remove, function(prefix) {
  startsWith(names(CLSA.baseline.v1.2), prefix)
}))]

# Report the variables that will be removed
cat("Variables to be removed:", vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(vars_to_remove))


# 4.6 Depression (DEP) n = 3959 ####
#keep "DEP_CESD10" remove all other "DEP_" 
dep_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "DEP_") & names(CLSA.baseline.v1.2) != "DEP_CESD10"]

# Report the variables that will be removed
cat("Variables to be removed:", dep_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(dep_vars_to_remove))

# 4.7 Education (ED) n = 3954 ####
# keep "ED_UDR11, ED_UDR04" remove all other "ED_"
ed_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "ED_") & !names(CLSA.baseline.v1.2) %in% c("ED_UDR11", "ED_UDR04")]
# Report the variables that will be removed
cat("Variables to be removed:", ed_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ed_vars_to_remove))

# 4.8 General Health (GEN) n = 3948 #### 
# keep "GEN_DHDI,GEN_DMHI" remove all other "GEN_" 
gen_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "GEN_") & !names(CLSA.baseline.v1.2) %in% c("GEN_DHDI", "GEN_DMHI")]
# Report the variables that will be removed
cat("Variables to be removed:", gen_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(gen_vars_to_remove))

# 4.9 Life Space Index (LSI) n = 3932 #### 
# keep "LSI_DSCR" and remove all other "LSI_"
lsi_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "LSI_") & names(CLSA.baseline.v1.2) != "LSI_DSCR"]
# Report the variables that will be removed
cat("Variables to be removed:", lsi_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(lsi_vars_to_remove))

# 4.10 Nutritional Risk (NUR) n = 3913 #### 
# keep "NUR_DHNR_MCQ" and remove all other "NUR_" 
nur_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "NUR_") & names(CLSA.baseline.v1.2) != "NUR_DHNR_MCQ"]
# Report the variables that will be removed
cat("Variables to be removed:", nur_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(nur_vars_to_remove))

# 4.11 Personality Traits (PER) n = 3873 #### 
# keep "PER_DSCR_EXT_MCQ, PER_DSCR_AGR_MCQ, PER_DSCR_CON_MCQ,
# PER_DSCR_EMOS_MCQ, PER_DSCR_OPEX_MCQ" and remove all other "PER_"
exceptions <- c("PER_DSCR_EXT_MCQ", "PER_DSCR_AGR_MCQ", "PER_DSCR_CON_MCQ",
                "PER_DSCR_EMOS_MCQ", "PER_DSCR_OPEX_MCQ")
# Identify all variables that start with "PER_" except for the specified exceptions
per_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "PER_") & !names(CLSA.baseline.v1.2) %in% exceptions]
# Report the variables that will be removed
cat("Variables to be removed:", per_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(per_vars_to_remove))

# 4.12 Post-traumatic Stress Disorder (PSD) n = 3867 #### 
# keep "PSD_DCTOFF" and remove all other "PSD_"
psd_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "PSD_") & names(CLSA.baseline.v1.2) != "PSD_DCTOFF"]
# Report the variables that will be removed
cat("Variables to be removed:", psd_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(psd_vars_to_remove))

# 4.13 Psychological Distress (K10) n = 3848 #### 
# keep "K10_DSCORE_MCQ" and remove all other "K10_"
k10_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "K10_") & names(CLSA.baseline.v1.2) != "K10_DSCORE_MCQ"]
# Report the variables that will be removed
cat("Variables to be removed:", k10_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(k10_vars_to_remove))

# 4.14 Retirement Planning (RPL) n = 3791 #### 
# keep "RPL_FPRE" and remove all "RPL_"
rpl_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "RPL_") & names(CLSA.baseline.v1.2) != "RPL_FPRE"]
# Report the variables that will be removed
cat("Variables to be removed:", rpl_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(rpl_vars_to_remove))

# 4.15 Satisfaction with Life (SLS) n = 3770 #### 
# keep "SLS_DSCR" and remove all other "SLS_"
sls_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "SLS_") & names(CLSA.baseline.v1.2) != "SLS_DSCR"]
# Report the variables that will be removed
cat("Variables to be removed:", sls_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(sls_vars_to_remove))

# 4.16 Social Support Availability (SSA) n = 3746#### 
# keep "SSA_DPALL" and remove all other  "SSA_"
ssa_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "SSA_") & names(CLSA.baseline.v1.2) != "SSA_DPALL"]
# Report the variables that will be removed
cat("Variables to be removed:", ssa_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ssa_vars_to_remove))

# 4.17 Social Participation (SPA) n = 3705 #### 
# keep "SPA_FPAR, SPA_DFRE" and remove all other "SPA_"
spa_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "SPA_") & !names(CLSA.baseline.v1.2) %in% c("SPA_DFRE", "SPA_FPAR")]
# Report the variables that will be removed
cat("Variables to be removed:", spa_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(spa_vars_to_remove))


# 4.18 Medicine (MEDI) n =  3025 #### 
# Identify all variables that start with "MEDI_" except for "MEDI_NO_COM"
medi_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "MEDI_") & names(CLSA.baseline.v1.2) != "MEDI_NO_COM"]

# Report the variables that will be removed
cat("Variables to be removed:", medi_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(medi_vars_to_remove))

# 4.19 Cognitive measures (COG) n = 2870 ####
# keep "COG_REYI_SCORE, COG_AFT_SCORE_1, COG_AFT_SCORE_2, COG_MAT_SCORE,
# COG_REYII_SCORE" and remove all other "COG_"
exceptions <- c("COG_REYI_SCORE", "COG_AFT_SCORE_1", "COG_AFT_SCORE_2", "COG_MAT_SCORE", "COG_REYII_SCORE")
# Identify all variables that start with "COG_" except for the specified exceptions
cog_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "COG_") & !names(CLSA.baseline.v1.2) %in% exceptions]
# Report the variables that will be removed
cat("Variables to be removed:", cog_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(cog_vars_to_remove))

# 4.20 Other cognitive measures n = 2711 ####

# PMT_ACR_COM; remove all over PMT_ n = 2844
pmt_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "PMT_") & names(CLSA.baseline.v1.2) != "PMT_ACR_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", pmt_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(pmt_vars_to_remove))

# STP_COLTIME_SS_COM, STP_DOTTIME_SS_COM, STP_WORTIME_SS_COM; remove all over STP_ n = 2784
exceptions <- c("STP_COLTIME_SS_COM", "STP_DOTTIME_SS_COM", "STP_WORTIME_SS_COM")
stp_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "STP_") & !names(CLSA.baseline.v1.2) %in% exceptions]
# Report the variables that will be removed
cat("Variables to be removed:", stp_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(stp_vars_to_remove))

# FAS_A_SCORE_COM, FAS_TOTAL_SCORE_COM, FAS_F_SCORE_COM, FAS_S_SCORE_COM; remove all over FAS_ 
# n=2747 
#Specify the variables to keep
fas_vars_to_keep <- c("FAS_A_SCORE_COM", "FAS_TOTAL_SCORE_COM", "FAS_F_SCORE_COM", "FAS_S_SCORE_COM")
# Identify all variables that start with "FAS_" except for the ones to keep
fas_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "FAS_") & !names(CLSA.baseline.v1.2) %in% fas_vars_to_keep]
# Report the variables that will be removed
cat("Variables to be removed:", fas_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(fas_vars_to_remove))

# CRT_MRT_CORRANS_COM; remove all over CRT_ n = 2739
# Identify all variables that start with "CRT_" except for "CRT_MRT_CORRANS_COM"
crt_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "CRT_") & names(CLSA.baseline.v1.2) != "CRT_MRT_CORRANS_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", crt_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(crt_vars_to_remove))

# TMT_ACC_COM; remove all over TMT_ n =2711
tmt_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "TMT_") & names(CLSA.baseline.v1.2) != "TMT_ACC_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", tmt_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(tmt_vars_to_remove))



# 4.21 Oral Health (ORH) n = 2683 ####
vars_to_remove <- c(
  "ORH_EXP_TTH_MCQ", "ORH_EXP_CHW_MCQ", "ORH_EXP_DNU_MCQ",
  "ORH_EXP_DNL_MCQ", "ORH_EXP_DNB_MCQ", "ORH_EXP_DNM_MCQ", 
  "ORH_EXP_SWL_MCQ", "ORH_EXP_DRM_MCQ", "ORH_EXP_BRM_MCQ", 
  "ORH_EXP_JWS_MCQ", "ORH_EXP_JJP_MCQ", "ORH_EXP_NTD_MCQ", 
  "ORH_EXP_NTL_MCQ", "ORH_EXP_NTB_MCQ", "ORH_EXP_GUMS_MCQ", 
  "ORH_EXP_GUMB_MCQ", "ORH_EXP_DNS_MCQ", "ORH_EXP_TTHD_MCQ",
  "ORH_EXP_BB_MCQ", "ORH_EXP_NONE_MCQ", "ORH_EXP_OT_MCQ", 
  "ORH_EXP_DK_NA_MCQ", "ORH_EXP_REFUSED_MCQ", "ORH_EXP_OTSP01_MCQ", 
  "ORH_EXP_OTSP02_MCQ", "ORH_EXP_OTSP03_MCQ", "ORH_EXP_OTSP04_MCQ", 
  "ORH_EXP_OTSP05_MCQ"
)

# Remove the listed variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(vars_to_remove))

# 4.22 Miscellaneous supplements (DSU) n = 2672 #### 
vars_to_remove <- c("DSU_OT_MCQ", "DSU_OTSP01_MCQ", "DSU_OTSP02_MCQ",
  "DSU_OTSP03_MCQ", "DSU_OTSP04_MCQ", "DSU_OTSP05_MCQ", "DSU_OTSP06_MCQ", "DSU_OTSP07_MCQ",
  "DSU_OTSP08_MCQ", "DSU_OTSP09_MCQ", "DSU_OTSP10_MCQ"
)

# Remove the listed variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(vars_to_remove))

# 4.23 Internet access (INT) n = 2664 #### 
vars_to_remove <- c("INT_WYSSCL_MNF_MCQ","INT_WYSSCL_FRI_MCQ",
                    "INT_WYSSCL_FAM_MCQ","INT_WYSSCL_PRO_MCQ", "INT_WYSSCL_OT_MCQ","INT_WYSSCL_DK_NA_MCQ",
                    "INT_WYSSCL_REFUSED_MCQ", "INT_FRQOT_MCQ"
)

# Remove the listed variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(vars_to_remove))


# 4.24 Local environment (ENV) n = 2650 #### 
vars_to_remove <- c("ENV_HMPRB_NOI_MCQ","ENV_HMPRB_LEA_MCQ","ENV_HMPRB_CON_MCQ",
 "ENV_HMPRB_EP_MCQ","ENV_HMPRB_HEA_MCQ","ENV_HMPRB_MAI_MCQ","ENV_HMPRB_INF_MCQ","ENV_HMPRB_OT_MCQ",
 "ENV_HMPRB_NONE_MCQ","ENV_HMPRB_DK_NA_MCQ","ENV_HMPRB_REFUSED_MCQ","ENV_HMPRB_OTSP01_MCQ",
 "ENV_HMPRB_OTSP02_MCQ","ENV_HMPRB_OTSP03_MCQ"
)

# Remove the listed variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(vars_to_remove))

# 4.25 Physical activity (PA2) n = 2510 #### 
exceptions <- c("PA2_SIT_MCQ", "PA2_WALK_MCQ", "PA2_WALKHR_MCQ", "PA2_LSPRT_MCQ",
                "PA2_LSPRTHR_MCQ", "PA2_MSPRT_MCQ", "PA2_MSPRTHR_MCQ", "PA2_SSPRT_MCQ",
                "PA2_SSPRTHR_MCQ", "PA2_EXER_MCQ", "PA2_EXERHR_MCQ", "PA2_DSCR2_MCQ",
                "PA2_DSCR_MCQE")
# Identify all variables that start with "PA2_" except for the exceptions
pa2_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "PA2_") & !names(CLSA.baseline.v1.2) %in% exceptions]
# Report the variables that will be removed
cat("Variables to be removed:", pa2_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(pa2_vars_to_remove))

# 4.26 Transportation, mobility, migration (TRA) n = 2350 ####
#keep "TRA_DSTATUS_MCQ" and remove all other "TRA_"
tra_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "TRA_") & names(CLSA.baseline.v1.2) != "TRA_DSTATUS_MCQ"]
# Report the variables that will be removed
cat("Variables to be removed:", tra_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(tra_vars_to_remove))

# 4.27 Wealth (WEA) n = 2295 #### 
# keep "WEA_SVNGSVL_MCQ, WEA_LFINS_MCQ" remove all other "WEA_"
wea_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "WEA_") & !names(CLSA.baseline.v1.2) %in% c("WEA_SVNGSVL_MCQ", "WEA_LFINS_MCQ")]
# Report the variables that will be removed
cat("Variables to be removed:", wea_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(wea_vars_to_remove))

# 4.28 Social networks (SN) n = 2240 #### 
# keep "SN_LIVH_NB_COM" and remove all other "SN_"
sn_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "SN_") & names(CLSA.baseline.v1.2) != "SN_LIVH_NB"]
# Report the variables that will be removed
cat("Variables to be removed:", sn_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(sn_vars_to_remove))

# 4.29 Walking status  (WLK) n =  2230 #### 
# keep "WLK_TIME_COM" and remove all other "WLK_"
# Identify all variables that start with "WLK_" except for "WLK_TIME_COM"
wlk_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "WLK_") & names(CLSA.baseline.v1.2) != "WLK_TIME_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", wlk_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(wlk_vars_to_remove))

# 4.30 Blood pressure (BP) n = 2204 #### 
# keep "BP_STATUS_COM, BP_SYSTOLIC_ALL_AVG_COM, BP_DIASTOLIC_ALL_AVG_COM,
# BP_PULSE_ALL_AVG_COM) and remove all other "BP_"
bp_keep <- c("BP_STATUS_COM", "BP_SYSTOLIC_ALL_AVG_COM", "BP_DIASTOLIC_ALL_AVG_COM", "BP_PULSE_ALL_AVG_COM")
bp_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "BP_") & !names(CLSA.baseline.v1.2) %in% bp_keep]
# Report the variables that will be removed
cat("Variables to be removed:", bp_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(bp_vars_to_remove))


# 4.31 Mobility and balance (TUG) n = 2199 #### 
# remove "TUG_DEVICE_COM, TUG_DEVICE_SP_COM"
tug_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "TUG_") & !names(CLSA.baseline.v1.2) %in% c("TUG_DEVICE_COM", "TUG_DEVICE_SP_COM")]
# Report the variables that will be removed
cat("Variables to be removed:", tug_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(tug_vars_to_remove))


# 4.32 ECG n = 2180 #### 
# keep "ECG_STATUS_COM, ECG_QUALITY_COM, ECG_RESULT_COM, ECG_DIAGNOSIS_DETAILS_COM)
# and remove all other "ECG_"
exceptions <- c("ECG_STATUS_COM", "ECG_QUALITY_COM", "ECG_RESULT_COM", "ECG_DIAGNOSIS_DETAILS_COM")
# Identify all variables that start with "ECG_" except for the exceptions
ecg_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "ECG_") & !names(CLSA.baseline.v1.2) %in% exceptions]
# Report the variables that will be removed
cat("Variables to be removed:", ecg_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ecg_vars_to_remove))

# 4.33 Spirometry (SPR) n = 2133 #### 
# remove all "SPR_" 
# Identify all variables that start with "SPR_"
spr_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "SPR_")]
# Report the variables that will be removed
cat("Variables to be removed:", spr_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(spr_vars_to_remove))

# 4.34 Grip strength (GS)  n = 2123 #### 
# keep "GS_EXAM_MAX_COM, GS_EXAM_AVG_COM" and remove all other "GS_"
gs_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "GS_") & 
                                                 !names(CLSA.baseline.v1.2) %in% c("GS_EXAM_MAX_COM", "GS_EXAM_AVG_COM")]

# Report the variables that will be removed
cat("Variables to be removed:", gs_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(gs_vars_to_remove))



# 4.35 Height (HGT), weight (WGT), and BMI n = 2120 #### 
# keep "HGT_HEIGHT_M_COM, WGT_WEIGHT_KG_COM, HWT_DBMI_COM)
# and remove all other "HGT_ , WGT_, and HWT_"
hgt_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "HGT_") & names(CLSA.baseline.v1.2) != "HGT_HEIGHT_M_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", hgt_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(hgt_vars_to_remove))

wgt_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "WGT_") & names(CLSA.baseline.v1.2) != "WGT_WEIGHT_KG_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", wgt_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(wgt_vars_to_remove))

hwt_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "HWT_") & names(CLSA.baseline.v1.2) != "HWT_DBMI_COM"]
# Report the variables that will be removed
cat("Variables to be removed:", hwt_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(hwt_vars_to_remove))

# 4.36 Functional status (FUL) n = 2091 #### 
# List of variables to keep
keep_vars <- c("FUL_SHLD_COM", "FUL_STOOP_COM", "FUL_PUSH_COM", "FUL_LFT10_COM",
               "FUL_HDLG_COM", "FUL_ST15_COM", "FUL_SIT1H_COM", "FUL_STDUP_COM",
               "FUL_FSTR_COM", "FUL_WK23B_COM", "FUL_MKBED_COM", "FUL_WSHBK_COM",
               "FUL_KNCUT_COM", "FUL_FORC_COM")

# Identify all variables that start with "FUL_" except for the specified list
ful_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "FUL_") & !names(CLSA.baseline.v1.2) %in% keep_vars]

# Report the variables that will be removed
cat("Variables to be removed:", ful_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ful_vars_to_remove))


# 4.37 Traumatic Brain Injury (TBI) n = 2055 #### 
# keep "TBI_NMBR_NB_COM, TBI_POSITIVE_COM" and remove "TBI_"
exceptions <- c("TBI_NMBR_NB_COM", "TBI_POSITIVE_COM")
tbi_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "TBI_") & !names(CLSA.baseline.v1.2) %in% exceptions]

# Report the variables that will be removed
cat("Variables to be removed:", tbi_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(tbi_vars_to_remove))

# 4.38 Chest pain (ROS) n = 2039 ####  
#keep "ROS_PAIN_COM, ROS_HILL_COM" and remove "ROS_"
ros_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "ROS_") & 
                                                  !names(CLSA.baseline.v1.2) %in% c("ROS_PAIN_COM", "ROS_HILL_COM")]

# Report the variables that will be removed
cat("Variables to be removed:", ros_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ros_vars_to_remove))


# 4.39 Contraindication questionnaire (ICQ) n = 2011 ####
exceptions <- c("ICQ_RISEASSI_COM", "ICQ_RISECANE_COM", "ICQ_ABLESTND_COM", "ICQ_ABLEWLK_COM", 
                "ICQ_ILLLUNG_COM", "ICQ_HRTCOND_COM", "ICQ_EMB6WK_COM", "ICQ_EMBMED_COM", 
                "ICQ_DERET3MO_COM", "ICQ_EYEINF_COM", "ICQ_EARINF_COM", "ICQ_CATRCT_COM", 
                "ICQ_CATRCT2_COM", "ICQ_GLAUC_COM", "ICQ_HRAID_COM", "ICQ_HRWORKING_COM", 
                "ICQ_GLASSES_COM", "ICQ_GLASSESWR_COM", "ICQ_CTLENS_COM", "ICQ_CTLENSWR_COM", 
                "ICQ_PROSLIM_COM", "ICQ_PROSARM_COM", "ICQ_PROSLEG_COM", "ICQ_PROSHND_COM", 
                "ICQ_PROSFT_COM", "ICQ_PROSHIP_COM", "ICQ_PROSKNEE_COM", "ICQ_FX_COM", 
                "ICQ_FXARM_COM", "ICQ_FXSHLD_COM", "ICQ_FXHND_COM", "ICQ_FXRIB_COM", 
                "ICQ_FXLEG_COM", "ICQ_FXANK_COM", "ICQ_FXFT_COM", "ICQ_FXHIP_COM", 
                "ICQ_FXKNEE_COM", "ICQ_FXCHK_COM", "ICQ_FXJAW_COM", "ICQ_FXNOSE_COM", 
                "ICQ_FXSKL_COM", "ICQ_FXNECK_COM", "ICQ_FXBACK_COM", "ICQ_FXCOLLR_COM", 
                "ICQ_FXPELV_COM", "ICQ_LAMIN_COM", "ICQ_POLIO_COM", "ICQ_BLDSP3MO_COM", 
                "ICQ_ANEURY_COM", "ICQ_PACEMKR_COM", "ICQ_DEFIBR_COM", "ICQ_COCHLIMP_COM", 
                "ICQ_NGTUBE_COM", "ICQ_ABDTUBE_COM", "ICQ_CHEMO4WK_COM", "ICQ_HAEMO_COM", 
                "ICQ_BLDTR24H_COM", "ICQ_NUCLMED_COM", "ICQ_NUCL48H_COM", "ICQ_NUCLTEST_SP_COM", 
                "ICQ_NUCLIV24H_COM", "ICQ_BARSWAL_COM", "ICQ_TINNIT_COM", "ICQ_SMOKE_COM")

# Identify all variables that start with "ICQ_" except for the exceptions
icq_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "ICQ_") & !names(CLSA.baseline.v1.2) %in% exceptions]

# Report the variables that will be removed
cat("Variables to be removed:", icq_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(icq_vars_to_remove))

# 4.40 DEXA Scans n = 1475 ####
# Remove all "DXA_"
dxa_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "DXA_")]
# Report the variables that will be removed
cat("Variables to be removed:", dxa_vars_to_remove, "\n")
# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(dxa_vars_to_remove))

# 4.41 Hearing (HRG) n = 1416 #### 
# keep "HRG_AID_COM, HRG_EARSTATUS_L_COM, HRG_EARSTATUS_R_COM"
# and remove all other "HRG_"
exceptions <- c("HRG_AID_COM", "HRG_EARSTATUS_L_COM", "HRG_EARSTATUS_R_COM")
ihrg_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "HRG_") & !names(CLSA.baseline.v1.2) %in% exceptions]

# Report the variables that will be removed
cat("Variables to be removed:", ihrg_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ihrg_vars_to_remove))

# 4.42 Balance (BAL) n = 1408 #### 
# keep "BAL_TIME_R_COM, BAL_TIME_L_COM, BAL_BEST_COM"
# and remove all other "BAL_"
exceptions <- c("BAL_TIME_R_COM", "BAL_TIME_L_COM", "BAL_BEST_COM")
bal_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "BAL_") & !names(CLSA.baseline.v1.2) %in% exceptions]

# Report the variables that will be removed
cat("Variables to be removed:", bal_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(bal_vars_to_remove))


# 4.43 Tonometry (TON) n = 1396 #### 
# keep "TON_QUALITYINDEX_R_COM, TON_QUALITYINDEX_L_COM"
# remove all other (TON_)
ton_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "TON_") & 
                                                  !names(CLSA.baseline.v1.2) %in% c("TON_QUALITYINDEX_R_COM", "TON_QUALITYINDEX_L_COM")]

# Report the variables that will be removed
cat("Variables to be removed:", ton_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(ton_vars_to_remove))

# 4.44 Visual acuity (VA_) n = 1364 #### 
# keep "VA_ETDRS_BOTH_RSLT_COM" and remove "VA_"
va_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "VA_") & names(CLSA.baseline.v1.2) != "VA_ETDRS_BOTH_RSLT_COM"]

# Report the variables that will be removed
cat("Variables to be removed:", va_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(va_vars_to_remove))



# 4.45 Waste/hip circumference in cm (WHC_) n = 1360 #### 
# keep "WHC_WAIST_CM_COM, WHC_HIP_CM_COM"
# and remove all other (WHC_)
whc_vars_to_remove <- names(CLSA.baseline.v1.2)[startsWith(names(CLSA.baseline.v1.2), "WHC_") & !names(CLSA.baseline.v1.2) %in% c("WHC_WAIST_CM_COM", "WHC_HIP_CM_COM")]

# Report the variables that will be removed
cat("Variables to be removed:", whc_vars_to_remove, "\n")

# Remove the identified variables
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(whc_vars_to_remove))

# 4.46 CANUE environmental data n = 1282 ####

# Define the variable pairs to coalesce
variable_pairs <- tribble(
  ~com, ~trm,
  "bp_1yr_cold_com", "bp_1yr_cold_trm",
  "bp_1yr_com", "bp_1yr_trm",
  "bp_1yr_warm_com", "bp_1yr_warm_trm",
  "bp_5yrs_cold_com", "bp_5yrs_cold_trm",
  "bp_5yrs_com", "bp_5yrs_trm",
  "bp_5yrs_warm_com", "bp_5yrs_warm_trm",
  "bp_7days_cold_com", "bp_7days_cold_trm",
  "bp_7days_warm_com", "bp_7days_warm_trm",
  "bp_exp_com", "bp_exp_trm",
  "Locrds_length_200m_com", "Locrds_length_200m_trm",
  "no2_1yr_cold_com", "no2_1yr_cold_trm",
  "no2_1yr_com", "no2_1yr_trm",
  "no2_1yr_warm_com", "no2_1yr_warm_trm",
  "no2_5yrs_cold_com", "no2_5yrs_cold_trm",
  "no2_5yrs_com", "no2_5yrs_trm",
  "no2_5yrs_warm_com", "no2_5yrs_warm_trm",
  "no2_7days_cold_com", "no2_7days_cold_trm",
  "no2_7days_warm_com", "no2_7days_warm_trm",
  "no2_exp_com", "no2_exp_trm",
  "NO2F_com", "NO2F_trm",
  "o3_1yr_cold_com", "o3_1yr_cold_trm",
  "o3_1yr_com", "o3_1yr_trm",
  "o3_1yr_warm_com", "o3_1yr_warm_trm",
  "o3_5yrs_cold_com", "o3_5yrs_cold_trm",
  "o3_5yrs_com", "o3_5yrs_trm",
  "o3_5yrs_warm_com", "o3_5yrs_warm_trm",
  "o3_7days_cold_com", "o3_7days_cold_trm",
  "o3_7days_warm_com", "o3_7days_warm_trm",
  "o3_exp_com", "o3_exp_trm",
  "o38h_1yr_cold_com", "o38h_1yr_cold_trm",
  "o38h_1yr_com", "o38h_1yr_trm",
  "o38h_1yr_warm_com", "o38h_1yr_warm_trm",
  "o38h_5yrs_cold_com", "o38h_5yrs_cold_trm",
  "o38h_5yrs_com", "o38h_5yrs_trm",
  "o38h_5yrs_warm_com", "o38h_5yrs_warm_trm",
  "o38h_7days_cold_com", "o38h_7days_cold_trm",
  "o38h_7days_warm_com", "o38h_7days_warm_trm",
  "o38h_exp_com", "o38h_exp_trm",
  "Phwy_length_200m_com","Phwy_length_200m_trm", 
  "PM_08_12_trm", "PM_08_12_com",
  "PM25_08_12_com", "PM25_08_12_trm",
  "pm25_1yr_cold_com", "pm25_1yr_cold_trm",
  "pm25_1yr_com", "pm25_1yr_trm",
  "pm25_1yr_warm_com", "pm25_1yr_warm_trm",
  "pm25_5yrs_cold_com", "pm25_5yrs_cold_trm",
  "pm25_5yrs_com", "pm25_5yrs_trm",
  "pm25_5yrs_warm_com", "pm25_5yrs_warm_trm",
  "pm25_7days_cold_com", "pm25_7days_cold_trm",
  "pm25_7days_warm_com", "pm25_7days_warm_trm",
  "pm25_exp_com", "pm25_exp_trm",
  "rh_1yr_cold_com", "rh_1yr_cold_trm",
  "rh_1yr_com", "rh_1yr_trm",
  "rh_1yr_warm_com", "rh_1yr_warm_trm",
  "rh_5yrs_cold_com", "rh_5yrs_cold_trm",
  "rh_5yrs_com", "rh_5yrs_trm",
  "rh_5yrs_warm_com", "rh_5yrs_warm_trm",
  "rh_7days_cold_com", "rh_7days_cold_trm",
  "rh_7days_warm_com", "rh_7days_warm_trm",
  "rh_exp_com", "rh_exp_trm",
  "so2_1yr_cold_com", "so2_1yr_cold_trm",
  "so2_1yr_com", "so2_1yr_trm",
  "so2_1yr_warm_com", "so2_1yr_warm_trm",
  "so2_5yrs_cold_com", "so2_5yrs_cold_trm",
  "so2_5yrs_com", "so2_5yrs_trm",
  "so2_5yrs_warm_com", "so2_5yrs_warm_trm",
  "so2_7days_cold_com", "so2_7days_cold_trm",
  "so2_7days_warm_com", "so2_7days_warm_trm",
  "so2_exp_com", "so2_exp_trm",
  "temp_1yr_cold_com", "temp_1yr_cold_trm",
  "temp_1yr_com", "temp_1yr_trm",
  "temp_1yr_warm_com", "temp_1yr_warm_trm",
  "temp_5yrs_cold_com", "temp_5yrs_cold_trm",
  "temp_5yrs_com", "temp_5yrs_trm",
  "temp_5yrs_warm_com", "temp_5yrs_warm_trm",
  "temp_7days_cold_com", "temp_7days_cold_trm",
  "temp_7days_warm_com", "temp_7days_warm_trm",
  "temp_exp_com", "temp_exp_trm"
)

# Coalesce each pair and create a new variable without the suffix
for (i in 1:nrow(variable_pairs)) {
  com_col <- variable_pairs$com[i]
  trm_col <- variable_pairs$trm[i]
  new_var <- str_replace(com_col, "_com$", "")
  
  # Check if both columns exist before attempting to coalesce
  if (all(c(com_col, trm_col) %in% names(CLSA.baseline.v1.2))) {
    CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
      mutate(!!new_var := coalesce(!!sym(com_col), !!sym(trm_col)))
  } else {
    warning(paste("Variables", com_col, "and/or", trm_col, "do not exist in the dataset."))
  }
}

# Gather the names of all com and trm variables to be removed
vars_to_remove <- c(variable_pairs$com, variable_pairs$trm)

# Remove the original variables with "_com" and "_trm" suffixes
CLSA.baseline.v1.2 <- CLSA.baseline.v1.2 %>%
  select(-all_of(vars_to_remove))



#### 5.0 Baseline dataset - remove additional variables  ####

# Save the modified dataset (n=1282)
write.csv(CLSA.baseline.v1.2, file = "CLSA.baseline.4.reducedvars.csv")


# Review data set and manually identify redundant variables

# remove diabetes drug items (diagnosis, type, and onset of insulin use retained)
# remove all fall related items (retaining the number of falls in the past 12 months)
# remove hyperthyroidism medication (keep age of onset)
# remove IADL scale singe items (already kept summary items)
# remove  income items (keep house and personal income, RRSP, old age security)
# remove injury items (keep number of injuries)
# remove social media items for specific use cases 
# remove follow-up labor force questions
# remove follow-up retirement items
# remove follow-up labor force items
# parental background (keeping personal background)
# language 
# remove follow-up smoking questions
# stroke follow-up
# veteran status follow-up
# visual acuity follow-up

columns_to_remove <- c("DIA_EVPRG_COM", "DIA_PRGDIA_COM", "DIA_MED_COM", "DIA_MEDCUR_COM", 
                       "DIA_MEDINSNAME_HUMU_COM", "DIA_MEDINSNAME_LEVE_COM", "DIA_MEDINSNAME_HUMA_COM", 
                       "DIA_MEDINSNAME_APID_COM", "DIA_MEDINSNAME_NOVOR_COM", "DIA_MEDINSNAME_LANT_COM", 
                       "DIA_MEDINSNAME_NOVOM_COM", "DIA_MEDINSNAME_OT_COM", "DIA_MEDINSNAME_NONE_COM", 
                       "DIA_MEDINSNAME_MISSING_COM", "DIA_MEDINSNAME_DK_NA_COM", "DIA_MEDINSNAME_REFUSED_COM", 
                       "DIA_MEDINSNAME_OTSP_COM", "FAL_MOST", "FAL_MOST_OTSP", "FAL_ATTN", "FAL_HOSP",
                       "FAL_FU", "FAL_WHERE", "FAL_HOW", "FAL_HOW_OTSP", "FAL_DV",  "FAL_DVCTR", "FAL_DVDSC", "FAL_LDR", "FAL_LDRDSC", "FAL_LDRDSC_OTSP",
                       "FAL_STL", "FAL_STLDSC", "FAL_STLDSC_OTSP", "FAL_BED", "FAL_BDDSC",
                       "FAL_BDDSC_OTSP", "FAL_CHR", "FAL_CHDSC", "FAL_CHDSC_OTSP", "FAL_FURN",
                       "FAL_FURN_SP", "FAL_FRNDSC", "FAL_FRNDSC_OTSP", "FAL_RUG", "FAL_RGDSC",
                       "FAL_RGDSC_OTSP", "FAL_FLR", "FAL_FLDSC", "FAL_FLDSC_OTSP", "FAL_ELEC",
                       "FAL_ELDSC", "FAL_ELDSC_OTSP", "FAL_FOOT", "FAL_FTDSC", "FAL_FTDSC_OTSP",
                       "FAL_CLTH", "FAL_CLTH_SP", "FAL_TOY", "FAL_TOYDSC", "FAL_TOYDSC_OTSP",
                       "FAL_YRD", "FAL_YRDDSC", "FAL_BIKE", "FAL_BKDSC",
                       "FAL_BKDSC_OTSP", "FAL_SPRT", "FAL_SPRT_SP", 
                       "FAL_ELSE", "FAL_ELSE_SP", "FAL_ELSEDSC", "FAL_ELSEDSC_OTSP",
                       "HYP_UTHYRMEDCUR_COM", "HYP_UTHYREVRMED_COM", "HYP_OTHYRMED_COM", "HYP_OTHYRMEDCUR_COM",
                       "IAL_ABLTEL", "IAL_HPTEL", "IAL_UNTEL", "IAL_ABLTRV",
                       "IAL_HPTRV", "IAL_UNTRV", "IAL_ABLGRO", "IAL_HPGRO", "IAL_UNGRO", 
                       "IAL_ABLML", "IAL_HPML", "IAL_UNML", "IAL_ABLWRK", "IAL_HPWRK", "IAL_UNWRK",
                       "IAL_ABLMED", "IAL_HPMED", "IAL_UNMED", "IAL_ABLMO", "IAL_HPMO", "IAL_UNMO",
                       "INC_SRCE_SE", "INC_SRCE_IN", "INC_SRCE_EI", "INC_SRCE_CM", "INC_SRCE_BN", 
                       "INC_SRCE_PN", "INC_SRCE_GIS", "INC_SRCE_WF", "INC_SRCE_CH", "INC_SRCE_SP", 
                       "INC_SRCE_AL", "INC_SRCE_CP", "INC_SRCE_NONE", "INC_SRCE_OT", "INC_SRCE_DK_NA", 
                       "INC_SRCE_REFUSED", "INC_PSRCE_SE", "INC_PSRCE_IN", "INC_PSRCE_EI", 
                       "INC_PSRCE_CM", "INC_PSRCE_BN", "INC_PSRCE_PN", "INC_PSRCE_GIS", "INC_PSRCE_WF",
                       "INC_PSRCE_CH", "INC_PSRCE_SP", "INC_PSRCE_AL", "INC_PSRCE_CP", "INC_PSRCE_NONE",
                       "INC_PSRCE_OT", "INC_PSRCE_DK_NA", "INC_PSRCE_REFUSED", "INJ_NMBR_NB", 
                       "INJ_CAUS_FL", "INJ_CAUS_VH", "INJ_CAUS_WK", "INJ_CAUS_NONE", "INJ_CAUS_DK_NA",
                       "INJ_CAUS_REFUSED", "INJ_HOW", "INJ_HOW_OTSP", "INJ_WHR", "INJ_WHR_OTSP",
                       "INJ_ACT", "INJ_ACT_OTSP", "INJ_TYPE", "INJ_TYPE_OTSP", "INJ_BRKN", "INJ_SITE_ML", "INJ_SITE_EYE",
                       "INJ_SITE_HD", "INJ_SITE_NE", "INJ_SITE_SH", "INJ_SITE_EL", "INJ_SITE_WR",
                       "INJ_SITE_HIP", "INJ_SITE_TH", "INJ_SITE_KN", "INJ_SITE_AN", "INJ_SITE_UP",
                       "INJ_SITE_LO", "INJ_SITE_CH", "INJ_SITE_AB", "INJ_SITE_OT", "INJ_SITE_DK_NA",
                       "INJ_SITE_REFUSED", "INJ_SITE_OTSP", "INJ_SITE_FN", "INJ_SITE_TOE", "LBF_SCHD", 
                       "LBF_SCHD_OTSP", "LBF_TYPE_NB", "LBF_BUSN_NB", "LBF_DURN",
                       "LBF_RSN", "LBF_RSN_OTSP", "LBF_NVR_OW", "LBF_NVR_CH", "LBF_NVR_EL",
                       "LBF_NVR_SP", "LBF_NVR_OT", "LBF_NVR_OTSP", "LBF_NVR_REFUSED", "LBF_LGEVER",
                       "LBF_LGSTAT", "LBF_LGSCHD", "LBF_LGSCHD_OTSP", "LBF_LGTYPE_SP", "LBF_LGIND_SP",
                       "LBF_LGDURN", "RET_WHY_UN",
                       "RET_WHY_RL", "RET_WHY_FA", "RET_WHY_AD", 
                       "RET_WHY_UW", "RET_WHY_SB", "RET_WHY_CW", "RET_WHY_GR", "RET_WHY_NA", 
                       "RET_WHY_CM", "RET_WHY_RE", "RET_WHY_HL", "RET_WHY_IN", "RET_WHY_OR", 
                       "RET_WHY_PR", "RET_WHY_MD", "RET_WHY_HO", "RET_WHY_ST", "RET_WHY_AG", 
                       "RET_WHY_OT", "RET_WHY_DK_NA", "RET_WHY_REFUSED", "RET_WHY_OTSP",
                       "LFP_SCHD", "LFP_SCHD_OTSP", "LFP_TYPE_SP", "LFP_IND_SP", "LFP_LNGST",
                       "LFP_LGPAY", "LFP_LGHR", "LFP_LGSCHD", "LFP_LGSCHD_OTSP", "LFP_LGTYPE_SP",
                       "LFP_LGIND_SP", "LFP_LGYRS", "LBF_EVER", "LBF_CURR",
                       "LBF_MANY", "LBF_STTS", "LBF_SCHD", "LBF_SCHD_OTSP", "LBF_TYPE_NB",
                       "LBF_BUSN_NB", "LBF_DURN", "LBF_RSN", "LBF_RSN_OTSP", "LBF_NVR_OW",
                       "LBF_NVR_CH", "LBF_NVR_EL", "LBF_NVR_SP", "LBF_NVR_OT", "LBF_NVR_OTSP",
                       "LBF_NVR_REFUSED", "LBF_LGEVER", "LBF_LGSTAT", "LBF_LGSCHD", "LBF_LGSCHD_OTSP",
                       "LBF_LGTYPE_SP", "LBF_LGIND_SP", "LBF_LGDURN", "SDC_ETHN_CA", "SDC_ETHN_FR",
                       "SDC_ETHN_EN", "SDC_ETHN_DE", "SDC_ETHN_GD", "SDC_ETHN_GA", "SDC_ETHN_IT",
                       "SDC_ETHN_UK","SDC_ETHN_NL", "SDC_ETHN_ZH", "SDC_ETHN_HE", "SDC_ETHN_PL",
                       "SDC_ETHN_PT", "SDC_ETHN_SA", "SDC_ETHN_NO", "SDC_ETHN_CY", "SDC_ETHN_SV",
                       "SDC_ETHN_OT", "SDC_ETHN_DK_NA", "SDC_ETHN_REFUSED", "SDC_ETHN_OTSP01", 
                       "SDC_ETHN_OTSP02", "SDC_ETHN_OTSP03", "SDC_CULT_OT", 
                       "SDC_CULT_DK_NA", "SDC_CULT_REFUSED", "SDC_CULT_OTSP","SDC_LANG_EN", 
                       "SDC_LANG_FR", "SDC_LANG_AR", "SDC_LANG_CN", "SDC_LANG_DE", "SDC_LANG_EL",
                       "SDC_LANG_HU", "SDC_LANG_IT", "SDC_LANG_KO", "SDC_LANG_MA", "SDC_LANG_FA",
                       "SDC_LANG_PL", "SDC_LANG_PT", "SDC_LANG_PJ", "SDC_LANG_ES", "SDC_LANG_TL",
                       "SDC_LANG_UK", "SDC_LANG_VI", "SDC_LANG_NL", "SDC_LANG_HI", "SDC_LANG_RU",
                       "SDC_LANG_TA", "SDC_LANG_OT", "SDC_LANG_DK_NA", "SDC_LANG_REFUSED", 
                       "SDC_LANG_OTSP01", "SDC_LANG_OTSP02", "SDC_LANG_OTSP03", "SDC_LANG_OTSP04", 
                       "SDC_LGMST", "SDC_FTLG_EN", "SDC_FTLG_FR", "SDC_FTLG_AR", "SDC_FTLG_CN", 
                       "SDC_FTLG_DE", "SDC_FTLG_EL", "SDC_FTLG_HU", "SDC_FTLG_IT", "SDC_FTLG_KO", 
                       "SDC_FTLG_MA", "SDC_FTLG_FA", "SDC_FTLG_PL", "SDC_FTLG_PT", "SDC_FTLG_PJ", 
                       "SDC_FTLG_ES", "SDC_FTLG_TL", "SDC_FTLG_UK", "SDC_FTLG_VI", "SDC_FTLG_NL", 
                       "SDC_FTLG_HI", "SDC_FTLG_RU", "SDC_FTLG_TA", "SDC_FTLG_OT", "SDC_FTLG_DK_NA",
                       "SDC_FTLG_REFUSED", "SMK_NBCG_NB",
                       "SMK_YRDL_NB", "SMK_FRQDL", "SMK_FRQDL_NB", "SMK_LST30", "SMK_NB30", 
                       "SMK_NB30_NB", "SMK_EVRDL", "SMK_SMKDL_AG", "SMK_NBDL", "SMK_NBDL_NB", 
                       "SMK_TOTYR_NB", "SMK_STOP", "SMK_OTREG","SMK_TYPEOT_CG", "SMK_TYPEOT_SM", 
                       "SMK_TYPEOT_PI", "SMK_TYPEOT_CH","SMK_TYPEOT_PT", "SMK_TYPEOT_GU", 
                       "SMK_TYPEOT_BE", "SMK_TYPEOT_PN","SMK_TYPEOT_SH", "SMK_TYPEOT_OT", 
                       "SMK_TYPEOT_DK_NA", "SMK_TYPEOT_REFUSED", "SMK_TYPEOT_OTSP", "SMK_TYPEOT_CA", "SMK_OTOCC",
                       "SMK_OTCURR_CG", "SMK_OTCURR_SM", "SMK_OTCURR_PI", "SMK_OTCURR_CH",
                       "SMK_OTCURR_PT", "SMK_OTCURR_GU", "SMK_OTCURR_BE", "SMK_OTCURR_PN",
                       "SMK_OTCURR_SH", "SMK_OTCURR_OT", "SMK_OTCURR_DK_NA", "SMK_OTCURR_REFUSED",
                       "SMK_OTCURR_OTSP", "SMK_OTCURR_CA", "SMK_CHILD_NB", "SMK_ADULT_NB", "SMK_HOME",
                       "SMK_ACTV", "SMK_YEAR_YR",
                       "VET_OCC_OUTSIDE", "VET_SERV_AR", "VET_SERV_NV", "VET_SERV_AF", 
                       "VET_SERV_RES", "VET_SERV_OT", "VET_SERV_DK_NA", "VET_SERV_REFUSED", "VET_SERV_RESSP", 
                       "VET_SERV_RESOTSP", "VET_SERV_OTSP", "VET_CRNT", "VET_RLSE_YR", "VET_JOIN_YR",
                       "VIS_USE_MG", "VIS_USE_BR", "VIS_USE_LG", "VIS_USE_TK", "VIS_USE_RC", "VIS_USE_CC", 
                       "VIS_USE_CP", "VIS_USE_CN", "VIS_USE_DG", "VIS_USE_OT", "VIS_USE_DK_NA",
                       "VIS_USE_REFUSED", "VIS_USE_OTSP", "VIS_USE_GL", "VIS_USE_LN", "VIS_USE_DR", 
                        "VIS_USE_EB", "VIS_USE_LT", "VIS_USE_ZM")


CLSA.baseline.v1.3 <- CLSA.baseline.v1.2 %>%
  select(-all_of(columns_to_remove))
  

# View and save the modified dataset (n=893)
write.csv(CLSA.baseline.v1.3, file = "CLSA.baseline.5.fullreducedvars.csv")

  
# Score an re-code all remaining variables 
sorted_vars <- sort(names(CLSA.baseline.v1.3))
print(sorted_vars)


#### 6.0 Baseline dataset - remove non-response values (e.g., -9999s)  ####

# View and save the modified dataset (n= )
write.csv(CLSA.baseline.v1.3, file = "CLSA.baseline.5.fullreducedvars.csv")

# set options to bias towards decimal (c.f., scientific notation)
options(scipen = 999)

CLSA.baseline.clean <- CLSA.baseline.v1.3 %>%
  mutate(ADL_DCLS = replace(ADL_DCLS, ADL_DCLS == 9, NA),
         ADL_DSUM = replace(ADL_DSUM, ADL_DSUM == 9, NA),
         ALE16_02 = replace(ALE16_02 , ALE16_02 == -8888, NA),
         ALE16_03 = replace(ALE16_03 , ALE16_03 == -8888, NA),
         ALE16_04 = replace(ALE16_04 , ALE16_04 == -8888, NA),
         ALE16_05 = replace(ALE16_05 , ALE16_05 == -8888, NA),
         ALE16_06 = replace(ALE16_06 , ALE16_06 == -8888, NA),
         ALE16_07 = replace(ALE16_07 , ALE16_07 == -8888, NA),
         ALE16_08 = replace(ALE16_08 , ALE16_08 == -8888, NA),
         ALE16_09 = replace(ALE16_09 , ALE16_09 == -8888, NA),
         ALE16_10 = replace(ALE16_10 , ALE16_10 == -8888, NA),
         ALE16_11 = replace(ALE16_11 , ALE16_11 == -8888, NA),
         ALE16_12 = replace(ALE16_12 , ALE16_12 == -8888, NA),
         ALE16_13 = replace(ALE16_13 , ALE16_13 == -8888, NA),
         bp_1yr = replace( bp_1yr,  bp_1yr == -8888, NA),
         bp_1yr_cold = replace( bp_1yr_cold, bp_1yr_cold == -8888, NA),
         bp_1yr_warm = replace(bp_1yr_warm, bp_1yr_warm == -8888, NA),
         bp_5yrs = replace(bp_5yrs, bp_5yrs == -8888, NA),
         bp_5yrs_cold = replace(bp_5yrs_cold, bp_5yrs_cold == -8888, NA),
         bp_5yrs_warm = replace(bp_5yrs_warm, bp_5yrs_warm == -8888, NA),
         bp_7days_cold = replace(bp_7days_cold, bp_7days_cold == -8888, NA),
         bp_7days_warm = replace(bp_7days_warm, bp_7days_warm == -8888, NA),
         bp_exp = replace(bp_exp,bp_exp == -8888, NA),
         CAG_FPAS = replace(CAG_FPAS, CAG_FPAS == 9, NA),
         CAO_ASTHMAGE_NB_COM = replace(CAO_ASTHMAGE_NB_COM, CAO_ASTHMAGE_NB_COM == 9998, NA),
         CAO_COFAM_COM = replace(CAO_COFAM_COM, CAO_COFAM_COM  %in% c(8, 9), NA),
         CAO_COFMAM_COM = replace(CAO_COFMAM_COM, CAO_COFMAM_COM == 8, NA),
         CAO_COFPY_COM = replace(CAO_COFPY_COM, CAO_COFPY_COM %in% c(8, 9), NA),
         CAO_COLD_COM = replace(CAO_COLD_COM, CAO_COLD_COM %in% c(8, 9), NA),
         CAO_COPDAGE_NB_COM = replace(CAO_COPDAGE_NB_COM, CAO_COPDAGE_NB_COM == 9998, NA),
         CAO_EXERT_COM = replace(CAO_EXERT_COM, CAO_EXERT_COM %in% c(8, 9), NA),
         CAO_MED_COM = replace(CAO_MED_COM, CAO_MED_COM %in% c(-88, 98, 99), NA),
         CAO_PHLEGMPY_COM = replace(CAO_PHLEGMPY_COM, CAO_PHLEGMPY_COM %in% c(8, 9) , NA),
         CAO_SOBFLAT_COM = replace(CAO_SOBFLAT_COM, CAO_SOBFLAT_COM %in% c(8, 9), NA),
         CAO_SOBPM_COM = replace(CAO_SOBPM_COM, CAO_SOBPM_COM %in% c(8, 9), NA),
         CAO_SOBUP_COM = replace(CAO_SOBUP_COM, CAO_SOBUP_COM %in% c(8, 9), NA),
         CAO_WHEZ_COM = replace(CAO_WHEZ_COM, CAO_WHEZ_COM %in% c(8, 9), NA),
         CAO_WKCOF_COM = replace(CAO_WKCOF_COM, CAO_WKCOF_COM %in% c(8, 9), NA),
         CAO_WKSOB_COM = replace(CAO_WKSOB_COM, CAO_WKSOB_COM %in% c(8, 9) , NA),
         CAO_WKWHEZ_COM = replace(CAO_WKWHEZ_COM, CAO_WKWHEZ_COM %in% c(8, 9) , NA),
         CCC_ALLRG_COM = replace(CCC_ALLRG_COM, CCC_ALLRG_COM %in% c(8, 9), NA),
         CCC_ALZH_COM = replace(CCC_ALZH_COM, CCC_ALZH_COM %in% c(8, 9), NA),
         CCC_AMI_COM = replace(CCC_AMI_COM, CCC_AMI_COM %in% c(8, 9), NA),
         CCC_ANGI_COM = replace(CCC_ANGI_COM, CCC_ANGI_COM %in% c(8, 9), NA),
         CCC_ANXI_COM = replace(CCC_ANXI_COM, CCC_ANXI_COM %in% c(8, 9), NA),
         CCC_ASTHM_COM = replace(CCC_ASTHM_COM, CCC_ASTHM_COM %in% c(8, 9), NA),
         CCC_BCKP_COM = replace(CCC_BCKP_COM, CCC_BCKP_COM %in% c(8, 9), NA),
         CCC_BOWINC_COM = replace(CCC_BOWINC_COM, CCC_BOWINC_COM %in% c(8, 9), NA),
         CCC_CANC_COM = replace(CCC_CANC_COM, CCC_CANC_COM %in% c(8, 9), NA),
         CCC_CANTP_SM_COM = replace(CCC_CANTP_SM_COM, CCC_CANTP_SM_COM %in% c(8, 9), NA),
         CCC_COPD_COM = replace(CCC_COPD_COM, CCC_COPD_COM %in% c(8, 9), NA),
         CCC_CVA_COM = replace(CCC_CVA_COM, CCC_CVA_COM %in% c(8, 9), NA),
         CCC_CVAFX_COM = replace(CCC_CVAFX_COM, CCC_CVAFX_COM %in% c(8, 9), NA),
         CCC_DRFLU_COM = replace(CCC_DRFLU_COM, CCC_DRFLU_COM %in% c(8, 9), NA),
         CCC_DRPNEU_COM = replace(CCC_DRPNEU_COM, CCC_DRPNEU_COM %in% c(8, 9), NA),
         CCC_DRUTI_COM = replace(CCC_DRUTI_COM, CCC_DRUTI_COM %in% c(8, 9), NA),
         CCC_EPIL_COM = replace(CCC_EPIL_COM, CCC_EPIL_COM %in% c(8, 9), NA),
         CCC_F1_COM = replace(CCC_F1_COM , CCC_F1_COM %in% c(8, 9), NA),
         CCC_F2_COM = replace(CCC_F2_COM, CCC_F2_COM %in% c(8, 9), NA),
         CCC_HBP_COM = replace(CCC_HBP_COM, CCC_HBP_COM %in% c(8, 9), NA),
         CCC_HEART_COM = replace(CCC_HEART_COM, CCC_HEART_COM %in% c(8, 9), NA),
         CCC_IBDIBS_COM = replace(CCC_IBDIBS_COM, CCC_IBDIBS_COM %in% c(8, 9), NA),
         CCC_KIDN_COM = replace(CCC_KIDN_COM, CCC_KIDN_COM %in% c(8, 9), NA),
         CCC_MACDEG_COM = replace(CCC_MACDEG_COM, CCC_MACDEG_COM %in% c(8, 9), NA),
         CCC_MEMPB_COM = replace(CCC_MEMPB_COM, CCC_MEMPB_COM %in% c(8, 9), NA),
         CCC_MGRN_COM = replace(CCC_MGRN_COM, CCC_MGRN_COM %in% c(8, 9), NA),
         CCC_MOOD_COM = replace(CCC_MOOD_COM, CCC_MOOD_COM %in% c(8, 9), NA),
         CCC_MS_COM = replace(CCC_MS_COM, CCC_MS_COM %in% c(8, 9), NA),
         CCC_OAHAND_COM = replace(CCC_OAHAND_COM,CCC_OAHAND_COM %in% c(8, 9), NA),
         CCC_OAHIP_COM = replace(CCC_OAHIP_COM, CCC_OAHIP_COM %in% c(8, 9), NA),
         CCC_OAKNEE_COM = replace(CCC_OAKNEE_COM, CCC_OAKNEE_COM %in% c(8, 9), NA),
         CCC_OSTPO_COM = replace(CCC_OSTPO_COM, CCC_OSTPO_COM %in% c(8, 9), NA),
         CCC_PARK_COM = replace(CCC_PARK_COM , CCC_PARK_COM  %in% c(8, 9), NA),
         CCC_PVD_COM = replace(CCC_PVD_COM, CCC_PVD_COM %in% c(8, 9), NA),
         CCC_RA_COM = replace(CCC_RA_COM, CCC_RA_COM %in% c(8, 9), NA),
         CCC_TIA_COM = replace(CCC_TIA_COM, CCC_TIA_COM %in% c(8, 9), NA),
         CCC_ULCR_COM = replace(CCC_ULCR_COM, CCC_ULCR_COM %in% c(8, 9), NA),
         CCC_URIINC_COM = replace(CCC_URIINC_COM, CCC_URIINC_COM %in% c(8, 9), NA),
         CCC_UTHYR_COM = replace(CCC_UTHYR_COM, CCC_UTHYR_COM %in% c(8, 9), NA),
         CCT_ALLRG_TRM = replace(CCT_ALLRG_TRM, CCT_ALLRG_TRM %in% c(8, 9), NA),
         CCT_ALZH_TRM = replace(CCT_ALZH_TRM, CCT_ALZH_TRM %in% c(8, 9), NA),
         CCT_AMI_TRM = replace(CCT_AMI_TRM, CCT_AMI_TRM %in% c(8, 9), NA),
         CCT_ANGI_TRM = replace(CCT_ANGI_TRM, CCT_ANGI_TRM %in% c(8, 9), NA),
         CCT_ANXI_TRM = replace(CCT_ANXI_TRM, CCT_ANXI_TRM %in% c(8, 9), NA),
         CCT_ASTHM_TRM = replace(CCT_ASTHM_TRM, CCT_ASTHM_TRM %in% c(8, 9), NA),
         CCT_BCKP_TRM = replace(CCT_BCKP_TRM, CCT_BCKP_TRM %in% c(8, 9), NA),
         CCT_BOWINC_TRM = replace(CCT_BOWINC_TRM, CCT_BOWINC_TRM %in% c(8, 9), NA),
         CCT_CANC_TRM = replace(CCT_CANC_TRM, CCT_CANC_TRM %in% c(8, 9), NA),
         CCT_CANTP_BL_TRM = replace(CCT_CANTP_BL_TRM, CCT_CANTP_BL_TRM %in% c(8, 9), NA),
         CCT_CANTP_BN_TRM = replace(CCT_CANTP_BN_TRM, CCT_CANTP_BN_TRM %in% c(8, 9), NA),
         CCT_CANTP_BR_TRM = replace(CCT_CANTP_BR_TRM, CCT_CANTP_BR_TRM %in% c(8, 9), NA),
         CCT_CANTP_CNS_TRM = replace(CCT_CANTP_CNS_TRM, CCT_CANTP_CNS_TRM %in% c(8, 9), NA),
         CCT_CANTP_COL_TRM = replace(CCT_CANTP_COL_TRM, CCT_CANTP_COL_TRM %in% c(8, 9), NA),
         CCT_CANTP_DG_TRM = replace(CCT_CANTP_DG_TRM, CCT_CANTP_DG_TRM %in% c(8, 9), NA),
         CCT_CANTP_DK_NA_TRM = replace(CCT_CANTP_DK_NA_TRM, CCT_CANTP_DK_NA_TRM %in% c(8, 9), NA),
         CCT_CANTP_EG_TRM = replace(CCT_CANTP_EG_TRM, CCT_CANTP_EG_TRM %in% c(8, 9), NA),
         CCT_CANTP_FGO_TRM = replace(CCT_CANTP_FGO_TRM, CCT_CANTP_FGO_TRM %in% c(8, 9), NA),
         CCT_CANTP_KD_TRM = replace(CCT_CANTP_KD_TRM, CCT_CANTP_KD_TRM %in% c(8, 9), NA),
         CCT_CANTP_LHT_TRM = replace(CCT_CANTP_LHT_TRM, CCT_CANTP_LHT_TRM %in% c(8, 9), NA),
         CCT_CANTP_LK_TRM = replace(CCT_CANTP_LK_TRM, CCT_CANTP_LK_TRM %in% c(8, 9), NA),
         CCT_CANTP_LU_TRM = replace(CCT_CANTP_LU_TRM, CCT_CANTP_LU_TRM %in% c(8, 9), NA),
         CCT_CANTP_MGO_TRM = replace(CCT_CANTP_MGO_TRM, CCT_CANTP_MGO_TRM %in% c(8, 9), NA),
         CCT_CANTP_MST_TRM = replace(CCT_CANTP_MST_TRM, CCT_CANTP_MST_TRM %in% c(8, 9), NA),
         CCT_CANTP_NHL_TRM = replace(CCT_CANTP_NHL_TRM, CCT_CANTP_NHL_TRM %in% c(8, 9), NA),
         CCT_CANTP_OR_TRM = replace(CCT_CANTP_OR_TRM, CCT_CANTP_OR_TRM %in% c(8, 9), NA),
         CCT_CANTP_OT_TRM = replace(CCT_CANTP_OT_TRM, CCT_CANTP_OT_TRM %in% c(8, 9), NA),
         CCT_CANTP_OV_TRM = replace(CCT_CANTP_OV_TRM, CCT_CANTP_OV_TRM %in% c(8, 9), NA),
         CCT_CANTP_PA_TRM = replace(CCT_CANTP_PA_TRM, CCT_CANTP_PA_TRM %in% c(8, 9), NA),
         CCT_CANTP_PR_TRM = replace(CCT_CANTP_PR_TRM, CCT_CANTP_PR_TRM %in% c(8, 9), NA),
         CCT_CANTP_RS_TRM = replace(CCT_CANTP_RS_TRM, CCT_CANTP_RS_TRM %in% c(8, 9), NA),
         CCT_CANTP_SM_TRM = replace(CCT_CANTP_SM_TRM, CCT_CANTP_SM_TRM %in% c(8, 9), NA),
         CCT_CANTP_SNM_TRM = replace(CCT_CANTP_SNM_TRM, CCT_CANTP_SNM_TRM %in% c(8, 9), NA),
         CCT_CANTP_SNS_TRM = replace(CCT_CANTP_SNS_TRM, CCT_CANTP_SNS_TRM %in% c(8, 9), NA),
         CCT_CANTP_TH_TRM = replace(CCT_CANTP_TH_TRM, CCT_CANTP_TH_TRM %in% c(8, 9), NA),
         CCT_CANTP_UN_TRM = replace(CCT_CANTP_UN_TRM, CCT_CANTP_UN_TRM %in% c(8, 9), NA),
         CCT_CANTP_UR_TRM = replace(CCT_CANTP_UR_TRM, CCT_CANTP_UR_TRM %in% c(8, 9), NA),
         CCT_CATAR_TRM = replace(CCT_CATAR_TRM, CCT_CATAR_TRM %in% c(8, 9), NA),
         CCT_COPD_TRM = replace(CCT_COPD_TRM, CCT_COPD_TRM %in% c(8, 9), NA),
         CCT_CVA_TRM = replace(CCT_CVA_TRM, CCT_CVA_TRM %in% c(8, 9), NA),
         CCT_CVAFX_TRM = replace(CCT_CVAFX_TRM, CCT_CVAFX_TRM %in% c(8, 9), NA),
         CCT_DIAB_TRM = replace(CCT_DIAB_TRM, CCT_DIAB_TRM %in% c(8, 9), NA),
         CCT_DRFLU_TRM = replace(CCT_DRFLU_TRM, CCT_DRFLU_TRM %in% c(8, 9), NA),
         CCT_DROT_TRM = replace(CCT_DROT_TRM, CCT_DROT_TRM %in% c(8, 9), NA),
         CCT_DRPNEU_TRM = replace(CCT_DRPNEU_TRM, CCT_DRPNEU_TRM %in% c(8, 9), NA),
         CCT_DRUTI_TRM = replace(CCT_DRUTI_TRM, CCT_DRUTI_TRM %in% c(8, 9), NA),
         CCT_EPIL_TRM = replace(CCT_EPIL_TRM, CCT_EPIL_TRM %in% c(8, 9), NA),
         CCT_F2_TRM = replace(CCT_F2_TRM, CCT_F2_TRM %in% c(8, 9), NA),
         CCT_GLAUC_TRM = replace(CCT_GLAUC_TRM, CCT_GLAUC_TRM %in% c(8, 9), NA),
         CCT_HBP_TRM = replace(CCT_HBP_TRM, CCT_HBP_TRM %in% c(8, 9), NA),
         CCT_HBPOT_TRM = replace(CCT_HBPOT_TRM, CCT_HBPOT_TRM %in% c(8, 9), NA),
         CCT_HBPPRG_TRM = replace(CCT_HBPPRG_TRM, CCT_HBPPRG_TRM %in% c(8, 9), NA),
         CCT_HEART_TRM = replace(CCT_HEART_TRM, CCT_HEART_TRM %in% c(8, 9), NA),
         CCT_IBDIBS_TRM = replace(CCT_IBDIBS_TRM, CCT_IBDIBS_TRM %in% c(8, 9), NA),
         CCT_KIDN_TRM = replace(CCT_KIDN_TRM, CCT_KIDN_TRM %in% c(8, 9), NA),
         CCT_MACDEG_TRM = replace(CCT_MACDEG_TRM,CCT_MACDEG_TRM %in% c(8, 9), NA),
         CCT_MEMPB_TRM = replace(CCT_MEMPB_TRM, CCT_MEMPB_TRM %in% c(8, 9), NA),
         CCT_MGRN_TRM = replace(CCT_MGRN_TRM, CCT_MGRN_TRM %in% c(8, 9), NA),
         CCT_MOOD_TRM = replace(CCT_MOOD_TRM, CCT_MOOD_TRM %in% c(8, 9), NA),
         CCT_MS_TRM = replace(CCT_MS_TRM, CCT_MS_TRM %in% c(8, 9), NA),
         CCT_OAHAND_TRM = replace(CCT_OAHAND_TRM, CCT_OAHAND_TRM %in% c(8, 9), NA),
         CCT_OAHIP_TRM = replace(CCT_OAHIP_TRM, CCT_OAHIP_TRM %in% c(8, 9), NA),
         CCT_OAKNEE_TRM = replace(CCT_OAKNEE_TRM, CCT_OAKNEE_TRM %in% c(8, 9), NA),
         CCT_OSTPO_TRM = replace(CCT_OSTPO_TRM, CCT_OSTPO_TRM %in% c(8, 9), NA),
         CCT_OTART_TRM = replace(CCT_OTART_TRM, CCT_OTART_TRM %in% c(8, 9), NA),
         CCT_OTCCT_TRM = replace(CCT_OTCCT_TRM, CCT_OTCCT_TRM %in% c(8, 9), NA),
         CCT_OTHYR_TRM = replace(CCT_OTHYR_TRM, CCT_OTHYR_TRM %in% c(8, 9), NA),
         CCT_PARK_TRM = replace(CCT_PARK_TRM, CCT_PARK_TRM %in% c(8, 9), NA),
         CCT_PVD_TRM = replace(CCT_PVD_TRM, CCT_PVD_TRM %in% c(8, 9), NA),
         CCT_RA_TRM = replace(CCT_RA_TRM, CCT_RA_TRM %in% c(8, 9), NA),
         CCT_TIA_TRM = replace(CCT_TIA_TRM, CCT_TIA_TRM %in% c(8, 9), NA),
         CCT_ULCR_TRM = replace(CCT_ULCR_TRM, CCT_ULCR_TRM %in% c(8, 9), NA),
         CCT_URIINC_TRM = replace(CCT_URIINC_TRM, CCT_URIINC_TRM %in% c(8, 9), NA),
         CCT_UTHYR_TRM = replace(CCT_UTHYR_TRM, CCT_UTHYR_TRM %in% c(8, 9), NA),
         CR_TIME_COM = replace(CR_TIME_COM, CR_TIME_COM == -88, NA),
         CR1_FRHC = replace(CR1_FRHC, CR1_FRHC == 9, NA),
         CR2_FRHC = replace(CR2_FRHC, CR2_FRHC == 9, NA),
         DEP_CESD10 = replace(DEP_CESD10, DEP_CESD10 == -88, NA),
         DIA_AGE_NB_COM = replace(DIA_AGE_NB_COM, DIA_AGE_NB_COM == 9998, NA),
         DIA_DIAB_COM = replace(DIA_DIAB_COM, DIA_DIAB_COM == 8, NA),
         DIA_MEDAGE_NB_COM = replace(DIA_MEDAGE_NB_COM, DIA_MEDAGE_NB_COM %in% c(8, 9), NA),
         DIA_TYPE_COM = replace(DIA_TYPE_COM, DIA_TYPE_COM == 8, NA),
         DSU_CAL_MCQ = replace(DSU_CAL_MCQ,DSU_CAL_MCQ %in% c(8, 9), NA),
         DSU_IRON_MCQ = replace(DSU_IRON_MCQ, DSU_IRON_MCQ %in% c(8, 9), NA),
         DSU_MLTV_MCQ = replace(DSU_MLTV_MCQ, DSU_MLTV_MCQ %in% c(8, 9), NA),
         DSU_VITB12_MCQ = replace(DSU_VITB12_MCQ, DSU_VITB12_MCQ %in% c(8, 9), NA),
         DSU_VITC_MCQ = replace(DSU_VITC_MCQ, DSU_VITC_MCQ %in% c(8, 9), NA),
         DSU_VITD_MCQ = replace(DSU_VITD_MCQ, DSU_VITD_MCQ %in% c(8, 9), NA),
         ECG_STATUS_COM = replace(ECG_STATUS_COM, ECG_STATUS_COM == -8, NA),
         ED_UDR04 = replace(ED_UDR04, ED_UDR04 == 9, NA), 
         ED_UDR11 = replace(ED_UDR04, ED_UDR04 == 99, NA), 
         ENV_AFRDWLK_MCQ = replace(ENV_AFRDWLK_MCQ, ENV_AFRDWLK_MCQ %in% c(-8, 8, 9), NA),
         ENV_CLEAN_MCQ = replace(ENV_CLEAN_MCQ , ENV_CLEAN_MCQ %in% c(-8, 8, 9), NA),
         ENV_FLLNLY_MCQ = replace(ENV_FLLNLY_MCQ, ENV_FLLNLY_MCQ %in% c(-8, 8, 9), NA),
         ENV_FLPRTAREA_MCQ = replace(ENV_FLPRTAREA_MCQ, ENV_FLPRTAREA_MCQ %in% c(-8, 8, 9), NA),
         ENV_PPLFRNDLY_MCQ = replace(ENV_PPLFRNDLY_MCQ, ENV_PPLFRNDLY_MCQ %in% c(-8, 8, 9), NA),
         ENV_PPLHLP_MCQ = replace(ENV_PPLHLP_MCQ, ENV_PPLHLP_MCQ %in% c(-8, 8, 9), NA),
         ENV_PPLTKADV_MCQ = replace(ENV_PPLTKADV_MCQ, ENV_PPLTKADV_MCQ %in% c(-8, 8, 9), NA),
         ENV_PPLTRST_MCQ = replace(ENV_PPLTRST_MCQ, ENV_PPLTRST_MCQ %in% c(-8, 8, 9), NA),
         ENV_STFHM_MCQ = replace(ENV_STFHM_MCQ, ENV_STFHM_MCQ %in% c(-8, 8, 9), NA),
         ENV_VNDLSM_MCQ = replace(ENV_VNDLSM_MCQ, ENV_VNDLSM_MCQ %in% c(-8, 8, 9), NA),
         FAL_NMBR_NB = replace(FAL_NMBR_NB, FAL_NMBR_NB %in% c(98, 99), NA),
         FAL_12MN_MCQ = replace(FAL_12MN_MCQ, FAL_12MN_MCQ == 8, NA),
         GEN_DHDI = replace(GEN_DHDI, GEN_DHDI == 9, NA),
         GEN_DMHI = replace(GEN_DMHI, GEN_DMHI == 9, NA),
         GRLANYY_01  = replace(GRLANYY_01, GRLANYY_01 %in% c(8888, -9999), NA),
         GRLANYY_01_2011 = replace(GRLANYY_01_2011, GRLANYY_01_2011 %in% c(8888, -9999), NA),
         GRLANYY_01_2013 = replace(GRLANYY_01_2013, GRLANYY_01_2013 %in% c(8888, -9999), NA),
         GRLANYY_04 = replace(GRLANYY_04 , GRLANYY_04 %in% c(8888, -9999), NA),
         GRLANYY_04_2011 = replace(GRLANYY_04_2011, GRLANYY_04_2011 %in% c(8888, -9999), NA),
         GRLANYY_04_2013 = replace(GRLANYY_04_2013, GRLANYY_04_2013 %in% c(8888, -9999), NA),
         GRLANYY_07 = replace(GRLANYY_07, GRLANYY_07 %in% c(8888, -9999), NA),
         GRLANYY_07_2011 = replace(GRLANYY_07_2011, GRLANYY_07_2011 %in% c(8888, -9999), NA),
         GRLANYY_07_2013 = replace(GRLANYY_07_2013, GRLANYY_07_2013 %in% c(8888, -9999), NA),
         GRLANYY_08 = replace(GRLANYY_08, GRLANYY_08 %in% c(8888, -9999), NA),
         GRLANYY_08_2011 = replace(GRLANYY_08_2011, GRLANYY_08_2011 %in% c(8888, -9999), NA),
         GRLANYY_08_2013 = replace(GRLANYY_08_2013, GRLANYY_08_2013 %in% c(8888, -9999), NA),
         GRLANYY_09 = replace(GRLANYY_09, GRLANYY_09 %in% c(8888, -9999), NA),
         GRLANYY_09_2011 = replace(GRLANYY_09_2011, GRLANYY_09_2011 %in% c(8888, -9999), NA),
         GRLANYY_09_2013 = replace(GRLANYY_09_2013, GRLANYY_09_2013 %in% c(8888, -9999), NA),
         GRLANYY_10 = replace(GRLANYY_10, GRLANYY_10 %in% c(8888, -9999), NA),
         GRLANYY_10_2011 = replace(GRLANYY_10_2011, GRLANYY_10_2011 %in% c(8888, -9999), NA),
         GRLANYY_10_2013 = replace(GRLANYY_10_2013, GRLANYY_10_2013 %in% c(8888, -9999), NA),
         GRLANYY_12 = replace(GRLANYY_12, GRLANYY_12 %in% c(8888, -9999), NA),
         GRLANYY_12_2011 = replace(GRLANYY_12_2011, GRLANYY_12_2011 %in% c(8888, -9999), NA),
         GRLANYY_12_2013 = replace(GRLANYY_12_2013, GRLANYY_12_2013 %in% c(8888, -9999), NA),
         GRLANYY_13 = replace(GRLANYY_13, GRLANYY_13 %in% c(8888, -9999), NA),
         GRLANYY_13_2011 = replace(GRLANYY_13_2011, GRLANYY_13_2011 %in% c(8888, -9999), NA),
         GRLANYY_13_2013 = replace(GRLANYY_13_2013, GRLANYY_13_2013 %in% c(8888, -9999), NA),
         GRLANYY_14 = replace(GRLANYY_14, GRLANYY_14 %in% c(8888, -9999), NA),
         GRLANYY_14_2011 = replace(GRLANYY_14_2011, GRLANYY_14_2011 %in% c(8888, -9999), NA),
         GRLANYY_14_2013 = replace(GRLANYY_14_2013, GRLANYY_14_2013 %in% c(8888, -9999), NA),
         GRLANYY_16 = replace(GRLANYY_16, GRLANYY_16 %in% c(8888, -9999), NA),
         GRLANYY_16_2011 = replace(GRLANYY_16_2011, GRLANYY_16_2011 %in% c(8888, -9999), NA),
         GRLANYY_16_2013 = replace(GRLANYY_16_2013, GRLANYY_16_2013 %in% c(8888, -9999), NA),
         GRLANYY_17 = replace(GRLANYY_17, GRLANYY_17 %in% c(8888, -9999), NA),
         GRLANYY_17_2011 = replace(GRLANYY_17_2011, GRLANYY_17_2011 %in% c(8888, -9999), NA),
         GRLANYY_17_2013 = replace(GRLANYY_17_2013, GRLANYY_17_2013 %in% c(8888, -9999), NA),
         GRLANYY_18 = replace(GRLANYY_18, GRLANYY_18 %in% c(8888, -9999), NA),
         GRLANYY_18_2011 = replace(GRLANYY_18_2011, GRLANYY_18_2011 %in% c(8888, -9999), NA),
         GRLANYY_18_2013 = replace(GRLANYY_18_2013, GRLANYY_18_2013 %in% c(8888, -9999), NA),
         GRLANYY_19 = replace(GRLANYY_19, GRLANYY_19 %in% c(8888, -9999), NA),
         GRLANYY_19_2011 = replace(GRLANYY_19_2011, GRLANYY_19_2011 %in% c(8888, -9999), NA),
         GRLANYY_19_2013 = replace(GRLANYY_19_2013, GRLANYY_19_2013 %in% c(8888, -9999), NA),
         GRLANYY_21 = replace(GRLANYY_21, GRLANYY_21 %in% c(8888, -9999), NA),
         GRLANYY_21_2011 = replace(GRLANYY_21_2011, GRLANYY_21_2011 %in% c(8888, -9999), NA),
         GRLANYY_21_2013 = replace(GRLANYY_21_2013, GRLANYY_21_2013 %in% c(8888, -9999), NA),
         GRLANYY_22 = replace(GRLANYY_22, GRLANYY_22 %in% c(8888, -9999), NA),
         GRLANYY_22_2011 = replace(GRLANYY_22_2011, GRLANYY_22_2011 %in% c(8888, -9999), NA),
         GRLANYY_22_2013 = replace(GRLANYY_22_2013, GRLANYY_22_2013 %in% c(8888, -9999), NA),
         GRLANYY_23 = replace(GRLANYY_23, GRLANYY_23 %in% c(8888, -9999), NA),
         GRLANYY_23_2011 = replace(GRLANYY_23_2011, GRLANYY_23_2011 %in% c(8888, -9999), NA),
         GRLANYY_23_2013 = replace(GRLANYY_23_2013, GRLANYY_23_2013 %in% c(8888, -9999), NA),
         GRLANYY_25 = replace(GRLANYY_25, GRLANYY_25 %in% c(8888, -9999), NA),
         GRLANYY_25_2011 = replace(GRLANYY_25_2011, GRLANYY_25_2011 %in% c(8888, -9999), NA),
         GRLANYY_25_2013 = replace(GRLANYY_25_2013, GRLANYY_25_2013 %in% c(8888, -9999), NA),
         GRLANYY_26 = replace(GRLANYY_26, GRLANYY_26 %in% c(8888, -9999), NA),
         GRLANYY_26_2011 = replace(GRLANYY_26_2011, GRLANYY_26_2011 %in% c(8888, -9999), NA),
         GRLANYY_26_2013 = replace(GRLANYY_26_2013, GRLANYY_26_2013 %in% c(8888, -9999), NA),
         GRLANYY_27 = replace(GRLANYY_27, GRLANYY_27 %in% c(8888, -9999), NA),
         GRLANYY_27_2011 = replace(GRLANYY_27_2011, GRLANYY_27_2011 %in% c(8888, -9999), NA),
         GRLANYY_27_2013 = replace(GRLANYY_27_2013, GRLANYY_27_2013 %in% c(8888, -9999), NA),
         HBP_AGE_NB_COM = replace(HBP_AGE_NB_COM,HBP_AGE_NB_COM == 9998, NA),
         HBP_EVRMED_COM = replace(HBP_EVRMED_COM,HBP_EVRMED_COM %in% c(8, -8), NA),
         HBP_EVTRT_COM = replace(HBP_EVTRT_COM, HBP_EVTRT_COM == 8, NA),
         HBP_MED_COM   = replace(HBP_MED_COM, HBP_MED_COM %in% c(8, -8), NA),
         HBP_OTPRG_COM = replace(HBP_OTPRG_COM, HBP_OTPRG_COM == 8, NA),
         HBP_PRG_COM   = replace(HBP_PRG_COM, HBP_PRG_COM %in% c(8, -8), NA),
         HBP_TRT_COM   = replace(HBP_TRT_COM, HBP_TRT_COM == 8, NA),
         HCU_DEN_MCQ = replace(HCU_DEN_MCQ , HCU_DEN_MCQ  %in% c(8, 9), NA),
         HCU_EMEREG_MCQ = replace(HCU_EMEREG_MCQ, HCU_EMEREG_MCQ %in% c(8, 9), NA),
         HCU_FAMPHY_MCQ = replace(HCU_FAMPHY_MCQ, HCU_FAMPHY_MCQ %in% c(8, 9), NA),
         HCU_HLOVRNT_MCQ = replace(HCU_HLOVRNT_MCQ, HCU_HLOVRNT_MCQ %in% c(8, 9), NA),
         HCU_NRSHM_MCQ = replace(HCU_NRSHM_MCQ, HCU_NRSHM_MCQ %in% c(8, 9), NA),
         HCU_OPTO_MCQ = replace(HCU_OPTO_MCQ, HCU_OPTO_MCQ %in% c(8, 9), NA),
         HCU_PHYSIO_MCQ = replace(HCU_PHYSIO_MCQ, HCU_PHYSIO_MCQ %in% c(8, 9), NA),
         HCU_PSYCH_MCQ = replace(HCU_PSYCH_MCQ, HCU_PSYCH_MCQ %in% c(8, 9), NA),
         HCU_SOCLWRK_MCQ = replace(HCU_SOCLWRK_MCQ, HCU_SOCLWRK_MCQ %in% c(8, 9), NA),
         HCU_SPEC_MCQ = replace(HCU_SPEC_MCQ, HCU_SPEC_MCQ %in% c(8, 9), NA),
         HRG_EARSTATUS_L_COM = replace(HRG_EARSTATUS_L_COM ,HRG_EARSTATUS_L_COM == 7, NA),
         HRG_EARSTATUS_R_COM = replace(HRG_EARSTATUS_R_COM ,HRG_EARSTATUS_R_COM == 7, NA),
         HUP_FREE_MCQ  = replace(HUP_FREE_MCQ ,HUP_FREE_MCQ  %in% c(8,9), NA),
         HUP_INTNSTY_MCQ = replace(HUP_INTNSTY_MCQ, HUP_INTNSTY_MCQ %in% c(8,9), NA),
         HUP_PRVACT_MCQ = replace(HUP_PRVACT_MCQ, HUP_PRVACT_MCQ %in% c(8,9), NA),
         HWT_DBMI =  replace(HWT_DBMI, HWT_DBMI %in% c(999.96000, 999.99000), NA),
         HWT_WGHT_KG_TRM = replace(HWT_WGHT_KG_TRM, HWT_WGHT_KG_TRM %in% c(998, 999, 7777), NA),
         HYP_OTHYRAGE_NB_COM = replace(HYP_OTHYRAGE_NB_COM, HYP_OTHYRAGE_NB_COM == 9998, NA),
         HYP_OTHYREVRMD_COM = replace(HYP_OTHYREVRMD_COM, HYP_OTHYREVRMD_COM %in% c(8, -8), NA), 
         HYP_UTHYRAGE_NB_COM = replace(HYP_UTHYRAGE_NB_COM, HYP_UTHYRAGE_NB_COM %in% c(9998, 9999), NA),
         HYP_UTHYRMED_COM = replace(HYP_UTHYRMED_COM, HYP_UTHYRMED_COM %in% c(8, -8), NA), 
         ICQ_ABDTUBE_COM = replace(ICQ_ABDTUBE_COM , ICQ_ABDTUBE_COM %in% c(-8, 8), NA),
         ICQ_ABLESTND_COM = replace(ICQ_ABLESTND_COM , ICQ_ABLESTND_COM %in% c(-8, 8), NA),
         ICQ_ABLEWLK_COM = replace(ICQ_ABLEWLK_COM , ICQ_ABLEWLK_COM %in% c(-8, 8), NA),
         ICQ_ANEURY_COM = replace(ICQ_ANEURY_COM , ICQ_ANEURY_COM %in% c(-8, 8), NA),
         ICQ_BARSWAL_COM = replace(ICQ_BARSWAL_COM , ICQ_BARSWAL_COM %in% c(-8, 8), NA),
         ICQ_BLDSP3MO_COM = replace(ICQ_BLDSP3MO_COM , ICQ_BLDSP3MO_COM %in% c(-8, 8), NA),
         ICQ_BLDTR24H_COM = replace(ICQ_BLDTR24H_COM , ICQ_BLDTR24H_COM %in% c(-8, 8), NA),
         ICQ_CATRCT_COM = replace(ICQ_CATRCT_COM , ICQ_CATRCT_COM %in% c(-8, 8), NA),
         ICQ_CATRCT2_COM = replace(ICQ_CATRCT2_COM , ICQ_CATRCT2_COM %in% c(-8, 8), NA),
         ICQ_CHEMO4WK_COM = replace(ICQ_CHEMO4WK_COM , ICQ_CHEMO4WK_COM %in% c(-8, 8), NA),
         ICQ_COCHLIMP_COM = replace(ICQ_COCHLIMP_COM , ICQ_COCHLIMP_COM %in% c(-8, 8), NA),
         ICQ_CTLENS_COM = replace(ICQ_CTLENS_COM , ICQ_CTLENS_COM %in% c(-8,8), NA),
         ICQ_CTLENSWR_COM = replace(ICQ_CTLENSWR_COM , ICQ_CTLENSWR_COM %in% c(-8,8), NA),
         ICQ_DEFIBR_COM = replace(ICQ_DEFIBR_COM , ICQ_DEFIBR_COM %in% c(-8,8), NA),
         ICQ_DERET3MO_COM = replace(ICQ_DERET3MO_COM , ICQ_DERET3MO_COM %in% c(-8,8), NA),
         ICQ_EARINF_COM = replace(ICQ_EARINF_COM , ICQ_EARINF_COM %in% c(-8,8), NA),
         ICQ_EMB6WK_COM = replace(ICQ_EMB6WK_COM , ICQ_EMB6WK_COM %in% c(-8,8), NA),
         ICQ_EMBMED_COM = replace(ICQ_EMBMED_COM , ICQ_EMBMED_COM %in% c(-8,8), NA),
         ICQ_EYEINF_COM = replace(ICQ_EYEINF_COM , ICQ_EYEINF_COM %in% c(-8,8), NA),
         ICQ_FX_COM = replace(ICQ_FX_COM , ICQ_FX_COM %in% c(-8,8), NA),
         ICQ_FXANK_COM = replace(ICQ_FXANK_COM , ICQ_FXANK_COM == 9, NA),
         ICQ_FXARM_COM = replace(ICQ_FXARM_COM , ICQ_FXARM_COM == 9, NA),
         ICQ_FXBACK_COM = replace(ICQ_FXBACK_COM , ICQ_FXBACK_COM == 9, NA),
         ICQ_FXCHK_COM = replace(ICQ_FXCHK_COM , ICQ_FXCHK_COM == 9, NA),
         ICQ_FXCOLLR_COM = replace(ICQ_FXCOLLR_COM , ICQ_FXCOLLR_COM == 9, NA),
         ICQ_FXFT_COM = replace(ICQ_FXFT_COM , ICQ_FXFT_COM == 9, NA),
         ICQ_FXHIP_COM = replace(ICQ_FXHIP_COM , ICQ_FXHIP_COM == 9, NA),
         ICQ_FXHND_COM = replace(ICQ_FXHND_COM , ICQ_FXHND_COM == 9, NA),
         ICQ_FXJAW_COM = replace(ICQ_FXJAW_COM , ICQ_FXJAW_COM == 9, NA),
         ICQ_FXKNEE_COM = replace(ICQ_FXKNEE_COM , ICQ_FXKNEE_COM == 9, NA),
         ICQ_FXLEG_COM = replace(ICQ_FXLEG_COM , ICQ_FXLEG_COM == 9, NA),
         ICQ_FXNECK_COM = replace(ICQ_FXNECK_COM , ICQ_FXNECK_COM == 9, NA),
         ICQ_FXNOSE_COM = replace(ICQ_FXNOSE_COM , ICQ_FXNOSE_COM == 9, NA),
         ICQ_FXRIB_COM = replace(ICQ_FXRIB_COM , ICQ_FXRIB_COM == 9, NA),
         ICQ_FXSKL_COM = replace(ICQ_FXSKL_COM , ICQ_FXSKL_COM == 9, NA),
         ICQ_GLASSES_COM  = replace(ICQ_GLASSES_COM  ,ICQ_GLASSES_COM  %in%c(-8, 9),NA),
         ICQ_GLASSESWR_COM= replace(ICQ_GLASSESWR_COM,ICQ_GLASSESWR_COM%in%c(-8, 9),NA),
         ICQ_GLAUC_COM    = replace(ICQ_GLAUC_COM    ,ICQ_GLAUC_COM    %in%c(-8, 8),NA),
         ICQ_HAEMO_COM    = replace(ICQ_HAEMO_COM    ,ICQ_HAEMO_COM    %in%c(-8, 9),NA),
         ICQ_HRAID_COM    = replace(ICQ_HRAID_COM    ,ICQ_HRAID_COM    %in%c(-8, 9),NA),
         ICQ_HRTCOND_COM  = replace(ICQ_HRTCOND_COM  ,ICQ_HRTCOND_COM  %in%c(-8, 9),NA),
         ICQ_HRWORKING_COM= replace(ICQ_HRWORKING_COM,ICQ_HRWORKING_COM%in%c(-8, 9),NA),
         ICQ_ILLLUNG_COM  = replace(ICQ_ILLLUNG_COM  ,ICQ_ILLLUNG_COM  %in%c(-8, 9),NA),
         ICQ_LAMIN_COM    = replace(ICQ_LAMIN_COM    ,ICQ_LAMIN_COM    %in%c(-8, 9),NA),
         ICQ_NGTUBE_COM   = replace(ICQ_NGTUBE_COM   ,ICQ_NGTUBE_COM   %in%c(-8, 8),NA),
         ICQ_NUCL48H_COM  = replace(ICQ_NUCL48H_COM  ,ICQ_NUCL48H_COM  %in%c(-8, 9),NA),
         ICQ_NUCLIV24H_COM= replace(ICQ_NUCLIV24H_COM,ICQ_NUCLIV24H_COM%in%c(-8, 9),NA),
         ICQ_NUCLMED_COM  = replace(ICQ_NUCLMED_COM  ,ICQ_NUCLMED_COM  %in%c(-8, 9),NA),
         ICQ_PACEMKR_COM  = replace(ICQ_PACEMKR_COM ,ICQ_PACEMKR_COM  %in% c(-8, 8),NA),
         ICQ_POLIO_COM    = replace(ICQ_POLIO_COM   ,ICQ_POLIO_COM    %in% c(-8, 9),NA),
         ICQ_RISEASSI_COM = replace(ICQ_RISEASSI_COM,ICQ_RISEASSI_COM %in% c(-8, 9),NA),
         ICQ_RISECANE_COM = replace(ICQ_RISECANE_COM,ICQ_RISECANE_COM %in% c(-8, 9),NA),
         ICQ_TINNIT_COM   = replace(ICQ_TINNIT_COM  ,ICQ_TINNIT_COM   %in% c(-8, 9),NA),
         ICQ_SMOKE_COM =  replace(ICQ_SMOKE_COM,ICQ_SMOKE_COM == -8, NA),
         IHD_AMIAGE_NB_COM  = replace(IHD_AMIAGE_NB_COM ,IHD_AMIAGE_NB_COM  == 9998, NA),
         IHD_ANGIAGE_NB_COM = replace(IHD_ANGIAGE_NB_COM,IHD_ANGIAGE_NB_COM == 9998, NA),
         IHD_ANGIO_COM = replace(IHD_ANGIO_COM,IHD_ANGIO_COM %in% c(8,9), NA),
         IHD_CAB_COM   = replace(IHD_CAB_COM  ,IHD_CAB_COM   %in% c(8,9), NA),
         IHD_BLOCK_COM = replace(IHD_BLOCK_COM,IHD_BLOCK_COM == 8, NA),
         IHD_EVRMED_COM = replace(IHD_EVRMED_COM,IHD_EVRMED_COM %in% c(-88, 98),NA),
         IHD_MED_COM    = replace(IHD_MED_COM   ,IHD_MED_COM    %in% c(-8, 8),NA),
         INC_PTOT       = replace(INC_PTOT      ,INC_PTOT       %in% c(8,9),NA),
         INC_TOT        = replace(INC_TOT       ,INC_TOT        %in% c(8,9),NA),
         INJ_OCC        = replace(INJ_OCC       ,INJ_OCC        %in% c(7, 8, 9),NA),
         INT_ACCESSHM_MCQ = replace(INT_ACCESSHM_MCQ,INT_ACCESSHM_MCQ %in% c(-8, 8, 9),NA),
         INT_FRQEMAIL_MCQ = replace(INT_FRQEMAIL_MCQ,INT_FRQEMAIL_MCQ %in% c(-8, 8, 9),NA),
         INT_FRQFAM_MCQ   = replace(INT_FRQFAM_MCQ  ,INT_FRQFAM_MCQ   %in% c(-8, 8, 9),NA),
         INT_FRQFRI_MCQ   = replace(INT_FRQFRI_MCQ  ,INT_FRQFRI_MCQ   %in% c(-8, 8, 9),NA),
         INT_FRQHLTH_MCQ  = replace(INT_FRQHLTH_MCQ ,INT_FRQHLTH_MCQ  %in% c(-8, 8, 9),NA),
         INT_FRQMNF_MCQ   = replace(INT_FRQMNF_MCQ  ,INT_FRQMNF_MCQ   %in% c(-8, 8, 9),NA),
         INT_FRQPRO_MCQ   = replace(INT_FRQPRO_MCQ  ,INT_FRQPRO_MCQ   %in% c(-8, 8, 9),NA),
         INT_FRQWBSTS_MCQ = replace(INT_FRQWBSTS_MCQ,INT_FRQWBSTS_MCQ %in% c(-8, 8, 9),NA),
         INT_SCLNTWRK_MCQ = replace(INT_SCLNTWRK_MCQ,INT_SCLNTWRK_MCQ %in% c(-8, 8, 9),NA),
         K10_DSCORE_MCQ = replace(K10_DSCORE_MCQ ,K10_DSCORE_MCQ == 99, NA),
         LFP_HRWK       = replace(LFP_HRWK       ,LFP_HRWK       %in% c(7,8,9),NA),
         LFP_LAST_NB    = replace(LFP_LAST_NB    ,LFP_LAST_NB    %in% c(7777, 9996, 9998, 9999),NA),
         LFP_YRS        = replace(LFP_YRS        ,LFP_YRS        %in% c(7,8,9),NA),
         LGTNLTYY_01    = replace(LGTNLTYY_01    ,LGTNLTYY_01    == -8888, NA),
         MED_USE_MCQ    = replace(MED_USE_MCQ    ,MED_USE_MCQ    %in% c(-8, 8),NA),
         MED_USEQTY_MCQ = replace(MED_USEQTY_MCQ ,MED_USEQTY_MCQ %in% c(8,9),NA),
         MSDYY_DAPOP   = replace(MSDYY_DAPOP   ,MSDYY_DAPOP    == -8888, NA),
         MSDYY_MPPR    = replace(MSDYY_MPPR    ,MSDYY_MPPR     == -8888, NA),
         MSDYY_MQ5CMA  = replace(MSDYY_MQ5CMA  ,MSDYY_MQ5CMA   == -8888, NA),
         MSDYY_MQ5PR   = replace(MSDYY_MQ5PR   ,MSDYY_MQ5PR    == -8888, NA),
         MSDYY_MQ5REG  = replace(MSDYY_MQ5REG  ,MSDYY_MQ5REG   == -8888, NA),
         MSDYY_MQ5ZN   = replace(MSDYY_MQ5ZN   ,MSDYY_MQ5ZN    == -8888, NA),
         MSDYY_SPPR    = replace(MSDYY_SPPR    ,MSDYY_SPPR     == -8888, NA),
         MSDYY_SQ5CMA  = replace(MSDYY_SQ5CMA  ,MSDYY_SQ5CMA   == -8888, NA),
         MSDYY_SQ5PR   = replace(MSDYY_SQ5PR   ,MSDYY_SQ5PR    == -8888, NA),
         MSDYY_SQ5REG  = replace(MSDYY_SQ5REG  ,MSDYY_SQ5REG   == -8888, NA),
         MSDYY_SQ5ZN   = replace(MSDYY_SQ5ZN   ,MSDYY_SQ5ZN    == -8888, NA),
         no2_1yr       = replace(no2_1yr       ,no2_1yr        == -8888, NA),
         no2_1yr_cold  = replace(no2_1yr_cold  ,no2_1yr_cold   == -8888, NA),
         no2_1yr_warm  = replace(no2_1yr_warm  ,no2_1yr_warm   == -8888, NA),
         no2_5yrs      = replace(no2_5yrs      ,no2_5yrs       == -8888, NA),
         no2_5yrs_cold = replace(no2_5yrs_cold ,no2_5yrs_cold  == -8888, NA),
         no2_5yrs_warm = replace(no2_5yrs_warm ,no2_5yrs_warm  == -8888, NA),
         no2_7days_cold= replace(no2_7days_cold,no2_7days_cold == -8888, NA),
         no2_7days_warm= replace(no2_7days_warm,no2_7days_warm == -8888, NA),
         no2_exp       = replace(no2_exp       ,no2_exp        == -8888, NA),
         NO2F          = replace(NO2F          ,NO2F           == -8888, NA),
         NUT_BRD_COM     = replace(NUT_BRD_COM    ,NUT_BRD_COM     %in% c(98, 99),NA),
         NUT_BRD_NB_COM  = replace(NUT_BRD_NB_COM ,NUT_BRD_NB_COM  %in% c(9996, 9998, 9999),NA),
         NUT_BTTR_NB_COM = replace(NUT_BTTR_NB_COM,NUT_BTTR_NB_COM %in% c(7777, 9998, 9996),NA),
         NUT_CADR_NB_COM = replace(NUT_CADR_NB_COM,NUT_CADR_NB_COM %in% c(9996, 9998),NA),
         NUT_CAJC_COM    = replace(NUT_CAJC_COM   ,NUT_CAJC_COM    %in% c(98, 99),NA),
         NUT_CAJC_NB_COM = replace(NUT_CAJC_NB_COM,NUT_CAJC_NB_COM %in% c(9996, 9998, 9999) ,NA),
         NUT_CALC_NB_COM = replace(NUT_CALC_NB_COM,NUT_CALC_NB_COM %in% c(9996, 9998),NA),
         NUT_CAML_COM    = replace(NUT_CAML_COM   ,NUT_CAML_COM    %in% c(98, 99),NA),
         NUT_CAML_NB_COM = replace(NUT_CAML_NB_COM,NUT_CAML_NB_COM %in% c(9996, 9998, 9999),NA),
         NUT_CHCK_COM    = replace(NUT_CHCK_COM   ,NUT_CHCK_COM    %in% c(98, 99),NA),
         NUT_CHCK_NB_COM = replace(NUT_CHCK_NB_COM,NUT_CHCK_NB_COM %in% c(7777, 9996, 9998, 9999),NA),
         NUT_CHOC_COM    = replace(NUT_CHOC_COM   ,NUT_CHOC_COM    %in% c(98, 99),NA),
         NUT_CHOC_NB_COM = replace(NUT_CHOC_NB_COM,NUT_CHOC_NB_COM %in% c(7777, 9996, 9998, 9999),NA),
         NUT_CHSE_NB_COM = replace(NUT_CHSE_NB_COM,NUT_CHSE_NB_COM %in% c(9996, 9998),NA),
         NUT_CRRT_NB_COM = replace(NUT_CRRT_NB_COM,NUT_CRRT_NB_COM %in% c(9996, 9998),NA),
         NUT_DAIR_NB_COM = replace(NUT_DAIR_NB_COM,NUT_DAIR_NB_COM %in% c(9996, 9998),NA),
         NUT_DRSG_COM    = replace(NUT_DRSG_COM   ,NUT_DRSG_COM    %in% c(98, 99),NA),
         NUT_DRSG_NB_COM = replace(NUT_DRSG_NB_COM,NUT_DRSG_NB_COM %in% c(7777, 9996, 9998, 9999),NA),
         NUT_DSRT_COM    = replace(NUT_DSRT_COM   ,NUT_DSRT_COM    %in% c(98, 99),NA),
         NUT_CALC_COM = replace(NUT_CALC_COM,NUT_CALC_COM == 98, NA),
         NUT_CADR_COM = replace(NUT_CADR_COM,NUT_CADR_COM == 98, NA),
         NUT_CHSE_COM = replace(NUT_CHSE_COM,NUT_CHSE_COM == 98, NA),
         NUT_CRRT_COM = replace(NUT_CRRT_COM,NUT_CRRT_COM == 98, NA),
         NUT_DAIR_COM = replace(NUT_DAIR_COM,NUT_DAIR_COM == 98, NA),
         NUR_DHNR_MCQ = replace(NUR_DHNR_MCQ,NUR_DHNR_MCQ == 9, NA),
         NUT_BTTR_COM = replace(NUT_BTTR_COM,NUT_BTTR_COM == 98, NA),
         NUT_DSRT_NB_COM = replace(NUT_DSRT_NB_COM ,NUT_DSRT_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_EGGS_COM    = replace(NUT_EGGS_COM    ,NUT_EGGS_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_EGGS_NB_COM = replace(NUT_EGGS_NB_COM ,NUT_EGGS_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FBR_COM     = replace(NUT_FBR_COM     ,NUT_FBR_COM     %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FBR_NB_COM  = replace(NUT_FBR_NB_COM  ,NUT_FBR_NB_COM  %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FISH_COM    = replace(NUT_FISH_COM    ,NUT_FISH_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FISH_NB_COM = replace(NUT_FISH_NB_COM ,NUT_FISH_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FRIE_COM    = replace(NUT_FRIE_COM    ,NUT_FRIE_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FRIE_NB_COM = replace(NUT_FRIE_NB_COM ,NUT_FRIE_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FRUT_COM    = replace(NUT_FRUT_COM    ,NUT_FRUT_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_FRUT_NB_COM = replace(NUT_FRUT_NB_COM ,NUT_FRUT_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_GREEN_COM   = replace(NUT_GREEN_COM   ,NUT_GREEN_COM   %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_GREEN_NB_COM= replace(NUT_GREEN_NB_COM,NUT_GREEN_NB_COM%in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LEGM_COM    = replace(NUT_LEGM_COM    ,NUT_LEGM_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LEGM_NB_COM = replace(NUT_LEGM_NB_COM ,NUT_LEGM_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LFML_COM    = replace(NUT_LFML_COM    ,NUT_LFML_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LFML_NB_COM = replace(NUT_LFML_NB_COM ,NUT_LFML_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LWCS_COM    = replace(NUT_LWCS_COM    ,NUT_LWCS_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LWCS_NB_COM = replace(NUT_LWCS_NB_COM ,NUT_LWCS_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LWYG_COM    = replace(NUT_LWYG_COM    ,NUT_LWYG_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_LWYG_NB_COM = replace(NUT_LWYG_NB_COM ,NUT_LWYG_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_MEAT_COM    = replace(NUT_MEAT_COM    ,NUT_MEAT_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_MEAT_NB_COM = replace(NUT_MEAT_NB_COM ,NUT_MEAT_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_MTOT_COM    = replace(NUT_MTOT_COM    ,NUT_MTOT_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_MTOT_NB_COM = replace(NUT_MTOT_NB_COM ,NUT_MTOT_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_NUTS_COM    = replace(NUT_NUTS_COM    ,NUT_NUTS_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_NUTS_NB_COM = replace(NUT_NUTS_NB_COM ,NUT_NUTS_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_O3EG_COM    = replace(NUT_O3EG_COM    ,NUT_O3EG_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_O3EG_NB_COM = replace(NUT_O3EG_NB_COM ,NUT_O3EG_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_PATE_COM    = replace(NUT_PATE_COM    ,NUT_PATE_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_PATE_NB_COM = replace(NUT_PATE_NB_COM ,NUT_PATE_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_PTTO_COM    = replace(NUT_PTTO_COM    ,NUT_PTTO_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_PTTO_NB_COM = replace(NUT_PTTO_NB_COM ,NUT_PTTO_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_PURE_COM    = replace(NUT_PURE_COM    ,NUT_PURE_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_PURE_NB_COM = replace(NUT_PURE_NB_COM ,NUT_PURE_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_SALT_COM    = replace(NUT_SALT_COM    ,NUT_SALT_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_SALT_NB_COM = replace(NUT_SALT_NB_COM ,NUT_SALT_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_SASG_COM    = replace(NUT_SASG_COM    ,NUT_SASG_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_SASG_NB_COM = replace(NUT_SASG_NB_COM ,NUT_SASG_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_SAUC_COM    = replace(NUT_SAUC_COM    ,NUT_SAUC_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_SAUC_NB_COM = replace(NUT_SAUC_NB_COM ,NUT_SAUC_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_VGOT_COM    = replace(NUT_VGOT_COM    ,NUT_VGOT_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_VGOT_NB_COM = replace(NUT_VGOT_NB_COM ,NUT_VGOT_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_WHML_COM    = replace(NUT_WHML_COM    ,NUT_WHML_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_WHML_NB_COM = replace(NUT_WHML_NB_COM ,NUT_WHML_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_YOGR_COM    = replace(NUT_YOGR_COM    ,NUT_YOGR_COM    %in% c(97, 98, 9996, 9998, 9999), NA),
         NUT_YOGR_NB_COM = replace(NUT_YOGR_NB_COM ,NUT_YOGR_NB_COM %in% c(97, 98, 9996, 9998, 9999), NA),
         o3_1yr          = replace(o3_1yr          ,o3_1yr          == -8888, NA),
         o3_1yr_cold     = replace(o3_1yr_cold     ,o3_1yr_cold     == -8888, NA),
         o3_1yr_warm     = replace(o3_1yr_warm     ,o3_1yr_warm     == -8888, NA),
         o3_5yrs         = replace(o3_5yrs         ,o3_5yrs         == -8888, NA),
         o3_5yrs_cold    = replace(o3_5yrs_cold    ,o3_5yrs_cold    == -8888, NA),
         o3_5yrs_warm    = replace(o3_5yrs_warm    ,o3_5yrs_warm    == -8888, NA),
         o3_7days_cold   = replace(o3_7days_cold   ,o3_7days_cold   == -8888, NA),
         o3_7days_warm   = replace(o3_7days_warm   ,o3_7days_warm   == -8888, NA),
         o3_exp          = replace(o3_exp          ,o3_exp          == -8888, NA),
         o38h_1yr        = replace(o38h_1yr        ,o38h_1yr        == -8888, NA),
         o38h_1yr_cold   = replace(o38h_1yr_cold   ,o38h_1yr_cold   == -8888, NA),
         o38h_1yr_warm   = replace(o38h_1yr_warm   ,o38h_1yr_warm   == -8888, NA),
         o38h_5yrs       = replace(o38h_5yrs       ,o38h_5yrs       == -8888, NA),
         o38h_5yrs_cold  = replace(o38h_5yrs_cold  ,o38h_5yrs_cold  == -8888, NA),
         o38h_5yrs_warm  = replace(o38h_5yrs_warm  ,o38h_5yrs_warm  == -8888, NA),
         o38h_7days_cold = replace(o38h_7days_cold ,o38h_7days_cold == -8888, NA),
         o38h_7days_warm = replace(o38h_7days_warm ,o38h_7days_warm == -8888, NA),
         o38h_exp        = replace(o38h_exp        ,o38h_exp        == -8888, NA),
         O3CHGYY_01      = replace(O3CHGYY_01      ,O3CHGYY_01      == -8888, NA),
         ORH_AVDEAT_MCQ   = replace(ORH_AVDEAT_MCQ  ,ORH_AVDEAT_MCQ   %in%c(8,9), NA),
         ORH_BRUSH_NB_MCQ = replace(ORH_BRUSH_NB_MCQ,ORH_BRUSH_NB_MCQ %in%c(998, 999), NA),
         ORH_DENT_MCQ  = replace(ORH_DENT_MCQ  ,ORH_DENT_MCQ   %in% c(8,9), NA),
         ORH_HLTH_MCQ  = replace(ORH_HLTH_MCQ  ,ORH_HLTH_MCQ   %in% c(8,9), NA),
         ORH_TEETH_MCQ = replace(ORH_TEETH_MCQ ,ORH_TEETH_MCQ  %in% c(8,9), NA),
         ORH_UNCEAT_MCQ= replace(ORH_UNCEAT_MCQ,ORH_UNCEAT_MCQ %in% c(8,9), NA),
         OWN_OWN  = replace(OWN_OWN ,OWN_OWN  %in% c(97, 98, 99),NA),
         OWN_DWLG = replace(OWN_DWLG,OWN_DWLG %in% c(77, 97, 98),NA),
         OWN_MRTG = replace(OWN_MRTG,OWN_MRTG %in% c(98,99),NA),
         PA2_DSCR2_MCQ = replace(PA2_DSCR2_MCQ,PA2_DSCR2_MCQ == -8888, NA),
         OWN_FTEN      = replace(OWN_FTEN     ,OWN_FTEN      == 9, NA),
         PA2_EXER_MCQ   = replace(PA2_EXER_MCQ   ,PA2_EXER_MCQ    %in% c(8,9), NA),
         PA2_EXERHR_MCQ = replace(PA2_EXERHR_MCQ ,PA2_EXERHR_MCQ  %in% c(8,9), NA),
         PA2_LSPRT_MCQ  = replace(PA2_LSPRT_MCQ  ,PA2_LSPRT_MCQ   %in% c(8,9), NA),
         PA2_LSPRTHR_MCQ= replace(PA2_LSPRTHR_MCQ,PA2_LSPRTHR_MCQ %in% c(8,9), NA),
         PA2_MSPRT_MCQ  = replace(PA2_MSPRT_MCQ  ,PA2_MSPRT_MCQ   %in% c(8,9), NA),
         PA2_MSPRTHR_MCQ= replace(PA2_MSPRTHR_MCQ,PA2_MSPRTHR_MCQ %in% c(8,9), NA),
         PA2_SIT_MCQ    = replace(PA2_SIT_MCQ    ,PA2_SIT_MCQ     %in% c(8,9), NA),
         PA2_SSPRT_MCQ  = replace(PA2_SSPRT_MCQ  ,PA2_SSPRT_MCQ   %in% c(8,9), NA),
         PA2_SSPRTHR_MCQ= replace(PA2_SSPRTHR_MCQ,PA2_SSPRTHR_MCQ %in% c(8,9), NA),
         PA2_WALK_MCQ   = replace(PA2_WALK_MCQ   ,PA2_WALK_MCQ    %in% c(8,9), NA),
         PA2_WALKHR_MCQ = replace(PA2_WALKHR_MCQ ,PA2_WALKHR_MCQ  %in% c(8,9), NA), 
         PER_DSCR_AGR_MCQ  = replace(PER_DSCR_AGR_MCQ  ,PER_DSCR_AGR_MCQ  == 99, NA),
         PER_DSCR_CON_MCQ  = replace(PER_DSCR_CON_MCQ  ,PER_DSCR_CON_MCQ  == 99, NA),
         PER_DSCR_EMOS_MCQ = replace(PER_DSCR_EMOS_MCQ ,PER_DSCR_EMOS_MCQ == 99, NA),
         PER_DSCR_EXT_MCQ  = replace(PER_DSCR_EXT_MCQ  ,PER_DSCR_EXT_MCQ  == 99, NA),
         PER_DSCR_OPEX_MCQ = replace(PER_DSCR_OPEX_MCQ ,PER_DSCR_OPEX_MCQ == 99, NA),
         pm25_1yr        = replace(pm25_1yr        ,pm25_1yr        == -8888, NA),
         pm25_1yr_cold   = replace(pm25_1yr_cold   ,pm25_1yr_cold   == -8888, NA),
         pm25_1yr_warm   = replace(pm25_1yr_warm   ,pm25_1yr_warm   == -8888, NA),
         pm25_5yrs       = replace(pm25_5yrs       ,pm25_5yrs       == -8888, NA),
         pm25_5yrs_cold  = replace(pm25_5yrs_cold  ,pm25_5yrs_cold  == -8888, NA),
         pm25_5yrs_warm  = replace(pm25_5yrs_warm  ,pm25_5yrs_warm  == -8888, NA),
         pm25_7days_cold = replace(pm25_7days_cold ,pm25_7days_cold == -8888, NA),
         pm25_7days_warm = replace(pm25_7days_warm ,pm25_7days_warm == -8888, NA),
         pm25_exp        = replace(pm25_exp        ,pm25_exp        == -8888, NA),
         PM25DALYY_01 = replace(PM25DALYY_01,PM25DALYY_01 %in% c(-9999, -8888), NA),
         PM25DALYY_02 = replace(PM25DALYY_02,PM25DALYY_02 == -8888, NA),
         RET_RTRD      = replace(RET_RTRD     ,RET_RTRD      %in% c(8,9), NA),
         RET_RTRN     = replace(RET_RTRN    ,RET_RTRN     %in% c(8,9), NA),
         RET_SPSE     = replace(RET_SPSE    ,RET_SPSE     %in% c(7,8,9), NA),
         RET_AGE_NB   = replace(RET_AGE_NB  ,RET_AGE_NB   %in% c(98, 99), NA),
         PSD_DCTOFF = replace(PSD_DCTOFF,PSD_DCTOFF == 9, NA),
         rh_1yr       = replace(rh_1yr       ,rh_1yr        == -8888, NA),
         rh_1yr_cold  = replace(rh_1yr_cold  ,rh_1yr_cold   == -8888, NA),
         rh_1yr_warm  = replace(rh_1yr_warm  ,rh_1yr_warm   == -8888, NA),
         rh_5yrs      = replace(rh_5yrs      ,rh_5yrs       == -8888, NA),
         rh_5yrs_cold = replace(rh_5yrs_cold ,rh_5yrs_cold  == -8888, NA),
         rh_5yrs_warm = replace(rh_5yrs_warm ,rh_5yrs_warm  == -8888, NA),
         rh_7days_cold= replace(rh_7days_cold,rh_7days_cold == -8888, NA),
         rh_7days_warm= replace(rh_7days_warm,rh_7days_warm == -8888, NA),
         rh_exp       = replace(rh_exp       ,rh_exp        == -8888, NA),
         ROS_HILL_COM      = replace(ROS_HILL_COM      ,ROS_HILL_COM      %in% c(6,8),NA),
         ROS_PAIN_COM      = replace(ROS_PAIN_COM      ,ROS_PAIN_COM      %in% c(8,9),NA),
         SDC_ORTN          = replace(SDC_ORTN          ,SDC_ORTN          %in% c(8,9),NA),
         SDC_URBAN_RURAL   = replace(SDC_URBAN_RURAL   ,SDC_URBAN_RURAL   %in% c(6,9),NA),
         SEQ_LADDER_MCQ    = replace(SEQ_LADDER_MCQ    ,SEQ_LADDER_MCQ    %in% c(77, 98, 99),NA),
         SLE_30MIN_COM     = replace(SLE_30MIN_COM     ,SLE_30MIN_COM     %in% c(-8,8),NA),
         SLE_DREAM_COM     = replace(SLE_DREAM_COM     ,SLE_DREAM_COM     %in% c(-8, 8, 9),NA),
         SLE_HOUR_NB_COM   = replace(SLE_HOUR_NB_COM   ,SLE_HOUR_NB_COM   %in% c(-88, 98),NA),
         SLE_LEGS_COM      = replace(SLE_LEGS_COM      ,SLE_LEGS_COM      %in% c(-8, 8, 9),NA),
         SLE_LGDUR_NB_COM  = replace(SLE_LGDUR_NB_COM  ,SLE_LGDUR_NB_COM  %in% c(98, 99),NA),
         SLE_LGEVE_COM     = replace(SLE_LGEVE_COM     ,SLE_LGEVE_COM     %in% c(8,9),NA),
         SLE_LGFQ_COM      = replace(SLE_LGFQ_COM      ,SLE_LGFQ_COM      %in% c(8,9),NA),
         SLE_LGIMPR_COM    = replace(SLE_LGIMPR_COM    ,SLE_LGIMPR_COM    %in% c(8,9),NA),
         SLE_LGURG_COM     = replace(SLE_LGURG_COM     ,SLE_LGURG_COM     %in% c(-8, 8, 9),NA),
         SLE_MIDDUR_NB_COM = replace(SLE_MIDDUR_NB_COM ,SLE_MIDDUR_NB_COM %in% c(98, 99),NA),
         SLE_MIDFQ_COM     = replace(SLE_MIDFQ_COM     ,SLE_MIDFQ_COM     %in% c(-8,8),NA),
         SLE_QLTY_COM      = replace(SLE_QLTY_COM      ,SLE_QLTY_COM      %in% c(-8, 8, 9),NA),
         SLE_STAYFQ_COM    = replace(SLE_STAYFQ_COM    ,SLE_STAYFQ_COM    %in% c(-8, 8, 9),NA),
         SMK_CGDL_AG       = replace(SMK_CGDL_AG       ,SMK_CGDL_AG       %in% c(98, 777),NA),
         SMK_CURRCG        = replace(SMK_CURRCG        ,SMK_CURRCG        %in% c(7,8,9),NA),
         SMK_DYCS          = replace(SMK_DYCS          ,SMK_DYCS          %in% c(996, 999),NA),
         SMK_FRSTCG_AG     = replace(SMK_FRSTCG_AG     ,SMK_FRSTCG_AG     %in% c(98, 99),NA),
         SNO_SNORE_MCQ     = replace(SNO_SNORE_MCQ     ,SNO_SNORE_MCQ     %in% c(8,9),NA),
         SNO_STOPBREATH_MCQ= replace(SNO_STOPBREATH_MCQ,SNO_STOPBREATH_MCQ%in% c(8,9),NA),
         SMK_100CG         = replace(SMK_100CG         ,SMK_100CG          == 8, NA),
         SLE_STAYDUR_NB_COM= replace(SLE_STAYDUR_NB_COM,SLE_STAYDUR_NB_COM == 98, NA),
         SLE_MIDINTRF_COM  = replace(SLE_MIDINTRF_COM  ,SLE_MIDINTRF_COM   == 8, NA),
         SMK_DSTY          = replace(SMK_DSTY          ,SMK_DSTY           == 99, NA),
         SMK_WHLCG         = replace(SMK_WHLCG         ,SMK_WHLCG          == 8, NA),
         SLS_DSCR          = replace(SLS_DSCR          ,SLS_DSCR           == 99, NA),
         SLE_STAYINTRF_COM = replace(SLE_STAYINTRF_COM ,SLE_STAYINTRF_COM  == 8, NA),
         SDC_MRTL          = replace(SDC_MRTL          ,SDC_MRTL           == 9, NA),
         SLE_30INTRF_COM   = replace(SLE_30INTRF_COM   ,SLE_30INTRF_COM    == 8, NA),
         so2_1yr       = replace(so2_1yr       ,so2_1yr        == -8888, NA),
         so2_1yr_cold  = replace(so2_1yr_cold  ,so2_1yr_cold   == -8888, NA),
         so2_1yr_warm  = replace(so2_1yr_warm  ,so2_1yr_warm   == -8888, NA),
         so2_5yrs      = replace(so2_5yrs      ,so2_5yrs       == -8888, NA),
         so2_5yrs_cold = replace(so2_5yrs_cold ,so2_5yrs_cold  == -8888, NA),
         so2_5yrs_warm = replace(so2_5yrs_warm ,so2_5yrs_warm  == -8888, NA),
         so2_7days_cold= replace(so2_7days_cold,so2_7days_cold == -8888, NA),
         so2_7days_warm= replace(so2_7days_warm,so2_7days_warm == -8888, NA),
         so2_exp       = replace(so2_exp       ,so2_exp        == -8888, NA),
         SO2OMIYY      = replace(SO2OMIYY      ,SO2OMIYY       == -8888, NA),
         SPA_DFRE  = replace(SPA_DFRE ,SPA_DFRE == 9, NA),
         SPA_FPAR  = replace(SPA_FPAR ,SPA_FPAR == 9, NA),
         SSA_DPALL = replace(SSA_DPALL,SSA_DPALL== 999.99, NA),
         STP_COLTIME_SS_COM =replace(STP_COLTIME_SS_COM ,STP_COLTIME_SS_COM == -888, NA),
         STP_DOTTIME_SS_COM =replace(STP_DOTTIME_SS_COM ,STP_DOTTIME_SS_COM == -888, NA),
         STP_WORTIME_SS_COM =replace(STP_WORTIME_SS_COM ,STP_WORTIME_SS_COM == -888, NA),
         STR_CVAAGE_NB_COM = replace(STR_CVAAGE_NB_COM,STR_CVAAGE_NB_COM == 9998, NA),
         TBI_NMBR_NB_COM   = replace(TBI_NMBR_NB_COM  ,TBI_NMBR_NB_COM   == 98, NA),
         temp_1yr       = replace(temp_1yr       ,temp_1yr        == -8888, NA),
         temp_1yr_cold  = replace(temp_1yr_cold  ,temp_1yr_cold   == -8888, NA),
         temp_1yr_warm  = replace(temp_1yr_warm  ,temp_1yr_warm   == -8888, NA),
         temp_5yrs      = replace(temp_5yrs      ,temp_5yrs       == -8888, NA),
         temp_5yrs_cold = replace(temp_5yrs_cold ,temp_5yrs_cold  == -8888, NA),
         temp_5yrs_warm = replace(temp_5yrs_warm ,temp_5yrs_warm  == -8888, NA),
         temp_7days_cold= replace(temp_7days_cold,temp_7days_cold == -8888, NA),
         temp_7days_warm= replace(temp_7days_warm,temp_7days_warm == -8888, NA),
         temp_exp       = replace(temp_exp       ,temp_exp        == -8888, NA),
         TRA_DSTATUS_MCQ = replace(TRA_DSTATUS_MCQ ,TRA_DSTATUS_MCQ %in% c(-8, 8), NA),
         VET_OCC         = replace(VET_OCC         ,VET_OCC         %in% c(8,9), NA),
         VIS_AID         = replace(VIS_AID         ,VIS_AID         %in% c(-8, 7, 8), NA),
         VIS_SGHT        = replace(VIS_SGHT        ,VIS_SGHT        %in% c(-8, 8), NA),
         WEA_LFINS_MCQ   = replace(WEA_LFINS_MCQ   ,WEA_LFINS_MCQ   %in% c(8,9), NA),
         WEA_SVNGSVL_MCQ = replace(WEA_SVNGSVL_MCQ ,WEA_SVNGSVL_MCQ %in% c(8,9), NA),
         WHO_HRT         = replace(WHO_HRT         ,WHO_HRT         %in% c(-8, 7, 8), NA),
         WHO_MENOP       = replace(WHO_MENOP       ,WHO_MENOP       %in% c(-8, 7, 8), NA),
         WHO_MPAG_AG     = replace(WHO_MPAG_AG     ,WHO_MPAG_AG     %in% c(98, 99, 777), NA),
         WHO_HRTYR_YR           = replace(WHO_HRTYR_YR           ,WHO_HRTYR_YR           == 98, NA),
         VA_ETDRS_BOTH_RSLT_COM = replace(VA_ETDRS_BOTH_RSLT_COM ,VA_ETDRS_BOTH_RSLT_COM == -88.8, NA),
         WHO_TYPE               = replace(WHO_TYPE               ,WHO_TYPE               == 8, NA),
         WHO_HRTAG_AG           = replace(WHO_HRTAG_AG           ,WHO_HRTAG_AG           == 98, NA),
         WLK_TIME_COM           = replace(WLK_TIME_COM           ,WLK_TIME_COM           == -88, NA),
         WTHNRCYY_08 = replace(WTHNRCYY_08,WTHNRCYY_08 == -8888, NA),
         WTHNRCYY_13 = replace(WTHNRCYY_13,WTHNRCYY_13 == -8888, NA)
)

columns_to_remove <- c("CAO_MEDCUR_COM", "CCT_ALLRG_OTSP_TRM", "CCT_CANTP_BN_SP_TRM", "CCT_CANTP_CNS_SP_TRM", 
 "CCT_CANTP_DG_SP_TRM", "CCT_CANTP_FGO_SP_TRM", "CCT_CANTP_LHT_SP_TRM", "CCT_CANTP_MGO_SP_TRM",
 'CCT_CANTP_OR_SP_TRM', "CCT_CANTP_OTSP_TRM", "CCT_CANTP_REFUSED_TRM", "CCT_CANTP_RS_SP_TRM",
 "CCT_CANTP_UN_SP_TRM", "CCT_CANTP_UR_SP_TRM", "CCT_DROT_OTSP_TRM", "CCT_OTCCT_OTSP_TRM", "CD_EXCL_COM",
 "CHR_EXCL_COM", "CHR_PROS_COM", "DIS_STATUS_COM", "ECG_DIAGNOSIS_DETAILS_COM",
 "FAL_CJDSC_OTSP_TRM", "FAL_CJDSC_TRM", "FAL_CLDSC_COM", "FAL_CLDSC_OTSP_COM", "FAL_DCBP", 
 "FAL_DSTA", "FAL_DV_AL_COM", "FAL_DV_BR_COM", "FAL_DV_BT_COM", "FAL_DV_CN_COM", "FAL_DV_GR_COM", 
 "FAL_DV_HD_COM", "FAL_DV_LG_COM", "FAL_DV_LT_COM", "FAL_DV_OT_COM", "FAL_DV_SC_COM", "FAL_DV_UT_COM", 
 "FAL_DV_WC_COM", "FAL_DV_WK_COM", "FAL_DVCTR_AL_COM", 'FAL_DVCTR_BR_COM', "FAL_DVCTR_BT_COM", 
 "FAL_DVCTR_CN_COM", "FAL_DVCTR_GR_COM", "FAL_DVCTR_HD_COM", "FAL_DVCTR_LG_COM", "FAL_DVCTR_LT_COM", 
 "FAL_DVCTR_OT_COM", "FAL_DVCTR_SC_COM", "FAL_DVCTR_UT_COM", "FAL_DVCTR_WC_COM", "FAL_DVCTR_WK_COM", 
 "FAL_DVDSC_OTSP", "FAL_SPDSC_OTSP_TRM", "FAL_SPDSC_TRM", "FAL_SPRTDSC_COM", "FAL_SPRTDSC_OTSP_COM", 
 "FAL_YRDDSC_OT_COM", "FAL_YRDDSC_OTSP_TRM", "GRP_EXCL_COM", "GRP_EXCLLFT_COM", "GRP_EXCLRGT_COM",
 "HBP_MEDCUR_COM", "HWT_CNWGHT_TRM", "HWT_DHT_M_TRM","HWT_DWT_K_TRM","HWT_HGHT_TRM","HWT_HGHT1_TRM",
 "HWT_HGHT2_TRM","HWT_HGHT3_TRM","HWT_HGHT4_TRM","HWT_HGHT5_TRM","HWT_HGHT6_TRM","HWT_PREGN_TRM","ICQ_NUCLTEST_SP_COM",
 "ICQ_PROSARM_COM","ICQ_PROSFT_COM","ICQ_PROSHIP_COM","ICQ_PROSHND_COM","ICQ_PROSKNEE_COM",
 "ICQ_PROSLEG_COM","ICQ_PROSLIM_COM","IHD_MEDCUR_COM", "INC_FRST", "INC_PFRST", "INC_PSCND", "INC_PTHRD", "INC_SCND", 
 "INC_THRD", "INC_PSRCE_GV", "INC_PSRCE_OLD", "INC_PSRCE_WG", "INC_SRCE_GV", "INC_SRCE_OLD", "INC_SRCE_WG", 
 "INT_WYSSCL_OTSP01_MCQ", "INT_WYSSCL_OTSP02_MCQ", "INT_WYSSCL_OTSP03_MCQ", "INT_WYSSCL_OTSP04_MCQ", 
 "INT_WYSSCL_OTSP05_MCQ", "LBF_NVR_DK_NA", "LBF_UNEMDUR_YR", "MSDYY_CSD", "MSDYY_CMA", "MSDYY_DIS_DA",
 "NEUR_CONSREC_COM", "NPB_STATUS_COM", "OWN_OWN_OTSP", "OWN_DWLG_OTSP", "PMT_ACR_COM",
 "RET_BCKWRK_CD_TRM","RET_BCKWRK_DK_NA_TRM", "RET_BCKWRK_FC_TRM", "RET_BCKWRK_GR_TRM", "RET_BCKWRK_HF_TRM", 
 "RET_BCKWRK_IH_TRM", "RET_BCKWRK_JB_TRM", "RET_BCKWRK_LP_TRM", "RET_BCKWRK_LW_TRM", "RET_BCKWRK_MC_TRM", 
 "RET_BCKWRK_NA_TRM", "RET_BCKWRK_NL_TRM", "RET_BCKWRK_OT_TRM", "RET_BCKWRK_OTSP_TRM", "RET_BCKWRK_PB_TRM", 
 "RET_BCKWRK_REFUSED_TRM", "RET_BCKWRK_SI_TRM", "RET_BCKWRK_SR_TRM", "RET_BCKWRK_WC_TRM", "RET_BCKWRK_WO_TRM", 
 "RET_DUEHLTH_TRM", "RET_FOPTIME_TRM", "RET_PENSPL_TRM", "RET_POCWORK_TRM", "RET_PREP_BP_TRM", "RET_PREP_CJ_TRM", 
 "RET_PREP_DH_TRM", "RET_PREP_DK_NA_TRM", "RET_PREP_DLA_TRM", "RET_PREP_ED_TRM", "RET_PREP_FP_TRM", "RET_PREP_HR_TRM", 
 "RET_PREP_IH_TRM", "RET_PREP_ILA_TRM", "RET_PREP_INV_TRM", "RET_PREP_IPA_TRM", "RET_PREP_NONE_TRM", "RET_PREP_OT_TRM", 
 "RET_PREP_OTSP_TRM", "RET_PREP_POM_TRM", "RET_PREP_REFUSED_TRM", "RET_PREP_RET_TRM", "RET_PREP_RL_TRM", "RET_PREP_RP_TRM", 
 "RET_PREP_RSP_TRM", "RET_PREP_SB_TRM", "RET_PREP_SP_TRM", "RET_STDLIV_TRM", "RET_VOLUN_TRM", "RET_WKSAME_TRM", "SDC_DRES", 
 "SDC_FIMM", "SDC_GCB", "SDC_YACA_YR", "SDC_COB_OTSP", "SDC_LGMST_OTSP_TRM", "SDC_FTLG_OTSP_TRM", 
 "SDC_FTLG_OTSP01_COM", "SDC_FTLG_OTSP02_COM", "SDC_DCGT", "SDC_DFL1", "SDC_DLNG", "SDC_ETHN_OTSP04_COM", 
 "SDC_RELG_OTSP", "SDC_RELG", "SDC_COB" , "SDC_DAIM", "startdate", "startdate_MCQ", "startlanguage", 
 "startlanguage_MCQ", "STB_EARINF_COM", "STB_EXCL_COM", "STB_PROS_COM", "STDHT_EXCL_COM", "STR_EVRMD_COM", 
 "STR_MED_COM" , "STR_MEDCUR_COM", "STR_NOEXP_COM" , "STR_NOEXP_DUR_COM", "STR_NOUND_COM", 
 "STR_NOUND_DUR_COM", "STR_NOVIS_COM", "STR_NOVIS_DUR_COM", "STR_NUMB_COM", "STR_NUMB_DUR_COM", 
 "STR_OTHMD_COM", "STR_TIAAGE_NB_COM", "STR_TIAEVMD_COM", "STR_TIAMED_COM", "STR_TIAMEDCUR_COM", 
 "STR_TIAOTHMD_COM", "STR_VIS_COM", "STR_VIS_DUR_COM", "STR_WEAK_COM", "STR_WEAK_DUR_COM", 
 "TUG_DEVICE_COM", "TUG_DEVICE_SP_COM", "yeari", "WGT_PREGNT_COM", "WGT_STATUS_COM", "WGT_WEIGHT1_KG_COM", 
 "WGT_WEIGHT2_KG_COM")

CLSA.baseline.v1.4 <- CLSA.baseline.clean %>%
    select(-all_of(columns_to_remove))

write.csv(CLSA.baseline.v1.4, "CLSA.baseline.6.clean.csv") # 51338 obs for 680 vars
CLSA.baseline.v1.4<-read.csv("CLSA.baseline.6.clean.csv")

#### 7.0 Follow-Up 2 dataset - select DVs ####


# 7.1 Read in DVs from follow-up 2 (FU_2) assessment ####
CLSA.Com.FU2 <-read_csv("2304007_UOttawa_MBoisgontier_FUP2_CoPv1-1.CSV")
CLSA.Tra.FU2 <-read_csv("2304007_UOttawa_MBoisgontier_FUP2_Trav1-1.CSV")
CLSA.FU2 <- merge(CLSA.Com.FU2, CLSA.Tra.FU2, all=TRUE) 
write.csv(CLSA.FU2, "CLSA.FU.v1.csv")

# 7.2 Select DVs (ADL/IADL) and ID ####
CLSA.FU.select <- CLSA.FU2 %>%
  select(entity_id, ADL_ABLDR_COF2, ADL_HPDR_COF2, ADL_UNDR_COF2, ADL_ABLFD_COF2, 
         ADL_HPFD_COF2, ADL_UNFD_COF2, ADL_ABLAP_COF2, ADL_HPAP_COF2, ADL_UNAP_COF2,
         ADL_ABLWK_COF2, ADL_HPWK_COF2, ADL_UNWK_COF2, ADL_ABLBD_COF2, ADL_HPBD_COF2,
         ADL_UNBD_COF2, ADL_ABLBT_COF2, ADL_HPBT_COF2, ADL_UNBT_COF2, ADL_BATH_COF2,
         ADL_INCNT_COF2, IAL_ABLTEL_COF2, IAL_HPTEL_COF2, IAL_UNTEL_COF2, 
         IAL_ABLTRV_COF2, IAL_HPTRV_COF2, IAL_UNTRV_COF2, IAL_ABLGRO_COF2, 
         IAL_HPGRO_COF2, IAL_UNGRO_COF2, IAL_ABLML_COF2, IAL_HPML_COF2, IAL_UNML_COF2,
         IAL_ABLWRK_COF2, IAL_HPWRK_COF2, IAL_UNWRK_COF2, IAL_ABLMED_COF2, 
         IAL_HPMED_COF2, IAL_UNMED_COF2, IAL_ABLMO_COF2, IAL_HPMO_COF2, IAL_UNMO_COF2,
         ADL_ABLDR_TRF2, ADL_HPDR_TRF2, ADL_UNDR_TRF2, ADL_ABLFD_TRF2, ADL_HPFD_TRF2, 
         ADL_UNFD_TRF2, ADL_ABLAP_TRF2, ADL_HPAP_TRF2, ADL_UNAP_TRF2, ADL_ABLWK_TRF2, 
         ADL_HPWK_TRF2, ADL_UNWK_TRF2, ADL_ABLBD_TRF2, ADL_HPBD_TRF2, ADL_UNBD_TRF2, 
         ADL_ABLBT_TRF2, ADL_HPBT_TRF2, ADL_UNBT_TRF2, ADL_BATH_TRF2, ADL_INCNT_TRF2, 
         IAL_ABLTEL_TRF2, IAL_HPTEL_TRF2, IAL_UNTEL_TRF2, IAL_ABLTRV_TRF2, 
         IAL_HPTRV_TRF2, IAL_UNTRV_TRF2, IAL_ABLGRO_TRF2, IAL_HPGRO_TRF2, 
         IAL_UNGRO_TRF2, IAL_ABLML_TRF2, IAL_HPML_TRF2, IAL_UNML_TRF2, IAL_ABLWRK_TRF2, 
         IAL_HPWRK_TRF2, IAL_UNWRK_TRF2, IAL_ABLMED_TRF2, IAL_HPMED_TRF2, 
         IAL_UNMED_TRF2, IAL_ABLMO_TRF2, IAL_HPMO_TRF2, IAL_UNMO_TRF2)

# 7.3 merge ADL/IADL items ####

# Identify columns that end with "COF2" or "_TRF2"
com_cols <- grep("_COF2$", names(CLSA.FU.select), ignore.case = TRUE, value = TRUE)
trm_cols <- grep("_TRF2$", names(CLSA.FU.select), ignore.case = TRUE, value = TRUE)

# Extract base names
com_bases <- sub("_COF2$", "", com_cols, ignore.case = TRUE)
trm_bases <- sub("_TRF2$", "", trm_cols, ignore.case = TRUE)

# Identify common base names
common_bases <- intersect(com_bases, trm_bases)

# Initialize a list to store data frames of coalesced columns
coalesced_dfs <- list()

# Coalesce the values for the common base names
for (base_name in common_bases) {
  com_col <- paste0(base_name, "_COF2")
  trm_col <- paste0(base_name, "_TRF2")
  
  # Ensure columns exist in the current dataset
  if (com_col %in% names(CLSA.FU.select) && trm_col %in% names(CLSA.FU.select)) {
    # Convert both columns to character type
    CLSA.FU.select[[com_col]] <- as.character(CLSA.FU.select[[com_col]])
    CLSA.FU.select[[trm_col]] <- as.character(CLSA.FU.select[[trm_col]])
    
    # Coalesce values into a new data frame and add to the list
    coalesced_dfs[[base_name]] <- CLSA.FU.select%>%
      transmute(!!base_name := coalesce(!!sym(com_col), !!sym(trm_col)))
  }
}

# Combine the coalesced columns with the original data frame
CLSA.FU2.v1 <- bind_cols(CLSA.FU.select, coalesced_dfs)

# Create a vector of column names that should be removed (those that have been coalesced)
cols_to_remove <- unlist(lapply(common_bases, function(base) {
  c(paste0(base, "_COF2"), paste0(base, "_TRF2"))
}))

# Check if the columns to remove actually exist in the data frame
existing_cols_to_remove <- cols_to_remove[cols_to_remove %in% names(CLSA.FU2.v1)]

# Remove only the coalesced "_COM" and "_TRM" columns that exist
CLSA.FU2.v1.1 <- CLSA.FU2.v1 %>%
  select(-all_of(existing_cols_to_remove))

# 7.4 remove placeholder values (e.g., -9999), and create summary items ####

# format missing variables
CLSA.FU2.v1.2 <- CLSA.FU2.v1.1 %>%
  mutate(across(where(is.numeric), ~na_if(., -99999))) %>% 
  mutate(across(where(is.character), ~na_if(., "-99999")))


# 1) OARS Scale: Number of Missing Items (Excluding Meal Preparation) Variable Name: ADL_NBRMIS
# Description: This variable counts the total number of daily activities, both basic and instrumental,
# except for meal preparation, for which the respondent did not provide an answer of their ability to 
# perform the task.

# ADL_NBRMIS == Number of missing items, excluding meal preparation.

CLSA.FU2.v1.3 <- CLSA.FU2.v1.2 %>%
  mutate(ADL_IMDR = if_else(ADL_ABLDR == 1 | ADL_HPDR == 1 | ADL_UNDR == 1, 0, 1),
         ADL_IMFD = if_else(ADL_ABLFD == 1 | ADL_HPFD == 1 | ADL_UNFD == 1, 0, 1),
         ADL_IMAP = if_else(ADL_ABLAP == 1 | ADL_HPAP == 1 | ADL_UNAP == 1, 0, 1),
         ADL_IMWK = if_else(ADL_ABLWK== 1 | ADL_HPWK == 1 | ADL_UNWK == 1, 0, 1),
         ADL_IMBD = if_else(ADL_ABLBD == 1 | ADL_HPBD == 1 | ADL_UNBD == 1, 0, 1),
         ADL_IMBT = if_else(ADL_ABLBT == 1 | ADL_HPBT == 1 | ADL_UNBT == 1, 0, 1),
         ADL_IMBATH = if_else(
           (ADL_BATH != 1 & ADL_BATH != 2) | (ADL_BATH == 1 & !ADL_INCNT %in% c(1, 2, 3)), 1,
           if_else(ADL_BATH == 1 & ADL_INCNT %in% c(1, 2, 3) | ADL_BATH == 2, 0, NA_real_)),
         ADL_IMTEL = if_else(IAL_ABLTEL == 1 | IAL_HPTEL == 1 | IAL_UNTEL == 1, 0, 1),
         ADL_IMTRV = if_else(IAL_ABLTRV == 1 | IAL_HPTRV == 1 | IAL_UNTRV == 1, 0, 1),
         ADL_IMGRO = if_else(IAL_ABLGRO == 1 | IAL_HPGRO == 1 | IAL_UNGRO == 1, 0, 1),
         ADL_IMWRK = if_else(IAL_ABLWRK == 1 | IAL_HPWRK == 1 | IAL_UNWRK == 1, 0, 1),
         ADL_IMMED = if_else(IAL_ABLMED == 1 | IAL_HPMED == 1 | IAL_UNMED == 1, 0, 1),
         ADL_IMMO = if_else(IAL_ABLMO == 1 | IAL_HPMO == 1 | IAL_UNMO == 1, 0, 1))

# List of variables to sum
variables_to_sum <- c("ADL_IMDR", "ADL_IMFD", "ADL_IMAP", "ADL_IMWK",
                      "ADL_IMBD", "ADL_IMBT", "ADL_IMBATH", "ADL_IMTEL", "ADL_IMTRV",
                      "ADL_IMGRO", "ADL_IMWRK", "ADL_IMMED", "ADL_IMMO")

# Filter the list to include only those variables present in the dataset
existing_vars_to_sum <- variables_to_sum[variables_to_sum %in% names(CLSA.FU2.v1.3)]

# Create the new variable 'ADL_NBRMIS'
CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(ADL_NBRMIS = rowSums(select(., all_of(existing_vars_to_sum)), na.rm = TRUE))


# 2) OARS Scale: Some or Complete Dependence for Meal Preparation - Intermediate Derived Variable
# Derived Variable Name: ADL_DMEA_TRM
# Description: This variable indicates whether a respondent is able to prepare their own meals. The 
# authors of the OARS instrument determined that the inability to prepare one’s own meals without help 
# is more detrimental to independent living than all other activities of daily living and hence, the meal 
# preparation component of the questionnaire should be considered separately when determining 
# functional capacity (2). 

# Create the new variable 'ADL_DMEA'
CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(ADL_DMEA = case_when(
    IAL_ABLML == 1 ~ 0,
    IAL_HPML == 1 | IAL_UNML == 1 ~ 1,
    TRUE ~ 9
  ))

# 3) OARS scale: Sum of Some Dependence and Complete Dependence (Excluding Meal                                                                Preparation) – Temporary Variable
# Derived Variable Name: ADL_TDSUM_TRM
# Description: This variable calculates the total number of times the respondent indicated that they need 
# help with an activity or that they are completely unable to do an activity in the ADL and IAL modules, 
# excluding the questions regarding meal preparation ability (IAL_ABLML_TRM, IAL_HPML_TRM, and 
# IAL_UNML_TRM). It ignores missing values and cannot be interpreted without considering the number 
# of missing items, ADL_NBRMIS_TRM. This variable in not provided in the CLSA dataset

CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(
    ADL_THPDR = case_when(ADL_HPDR == 1 ~ 1,TRUE ~ 0),
    ADL_TUNDR = case_when(ADL_UNDR == 1 ~ 1,TRUE ~ 0),
    ADL_THPFD = case_when(ADL_HPFD == 1 ~ 1,TRUE ~ 0),
    ADL_TUNFD = case_when(ADL_UNFD == 1 ~ 1,TRUE ~ 0),
    ADL_THPAP = case_when(ADL_HPAP == 1 ~ 1,TRUE ~ 0),
    ADL_TUNAP = case_when(ADL_UNAP == 1 ~ 1,TRUE ~ 0),
    ADL_THPWK = case_when(ADL_HPWK == 1 ~ 1,TRUE ~ 0),
    ADL_TUNWK = case_when(ADL_UNWK == 1 ~ 1,TRUE ~ 0),
    ADL_THPBD = case_when(ADL_HPBD == 1 ~ 1,TRUE ~ 0),
    ADL_TUNBD = case_when(ADL_UNBD == 1 ~ 1,TRUE ~ 0),
    ADL_THPBT = case_when(ADL_HPBT == 1 ~ 1,TRUE ~ 0),
    ADL_TUNBT = case_when(ADL_UNBT == 1 ~ 1,TRUE ~ 0),
    ADL_TINCNT = case_when(ADL_INCNT %in% c(2, 3) ~ 1,TRUE ~ 0),
    IAL_THPTEL = case_when(IAL_HPTEL == 1 ~ 1,TRUE ~ 0),
    IAL_TUNTEL = case_when(IAL_UNTEL == 1 ~ 1,TRUE ~ 0),
    IAL_THPTRV = case_when(IAL_HPTRV == 1 ~ 1,TRUE ~ 0),
    IAL_TUNTRV = case_when(IAL_UNTRV == 1 ~ 1,TRUE ~ 0),
    IAL_THPGRO = case_when(IAL_HPGRO == 1 ~ 1,TRUE ~ 0),
    IAL_TUNGRO = case_when(IAL_UNGRO == 1 ~ 1,TRUE ~ 0),
    IAL_THPWRK = case_when(IAL_HPWRK == 1 ~ 1,TRUE ~ 0),
    IAL_TUNWRK = case_when(IAL_UNWRK == 1 ~ 1,TRUE ~ 0),
    IAL_THPMED = case_when(IAL_HPMED == 1 ~ 1,TRUE ~ 0),
    IAL_TUNMED = case_when(IAL_UNMED == 1 ~ 1,TRUE ~ 0),
    IAL_THPMO = case_when(IAL_HPMO == 1 ~ 1,TRUE ~ 0),
    IAL_TUNMO = case_when(IAL_UNMO == 1 ~ 1,TRUE ~ 0))


# List of variables to sum
variables_to_sum <- c("ADL_THPDR", "ADL_TUNDR", "ADL_THPFD", "ADL_TUNFD", "ADL_THPAP", 
                      "ADL_TUNAP", "ADL_THPWK", "ADL_TUNWK", "ADL_THPBD", "ADL_TUNBD",
                      "ADL_THPBT", "ADL_TUNBT", "ADL_TINCNT", "IAL_THPTEL", "IAL_TUNTEL",
                      "IAL_THPTRV", "IAL_TUNTRV", "IAL_THPGRO", "IAL_TUNGRO",
                      "IAL_THPWRK", "IAL_TUNWRK", "IAL_THPMED", "IAL_TUNMED", "IAL_THPMO",
                      "IAL_TUNMO") 

# Filter the list to include only those variables present in the dataset
existing_vars_to_sum <- variables_to_sum[variables_to_sum %in% names(CLSA.FU2.v1.3)]

# Create the new variable 'ADL_NBRMIS'
CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(ADL_TDSUM = rowSums(select(., all_of(existing_vars_to_sum)), na.rm = TRUE))


# 4) OARS scale: Basic and Instrumental Activities of Daily Living Classification (Excluding Meal Preparation) – Intermediate Derived Variable
# Derived Variable Name: ADL_DCLST_TRM
# Description: This variable categorizes the respondent’s ability to perform activities of daily living based 
# on the number of times they indicated that they need help with an activity or that they are completely 
# unable to do an activity in the ADL and IAL modules, excluding the questions regarding meal preparation 
# ability (IAL_ABLML_TRM, IAL_HPML_TRM, and IAL_UNML_TRM). The classification is done according to 
# the instructions provided in Table 11 of the OARS manual (2). However, the CLSA has augmented the 
# classification conditions to also take account of the number of missing items. If the possible values of 
# those missing items would not change the results of a classification, then the classification can be made. 
# Hence, some participants with missing items who would have otherwise had a missing classification can 
# be classified. The classification values range from 0 (no problems performing activities of daily living) to 
# 4 (complete inability in performing daily activities).


CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(
    ADL_DCLST = case_when(
      ADL_TDSUM == 0 & ADL_NBRMIS == 0 ~ 0,
      ADL_TDSUM %in% 1:3 & ADL_NBRMIS == 0 | ADL_TDSUM %in% 1:2 & ADL_NBRMIS == 1 | ADL_TDSUM == 1 & ADL_NBRMIS == 2 ~ 1,
      ADL_TDSUM %in% 4:5 & ADL_NBRMIS == 0 | ADL_TDSUM == 4 & ADL_NBRMIS == 1 ~ 2,
      ADL_TDSUM %in% 6:7 & ADL_NBRMIS == 0 | ADL_TDSUM == 6 & ADL_NBRMIS == 1 ~ 3,
      ADL_TDSUM %in% 8:13 ~ 4,
      TRUE ~ 9
    )
  )

# 5) OARS scale: Basic and Instrumental Activities of Daily Living Classification
# Derived Variable Name: ADL_DCLS_TRM
# Description: This variable is an overall classification of a respondent’s capacity to perform activities of 
# daily living. It is based on the 5-point scale described in Table 11 of the OARS manual (2), which ranges 
# from 2 (Excellent/Good) to 6 (Total Impairment). The CLSA has modified this scale so that the range is 
# now 1 to 5, and the first category “Excellent/Good” has been renamed “No functional impairment”. 
# Higher values indicate greater impairment. This variable combines the functional status classification 
# determined from all activities of daily living excluding meal preparation (ADL_DCLST_TRM) with the 
# meal preparation indicator (ADL_DMEA_TRM), creating an overall classification of functional capacity 
# that assigns extra weight to the meal preparation component.

CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(
    ADL_DCLS = case_when(
      ADL_DMEA == 0 & ADL_DCLST == 0 ~ 1,
      ADL_DMEA == 0 & ADL_DCLST == 1 ~ 2,
      (ADL_DMEA == 1 & ADL_DCLST %in% c(0, 1)) | ADL_DCLST == 2 ~ 3,
      ADL_DCLST == 3 ~ 4,
      ADL_DCLST == 4 ~ 5,
      TRUE ~ NA_real_
    )
  )


# 6) OARS scale: Sum of Some Dependence and Complete Dependence (Excluding Meal Preparation)
# Derived Variable Name: ADL_DSUM_TRM
# Description: This variable calculates the total number of times the respondent indicated that they need 
# help with an activity or that they are completely unable to do an activity in the ADL and IAL modules, 
# excluding the questions regarding meal preparation ability (IAL_ABLML_TRM, IAL_HPML_TRM, and 
# IAL_UNML_TRM).If there are any missing items, it too is missing. However, it distinguishes between 
# participants with missing items who have provided sufficient information for a Basic and Instrumental 
# Activities of Daily Living Classification and those who have not.

CLSA.FU2.v1.3 <- CLSA.FU2.v1.3 %>%
  mutate(
    ADL_DSUM = case_when(
      ADL_NBRMIS == 0 ~ ADL_TDSUM,
      ADL_NBRMIS > 0 & ADL_DCLST == 9 ~ NA_real_,
      ADL_NBRMIS > 0 & ADL_DCLST != 9 ~ NA_real_
    )
  )

# 7.5 merge FU2 variables to baseline dataset #### 

CLSA.FU2.v1.3.select <- CLSA.FU2.v1.3 %>%
  select(entity_id, ADL_DSUM,  ADL_DCLS) 

CLSA.baseline.FU2 <- full_join(CLSA.baseline.v1.4, CLSA.FU2.v1.3.select, by = "entity_id")

write.csv(CLSA.baseline.FU2, "CLSA.basleine.followup2.csv")
CLSA.baseline.FU2<- read.csv("CLSA.basleine.followup2.csv")
CLSA.baseline.FU2<-CLSA.baseline.FU2%>%as_tibble()











