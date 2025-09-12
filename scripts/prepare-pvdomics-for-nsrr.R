ver="0.1.0"

library(tidyverse)
library(readxl)

# selectedvars <- c(
#   "GENDER","FEMALE","HISPANIC","RACE","BMI","FC","PH_PREV","F_STATUS",
#   "F_WNOWT","F_WNOWT_C","F_GROUP","F_COMPGRP","PH_AGE_DIAG","PH_YRS","MI",
#   "CAB","ATRIAL_ARRHY","CARDIAC_SHUNT","SHUNT_PFO","SHUNT_ASD","SHUNT_VSD",
#   "SHUNT_SEPTOSTOMY","SHUNT_STATUS","TIA","CVA","HYPERTENSION","DIABETES",
#   "HYPOTHYROIDISM","AIRWAY_DIS","DEPRESSION_YN","SMOKE","CIG_PACK_YRS",
#   "TIM_ENR_QUITCIG","PH_PDE5I","PH_ERA","PH_PROST","PH_SGC","PH_CCB","PH_MED",
#   "CARDIAC_MED","ANTIARRHYTHMICS","ANTICOAGULANTS_DOACS","ANTICOAGULANTS_OTHER",
#   "ANTIHYPERTENSIVES_OTHER","REST_O2","REST_LPM","NIGHT_O2","NIGHT_LPM",
#   "EXERTION_O2","EXERTION_LPM","CPAP_USED","PVDSLEEP","O2_NIGHT","AHI_C",
#   "OAHI_C","CAI_C","PCTLT90_C","ODI3P","ODI4P","TOT_SLEEP_TM","HI_C","C_OSA","C_OHS") |> 
#   tolower()
# 
# view(selectedvars)

datapath <- "/Volumes/bwh-sleepepi-nsrr-staging/20241025-pvdomics/original-data/nsrr_rel1noft_d04302024_v04152025.csv"

df <- read.csv(datapath) |>
  select(-PID)   #deidentify, use alt_pid instead

df <- df |>
  rename_with(tolower)|> 
  mutate(across(where(is.character), ~na_if(., "")))|>
  relocate(alt_pid, .before = 1)|>
  arrange(alt_pid)

# unique(df$f_group)
# sum(is.na(df$f_group))
# #?? for 'f_group' there is a '.' category, should i change this to "0" or something else?
# 
# setdiff(selectedvars, colnames(df))
# setdiff(colnames(df), unlist(selectedvars))


###NSRR Harmonized:
df_h <- df|>
  select(alt_pid, age, female, race, hispanic, bmi, smoke, ahi_c, oahi_c, cai_c, odi3p, odi4p, tot_sleep_tm)|> #they also have age_sleep: age during sleep study 
  rename(nsrrid = alt_pid,
         nsrr_ahi_hp3u = ahi_c,
         nsrr_oahi_hp3u = oahi_c,
         nsrr_cai = cai_c,
         nsrr_odi_dsge3 = odi3p,
         nsrr_odi_dsge4 = odi4p,
         nsrr_tst_f1 = tot_sleep_tm)|>
  mutate(visit = 1,
         nsrr_age = age,
         nsrr_sex = case_match(female,
                               0 ~ "male",
                               1 ~ "female"),
         nsrr_race = case_match(race,
                                1 ~ "american indian or alaska native",
                                2 ~ "asian",
                                4 ~ "black or african american",
                                5 ~ "white",
                                6 ~ "multiple",
                                9 ~ "not reported"),
         nsrr_ethnicity = case_match(hispanic,
                                     0 ~ "not hispanic",
                                     1 ~ "hispanic or latino",
                                     NA ~ "not reported"),
         nsrr_bmi = bmi,
         nsrr_current_smoker = case_match(smoke,
                                          0 ~ "no",
                                          1 ~ "yes"))|>
  select(-c(age, bmi, female, race, hispanic, smoke))|>
  relocate(nsrr_ahi_hp3u, nsrr_oahi_hp3u, nsrr_cai,
           nsrr_odi_dsge3, nsrr_odi_dsge4, nsrr_tst_f1,
           .after = last_col())
