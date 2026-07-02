version <- "0.1.0.pre5"
releasepath <- "/Volumes/bwh-sleepepi-nsrr-staging/20241025-pvdomics/nsrr-prep/_releases"

library(tidyverse)


datapath <- "/Volumes/bwh-sleepepi-nsrr-staging/20241025-pvdomics/original-data/nsrr_rel1noft_d04302024_v04152025.csv"

df <- read.csv(datapath) |>
  select(-PID) |>  #deidentify, use alt_pid instead
  rename_with(tolower)|> 
  mutate(visit = 1,
         across(where(is.character), ~na_if(., "")))|>
  relocate(alt_pid, .before = 1)|>
  relocate(visit, .before = 2)|>
  arrange(alt_pid) |>
  mutate( # remove undefined values
    tot_sleep_tm = case_when( # remove total sleep time for PID 410204 (outlier total sleep time, no sleep report available)
      tot_sleep_tm == 4748 ~ NA,
      TRUE ~ tot_sleep_tm)) 

## for f151_othdev, f151_oxygen, f151_papdev -> recode 9 to NA 

###------- Adding release 2 variables to the main df: --------##

datapath2 <- "/Volumes/bwh-sleepepi-nsrr-staging/20241025-pvdomics/original-data/rel2-20260609/PVD_NSRR_rel2_noft_d02052026_v05282026.csv"

df2 <- read.csv(datapath2) |>
  select(-PID) |>  #deidentify, use alt_pid instead
  rename("alt_pid" = "pidd") |>
  rename_with(tolower)

df_joined <- full_join(df, df2, by = "alt_pid", suffix = c("", ".df2")) 

overlap_cols <- intersect(names(df), names(df2))
overlap_cols <- setdiff(overlap_cols, "alt_pid")

for (col in overlap_cols) {
  df_joined[[col]] <- coalesce(df_joined[[col]], df_joined[[paste0(col, ".df2")]])
}

df_joined <- df_joined |>
  select(-all_of(paste0(overlap_cols, ".df2"))) |>
  mutate(
    across(
      c(f151_othdev, f151_oxygen, f151_papdev),
      ~ na_if(., 9)
    )
  ) |>
  mutate(
    across(
      c(f151_othdev, f151_oxygen, f151_papdev),
      ~ na_if(., 9))
  ) |>
  mutate(across(where(is.character), ~ na_if(str_trim(.x), ""))) 


write.csv(df_joined, file.path(releasepath, paste0(version, "/pvdomics-dataset-", version, ".csv")), na = "", row.names = F)



###NSRR Harmonized:
df_h <- df|>
  select(alt_pid, visit, age, female, race, hispanic, bmi, smoke, ahi_c, odi3p, odi4p)|> #they also have age_sleep: age during sleep study 
  rename(nsrrid = alt_pid,
         nsrr_rei_hp3n = ahi_c,
         #nsrr_oahi_hp3u = oahi_c,
         #nsrr_cai = cai_c,
         nsrr_odi_dsge3 = odi3p,
         nsrr_odi_dsge4 = odi4p) |>
         #nsrr_tst_f1 = tot_sleep_tm)|>
  mutate(
         nsrr_age = age,
         nsrr_sex = case_match(female,
                               0 ~ "male",
                               1 ~ "female"),
         nsrr_race = case_match(race,
                                1 ~ "american indian or alaska native",
                                2 ~ "asian",
                                3 ~ "native hawaiian or other pacific islander",
                                4 ~ "black or african american",
                                5 ~ "white",
                                6 ~ "multiple",
                                9 ~ "not reported"),
         nsrr_ethnicity = case_match(hispanic,
                                     0 ~ "not hispanic or latino",
                                     1 ~ "hispanic or latino",
                                     NA ~ "not reported"),
         nsrr_bmi = bmi,
         nsrr_current_smoker = case_match(smoke,
                                          0 ~ "no",
                                          1 ~ "yes"))|>
  select(-c(age, bmi, female, race, hispanic, smoke))|>
  relocate(nsrr_rei_hp3n,nsrr_odi_dsge3, nsrr_odi_dsge4, 
           .after = last_col())

write.csv(df_h, file.path(releasepath, paste0(version, "/pvdomics-harmonized-dataset-", version, ".csv")), na = "", row.names = F)


unique(df_joined$f_group)


