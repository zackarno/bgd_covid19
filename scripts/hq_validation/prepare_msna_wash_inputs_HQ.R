library(dplyr)
library(lubridate)
library(sf)
library(butteR)
library(tmap)
library(anytime)

#source recoding functinons
source("scripts/functions/wash_hh_r3_2019_recoding.R")
source("scripts/functions/wash_container_volume_bgd_2019.R")
# source("../../04_MSNA/2019_BGD_MSNAs/Functions/make_composite_indicators_bgd_msna_2019_mk2.R")
source("scripts/functions/make_composite_indicators_bgd_msna_2019.R")
# source("Functions/make_composite_indicators_bgd_msna_2019.R")

kobo_coords<-c("X_gps_reading_longitude",
               "X_gps_reading_latitude")


# read in data sets -------------------------------------------------------


msna_wash2019_datasets<- butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/inputs_hq")
msna_hh<- msna_wash2019_datasets$reach_bgd_dataset_2019refugee_msna_HH.csv
msna_indiv<-msna_wash2019_datasets$reach_bgd_dataset_2019refugee_msna_INDIV.csv
wash_hh<-msna_wash2019_datasets$reach_bgd_dataset_2019refugee_wash_hh_r3_HH.csv
wash_container<-msna_wash2019_datasets$reach_bgd_dataset_2019refugee_wash_hh_r3_CONTAINER.csv
wash_indiv<-msna_wash2019_datasets$reach_bgd_dataset_2019refugee_wash_hh_r3_INDIV.csv






msna_with_composite<-make_composite_indicators_bgd_msna_2019(hh_data = msna_hh,  individual_data = msna_indiv,population = "Refugee")





# JOIN COMPOSITE INDICATORS TO APPROPRIATE DATA SETS
msna_hh_with_composite<-msna_hh %>% left_join(msna_with_composite$household_composites,by="X_uuid")
msna_indiv_with_composite<-msna_indiv %>% left_join(msna_with_composite$individual_composites,by="X_index")




# GET INDIVIDUALDATA SORTED FOR ANALYSIS
msna_indiv_with_composite<-msna_indiv_with_composite %>%
  mutate(
    formal_treatment= treatment_location.govt_clinic| treatment_location.private_clinic| treatment_location.ngo_clinic,
    I.HEALTH.ind_formal_treatment= case_when(
      treatment_sought=="no"~ "no",
      treatment_sought=="yes" & formal_treatment==F~ "no",
      treatment_sought=="yes"~ "yes",
      TRUE ~ NA_character_),
    I.ind_age_group_gte60=ifelse(ind_age>=60, "yes","no"),
    I.ind_age_group_gte50=ifelse(ind_age>=50, "yes","no"),
    I.ind_age_group_gte40=ifelse(ind_age>=40, "yes","no"),
    I.ind_smoke_male= case_when(
      ind_gender!="male"~NA_character_,
      is.na(I.HEALTH.ind_smoke_some_all_days.INDV)~NA_character_,
      ind_gender== "male" & I.HEALTH.ind_smoke_some_all_days.INDV=="yes"~"yes",
      TRUE~"no"
    ),
    I.ind_smoke_female= case_when(
      ind_gender!="female"~NA_character_,
      is.na(I.HEALTH.ind_smoke_some_all_days.INDV)~NA_character_,
      ind_gender== "male" & I.HEALTH.ind_smoke_some_all_days.INDV=="yes"~"yes",
      TRUE~"no"
    )
  )



msna_indiv<-msna_indiv_with_composite
msna_hh<-msna_hh_with_composite



# make composite indicators
wash<-wash_hh_r3_2019_recoding(hh_data = wash_hh,
                               container_data = wash_container,
                               individual_data = wash_indiv)

wash_hh<-wash$HH_dataset

wash_hh<- wash_hh %>%
  mutate(
      I.time_coll_wat.gte10min=ifelse(time_coll_wat %in% c("10_min","15_min", "20_min","30_min","more_30"),"yes", "no"),
      I.time_wat.gte10min=ifelse(time_wat %in% c("10_min", "15_min","20_min", "30_min", "more_30"),"yes", "no"),
      I.wash_hands.all_food_times= ifelse(wash_hands.bfr_eat &
      wash_hands.bfr_brst_feed &
      wash_hands.bfr_prep_food &
      wash_hands.bfr_feed_child, "yes","no"),
      I.wash_hands.all_fecal_times= ifelse(wash_hands.cleaning_chld_bottom & wash_hands.aftr_defecation,"yes","no"),
      I.wst_disp.open_area= ifelse(wst_disp %in% c("undesignated_openarea","designated_openarea"), "yes","no"),
      I.is_disab=ifelse(IS.disab == 1 , "yes", "no")
  )
print("WASH HH object is called wash_hh")
print("MSNA HH  object is called msna_hh")
print("MSNA INDIV  object is called msna_indiv")






# prepare data sets for hq -scrap now ------------------------------------------------


# msna_ref<-butteR::read_all_csvs_in_folder("../../04_MSNA/2019_BGD_MSNAs/Inputs/Refugee/04_data_analysis/cleaned_data")
# msna_hh<- msna_ref$`20190915_HH_Refugee_Cleaned_20190917.csv` %>% filter(informed_consent=="yes")
# msna_indiv<- msna_ref$`20190915_Indiv_Refugee_Cleaned_20190917.csv` %>% filter(X_submission__uuid %in% msna_hh$X_uuid)
# msna_hh<-msna_hh %>% left_join(msna_ref$`20190915_HH_Refugee_Cleaned_20190917_with_environmental.csv` %>%
#                                                     select(X_uuid,kobo_coords))
# msna_indiv_with_gps<- msna_indiv %>% left_join(msna_hh %>% select(X_uuid,kobo_coords),by = c("X_submission__uuid"="X_uuid" ))
#
# # dir.create("inputs/inputs_hq")
# st_as_sf(msna_hh, coords=kobo_coords,crs=4326) %>%
#   st_join(strata_poly) %>% st_drop_geometry() %>%
#   write.csv("inputs/inputs_hq/reach_bgd_dataset_2019refugee_msna_HH.csv")
# st_as_sf(msna_indiv_with_gps, coords=kobo_coords,crs=4326) %>%
#   st_join(strata_poly) %>% st_drop_geometry() %>%
#   write.csv("inputs/inputs_hq/reach_bgd_dataset_2019refugee_msna_INDIV.csv")
#
#
# #read all wash hh r 3 clean data
# wash_r3_clean<-butteR::read_all_csvs_in_folder("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/inputs/WASH_HH_R3/clean_data")
# #get the raw hh data with coordinates
# wash_r3_hh_raw<-read.csv("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/02_data_collection/clean_data/Header_07july2019.csv",
#                          stringsAsFactors = FALSE,
#                          row.names = NULL, na.strings = c(""," ",NA, "NA"),
#                          strip.white = TRUE)
#
# wash_r3_hh_clean<- wash_r3_clean$Header_07july2019.csv %>% left_join(wash_r3_hh_raw %>%
#                                                                        select(X_uuid,kobo_coords))
#
# st_as_sf(wash_r3_hh_clean, coords=kobo_coords,crs=4326) %>%
#   st_join(strata_poly) %>% st_drop_geometry() %>%
#   write.csv("inputs/inputs_hq/reach_bgd_dataset_2019refugee_wash_hh_r3_HH.csv")

# wash_r3_clean$Container_19June2019.csv %>% write.csv("inputs/inputs_hq/reach_bgd_dataset_2019refugee_wash_hh_r3_CONTAINER.csv")
# wash_r3_clean$Individual_19June2019.csv %>% write.csv("inputs/inputs_hq/reach_bgd_dataset_2019refugee_wash_hh_r3_INDIV.csv")



