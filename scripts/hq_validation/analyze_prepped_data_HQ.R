
library(dplyr)
library(lubridate)
library(sf)
library(butteR)
library(tmap)
library(ggplot2)
library(googlesheets4)
library(tidyverse)
source("scripts/hq_validation/prepare_msna_wash_inputs_HQ.R")

script_analysis_level<-c("grid","block")[1]
if(script_analysis_level=="grid"){
  aggreg_to="grid_id"
}
analysis_framework<-butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/dap")

dap<-analysis_framework$covid_indicator_indicator_dap.csv
analysis_params<-analysis_framework$covid_analysis_parameter_selection.csv

wash_hh_indicators<-dap %>%
  filter(assessment=="WASH_R_3" & spatial_level_2== script_analysis_level) %>%
  pull(indicator)
msna_indiv_dap<-dap %>% filter(assessment=="MSNA_Ref"& spatial_level_2== script_analysis_level)
msna_indiv_indicators<-msna_indiv_dap %>% pull(indicator)



# analyze_data ------------------------------------------------------------
library(srvyr)
msna_indiv$I.ind_smoke_female<-fct_expand(msna_indiv$I.ind_smoke_female, c("no","yes"))
wash_hh_svy<-as_survey(wash_hh)
msna_indiv_svy<-as_survey(msna_indiv)


wash_indicators_analyzed<-butteR::mean_proportion_table(design = wash_hh_svy,
                                                             list_of_variables = wash_hh_indicators,
                                                             aggregation_level = aggreg_to,
                                                             na_replace = F)
msna_indiv_indicators_analyzed<-butteR::mean_proportion_table(design = msna_indiv_svy,
                                                             list_of_variables = msna_indiv_indicators,
                                                             aggregation_level = aggreg_to,
                                                             na_replace = F)




#USE DAP TO SELECT VARIABLES TO SELECT INDICATOR RESPONSES TO MAP
msna_indicators_to_map<-analysis_params %>% filter(assessment== "MSNA_Ref" & analysis_level=="Ind" ) %>% pull(options)
msna_indicators_to_map<- msna_indicators_to_map[msna_indicators_to_map %in% colnames(msna_indiv_indicators_analyzed)]
msna_analysis_to_map<-msna_indiv_indicators_analyzed %>% select(c(aggreg_to,msna_indicators_to_map))


wash_indicators_to_map<-analysis_params %>% filter( assessment=="WASH_R_3") %>% pull(options)
wash_indicators_to_map<- wash_indicators_to_map[wash_indicators_to_map %in% colnames(wash_indicators_analyzed)]
wash_analysis_to_map<-wash_indicators_analyzed %>% select(aggreg_to,wash_indicators_to_map)



# REARRANGE DATA INTO LONG FORMAT TO JOIN TO POLYGON FOR ATLAS
# DRIVEN ATTRIBUTE MAPPING IN QGIS

msna_analysis_to_map_long<-msna_analysis_to_map %>%
  pivot_longer(-aggreg_to,
               names_to="indicator",
               values_to="indicator_val") %>%
  left_join(analysis_params %>% select(options,assessment ,label), by=c("indicator"="options"))


wash_analysis_to_map_long<-wash_analysis_to_map %>%
  pivot_longer(-aggreg_to,
               names_to="indicator",
               values_to="indicator_val") %>%
  left_join(analysis_params %>% select(options,assessment ,label), by=c("indicator"="options"))

results_to_map_long<-do.call("rbind", list(wash_analysis_to_map_long, msna_analysis_to_map_long))


strata_poly_long<- strata_poly %>%
  mutate(wash_pts_per_grid=lengths(st_intersects(strata_poly,wash_hh_sf)),
         msna_indiv_pts_per_grid=lengths(st_intersects(strata_poly,msna_indiv_sf))) %>%
  left_join(results_to_map_long) %>%
  mutate(pts_per_grid= ifelse(assessment=="WASH_R_3",wash_pts_per_grid, msna_indiv_pts_per_grid))


# st_write(strata_poly_long, "outputs/gis/hex300m_covid_results.shp")
