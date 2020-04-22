library(dplyr)
library(lubridate)
library(sf)
library(butteR)
library(tmap)
library(ggplot2)
source("scripts/prepare_msna_wash_inputs.R")


script_analysis_level<-c("grid","block")[1]
grid_size<- 300

pop<-read_csv("inputs/unhcr_rohingya_pop_stats_20200319.csv")
#MAKE POP DATA  SO IT CAN JOIN ADMIN SHP
pop_block<-pop %>%
  filter(!is.na(block)) %>%
  mutate(camp= str_replace_all(camp,c("Total"="")) %>% trimws(),
         block= str_replace_all(block,c("C04_Ext_A"="C04X_A")),
         v.female_young= female_lt1+female_lte4,
         v.male_young= male_lt1+male_lte4,
         v.female_young_old=v.female_young+female_gte60,
         v.male_young_old= v.male_young+male_gte60,
         v.old=male_gte60+female_gte60)


# READ IN CAMP AND MAKE GRID ----------------------------------------------
cmp_gdb<-"../../../../02_GIS_DataUnit/01_GIS_BASE_Data/01_boundaries/03_camp"
cmp<-st_read(cmp_gdb,"CampBoundaryA1_190310")

if(script_analysis_level=="block"){
strata_poly<-st_read(cmp_gdb,"CampBoudaryA2_190310_reach_edited", stringsAsFactors = F) %>%
  st_transform(crs=4326)
  aggreg_to= "BlockNam"
  strata_poly<-strata_poly %>%
    mutate(BlockNam=ifelse(CampName=="Kutupalong RC", paste0("KTP_",Blck_Let),BlockNam)) %>%
    filter(!is.na(CampName))

  strata_poly %>%    filter(!BlockNam %in% pop_block$block)

}


if(script_analysis_level=="grid"){
  aggreg_to="grid_id"
  strata_poly<-st_make_grid(x = cmp %>% st_transform(crs=32646),cellsize = grid_size,square = F) %>%
    st_transform(crs=4326)
  strata_poly<- st_as_sf(data.frame(grid_id=paste0("gr_",1:length(strata_poly)),geometry=strata_poly))

}



pop_block %>%
  filter(!block %in% strata_poly$BlockNam)%>% #pull(block)
  filter(block!="No Block", block!="No block") %>%
  filter(camp != "Camp 20 Extension",camp!= "Nayapara RC") %>%
  arrange(desc(total_fam)) %>%
  select(camp, block, total_fam, total_indiv) %>% data.frame()
pop_block %>%
  filter(!block %in% strata_poly$BlockNam)%>% #pull(block)
  filter(block!="No Block") %>%
  filter(camp != "Camp 20 Extension",camp!= "Nayapara RC") %>%
  group_by(camp) %>%
  summarise(total_fam_unmatched= sum(total_fam),
            total_indiv_unmatched= sum(total_indiv)) %>%
  arrange(desc(total_fam_unmatched))
  pull(total_indiv_unmatched) %>% sum()


pull(block)
pop_block %>% filter(camp=="Camp 14")
strata_poly %>% filter(CampName=="Camp 14")
# GOOGLESHEETS4! HOW EXCITING - READ IN DAP/ANALYSIS FRAMEWORK ------------

googlesheet_url<-"https://docs.google.com/spreadsheets/d/1oqhPtJdhe1a1kr1MojGoWfPxbgDlIaEeISe1nuKSh2k/"
dap<-googlesheets4::read_sheet(googlesheet_url, sheet = "sdr_indicator_selection")
analysis_params<-googlesheets4::read_sheet(googlesheet_url, sheet = "sdr_analysis_params")




wash_hh_indicators<-dap %>%
  filter(assessment=="WASH_R_3" & spatial_level_2== script_analysis_level) %>%
  pull(indicator)
msna_indiv_dap<-dap %>% filter(assessment=="MSNA_Ref"& spatial_level_2== script_analysis_level & analysis_level=="Ind")
msna_indiv_indicators<-msna_indiv_dap %>% pull(indicator)
msna_hh_indicators<-dap %>%
  filter(assessment=="MSNA_Ref"& spatial_level_2== script_analysis_level & analysis_level=="HH") %>%
  pull(indicator)

wash_hh_sf<-st_as_sf(wash_hh,coords = kobo_coords, crs=4326)
wash_joined_strata<-wash_hh_sf %>%
  st_join(strata_poly)


msna_indiv_sf<- st_as_sf(msna_indiv, coords=kobo_coords, crs=4326)
msna_hh_sf<- st_as_sf(msna_hh, coords=kobo_coords, crs=4326)
msna_joined_strata<- msna_indiv_sf %>%
  st_join(strata_poly)
msna_hh_joined_strata<- msna_hh_sf %>%
  st_join(strata_poly)




# st_write(hex,dsn = "outputs/gis/hex200.shp")
if (script_analysis_level=="grid"){
strata_poly %>%
  st_join(wash_hh_sf) %>%
  group_by(grid_id) %>%
  summarise(number_per_hex=n()) %>%
  ggplot()+
  geom_histogram(aes(x=number_per_hex),
                 bins=100)+
  scale_x_log10()
}

# analyze_data ------------------------------------------------------------
library(srvyr)

msna_joined_strata$I.ind_smoke_female<-forcats::fct_expand(msna_joined_strata$I.ind_smoke_female, c("no","yes"))

wash_joined_strata$wst_burn<-ifelse(is.na(wash_joined_strata$wst_burn), "no", "yes")

wash_hh_svy<-as_survey(wash_joined_strata %>% st_drop_geometry())
msna_indiv_svy<-as_survey(msna_joined_strata %>% st_drop_geometry())
msna_hh_svy<-as_survey(msna_hh_joined_strata %>% st_drop_geometry())


wash_indicators_analyzed<-butteR::mean_proportion_table(design = wash_hh_svy,
                                                             list_of_variables = wash_hh_indicators,
                                                             aggregation_level = aggreg_to,
                                                             na_replace = F)



msna_indiv_indicators_analyzed<-butteR::mean_proportion_table(design = msna_indiv_svy,
                                                             list_of_variables = msna_indiv_indicators,
                                                             aggregation_level = aggreg_to,
                                                             na_replace = F)

msna_hh_indicators_analyzed<-butteR::mean_proportion_table(design = msna_hh_svy,
                                                             list_of_variables = msna_hh_indicators,
                                                             aggregation_level = aggreg_to,
                                                             na_replace = F)






msna_indicators_to_map<-analysis_params %>% filter(assessment== "MSNA_Ref"  ) %>% pull(options)
msna_indicators_analyzed<-msna_indiv_indicators_analyzed%>% left_join(msna_hh_indicators_analyzed)
msna_indicators_to_map<- msna_indicators_to_map[msna_indicators_to_map %in% colnames(msna_indicators_analyzed)]
msna_analysis_to_map<-msna_indicators_analyzed %>% select(c(aggreg_to,msna_indicators_to_map))


analysis_params %>% View()
# wash_indicators_to_map[!wash_indicators_to_map %in%wash_indicators_to_map2]
wash_indicators_to_map<-analysis_params %>% filter( assessment=="WASH_R_3") %>% pull(options)
wash_indicators_to_map<- wash_indicators_to_map[wash_indicators_to_map %in% colnames(wash_indicators_analyzed)]
wash_analysis_to_map<-wash_indicators_analyzed %>% select(aggreg_to,wash_indicators_to_map)


library(tidyverse)

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


if(script_analysis_level=="block"){
  results_to_map_long<-results_to_map_long %>%
    left_join(pop_block, by=c("BlockNam"="block")) %>%
    mutate(
      proj_pop= indicator_val*total_indiv
    )
  results_to_map_long %>%
    filter(indicator=="I.ind_age_group.gte_60") %>%
    select(indicator_val,v.old,proj_pop)}

strata_poly_long<- strata_poly %>%
  mutate(wash_pts_per_grid=lengths(st_intersects(strata_poly,wash_hh_sf)),
         msna_indiv_pts_per_grid=lengths(st_intersects(strata_poly,msna_indiv_sf))) %>%
  left_join(results_to_map_long) %>%
  mutate(pts_per_grid= ifelse(assessment=="WASH_R_3",wash_pts_per_grid, msna_indiv_pts_per_grid))

strata_poly_long$indicator %>% unique()
# st_write(strata_poly_long,"outputs/gis/bgd_covid2020.gpkg",layer="msna_ref_wash_2019_indicators_300m", layer_options = 'OVERWRITE=YES', update = TRUE)

# st_write(strata_poly_long, paste0("outputs/gis/bgd_covid2020_", grid_size,"m/" ,"bgd_covid2020_",grid_size,"m.shp"),delete_layer = TRUE)



strata_poly_long_filtered<-strata_poly_long %>%
  mutate(indicator_val=ifelse(pts_per_grid<=3,NA,indicator_val) %>% as.numeric)

strata_poly_long_filtered %>%
  filter(indicator=="I.HEALTH.indhelp_atleast.INDVHH.yes") %>%
  ggplot(aes(x=indicator_val))+
  geom_histogram(bins=100)


strata_poly_age_groups<-strata_poly_long_filtered %>%
  filter(str_detect(indicator, "group.gte"))

strata_poly_age_groups %>%
  filter(indicator_val>.3)
strata_poly_age_groups %>%
  filter(grid_id=="gr_135")

windows();ggplot(strata_poly_age_groups %>% filter(!is.na(indicator_val)),
       aes(x= indicator_val, fill=cut(indicator_val,breaks=c(0,.02,.05,.10,.15,.20,.25,.30,.35,.40),include.lowest = T,right = T,)))+
  geom_histogram(bins=100)+
  ggplot2::facet_wrap(facets = ~indicator)





# dir.create("outputs/gis/bgd_covid2020_blocklevel")
 # st_write(strata_poly_long, "outputs/gis/bgd_covid2020_blocklevel/bgd_covid2020_blocklevel.shp")







windows();ggplot() +
  geom_sf(data=hex_long_filt %>% filter(indicator== "I.ind_age_group.gte_60"),
          aes(fill=indicator_val))+
  scale_fill_gradient2(low="#fcf6e1ff",
                       mid= "#b5d5ffff",
                       high="#fad2b2ff",
                       midpoint = 0.15)
tmap %>%



hex_long_filt %>%st_drop_geometry( ) %>%  group_by(indicator) %>%
  summarise(indic_min=min(indicator_val,na.rm=T),
            indic_max=max(indicator_val,na.rm=T)) %>% arrange(indic_max) #%>%
  # write.csv("covid_indicators_by_300m_grid_range.csv")
hex_poly_long %>%
  mutate(indicator_val=ifelse(pts_per_grid<=3,NA,indicator_val) %>% as.numeric) %>%
  filter(str_detect(indicator, "age_group.gte")) %>%
  pull(indicator_val) %>% range(na.rm=T)

indicators_unique<-hex_long_filt$indicator %>% unique()
histos<-list()
for(i in 1: length(indicators_unique)){
  indicator_temp<-indicators_unique[i]
  df_temp<-hex_long_filt %>%
    filter(indicator==indicator_temp)
  histos[[indicator_temp]]<-df_temp %>%
    ggplot(aes(x=indicator_val))+geom_histogram()


}
histos$I.ind_age_group.gte_40

# st_write(hex_poly_long,"outputs/gis/bgd_covid2020.gpkg",layer="msna_ref_wash_2019_indicators_250m")
# dir.create(paste0("outputs/gis/bgd_covid2020_", grid_size,"m"))
# st_write(hex_poly_long, paste0("outputs/gis/bgd_covid2020_", grid_size,"m/" ,"bgd_covid2020_",grid_size,".shp"))



# scrap used to make clean dap google sheet -------------------------------


msna_analysis_to_map_long<-msna_analysis_to_map %>%
  pivot_longer(-grid_id,
               names_to="indicator",
               values_to="indicator_val") %>%
  mutate(assessment="MSNA") %>%
  left_join(dap %>% select(options,label), by=c("indicator"="options")) %>%
  mutate(label=case_when(
    str_detect(string = indicator,pattern = "gte")~
      paste0("% Individuals >= ", indicator %>% tolower_rm_special() %>% parse_number),
    str_detect(string = indicator,pattern = "lt")~
      paste0("% Individuals < ", indicator %>% tolower_rm_special() %>% parse_number),
    TRUE~label
  )
  )
wash_analysis_to_map_long<-wash_analysis_to_map %>%
  pivot_longer(-grid_id,
               names_to="indicator",
               values_to="indicator_val") %>%
  mutate(assessment="WASH_R3") %>%
  left_join(dap %>% select(options,label), by=c("indicator"="options"))


grid_results_to_map_long<-do.call("rbind", list(wash_analysis_to_map_long, msna_analysis_to_map_long))

hex_poly_long<- hex_poly %>%
  mutate(wash_pts_per_grid=lengths(st_intersects(hex_poly,wash_hh_sf)),
         msna_indiv_pts_per_grid=lengths(st_intersects(hex_poly,msna_indiv_sf))) %>%
  left_join(grid_results_to_map_long)


st_write(hex_poly_long,"outputs/gis/hex200_washr3_msna_long.shp")

hex_output<- hex_poly %>%
  left_join(wash_analysis_grids_gt3)

st_write(hex_output,dsn = "outputs/gis/hex200_washr3.shp")

tmap_mode("view")
wash_indicator_maps<-tm_basemap(NULL)+
  tm_shape(cmp)+
  tm_polygons()+
  tm_text("New_Camp_N")+
  tm_shape(whh_sf)+
  tm_dots(c("lat_soap",
            "wst_disp",
            "soap_yest",
            "soap_yest_when",
            "wash_hands",
            "hnd_sp",
            "IS.disab"
            ))+
  tm_facets(sync = FALSE, ncol = 3)+
  tmap_options(limits=c(facets.view=7))
wash_leaflet<-tmap_leaflet(wash_indicator_maps)
htmltools::save_html(wash_leaflet, "wash_indicators_leaflet.html")



class(wash_indicator_maps)
htmltools::save_html(wash_indicator_maps, "wash_indicators.html")
htmlwidgets::saveWidget(wash_indicator_maps,"asdf.html")
tmap::save_tmap(wash_indicator_maps,"asdf.html")
tmap::tmap_save(wash_indicator_maps, "wash_indicators.html")

tm_shape(whh_sf)+
  tm_dots("lat_soap", "wst_disp")+
tm_shape(whh_sf)+
  tm_dots("wst_disp")


tmap::tm_sym



msna_camp<-read.csv(msna_camp_path, na.strings = c("NA",""," "),stringsAsFactors = F)










wash_r3$soap_yest
msna_camp %>%
  group_by(camp_name,
           pregnant_woman) %>%
  count() %>%
  arrange(pregnant_woman,n) %>% data.frame()
