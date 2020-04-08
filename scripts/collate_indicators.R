library(dplyr)
library(lubridate)
library(sf)
library(butteR)
library(tmap)
msna_camp_path<- "../../04_MSNA/2019_BGD_MSNAs/Inputs/Refugee/04_data_analysis/cleaned_data/20190915_HH_Refugee_Cleaned_20190917.csv"


"../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/inputs/WASH_HH_R3/clean_data"

kobo_coords<-c("X_gps_reading_longitude",
               "X_gps_reading_latitude")
cmp_gdb<-"../../../../02_GIS_DataUnit/01_GIS_BASE_Data/01_boundaries/03_camp"
cmp<-st_read(cmp_gdb,"CampBoundaryA1_190310")
wash_r3_cont<-read.csv("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/inputs/WASH_HH_R3/clean_data/Container_19June2019.csv",
                       stringsAsFactors = FALSE,
                       row.names = NULL, na.strings = c(""," ",NA, "NA"),
                       strip.white = TRUE)

wash_r3_indiv<-read.csv("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/inputs/WASH_HH_R3/clean_data/Individual_19June2019.csv",
                        stringsAsFactors = FALSE,
                        row.names = NULL, na.strings = c(""," ",NA, "NA"),
                        strip.white = TRUE)
wash_r3_hh_raw<-read.csv("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/02_data_collection/clean_data/Header_07july2019.csv",
                     stringsAsFactors = FALSE,
                     row.names = NULL, na.strings = c(""," ",NA, "NA"),
                     strip.white = TRUE)

wash_r3_hh_clean<-read.csv("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/inputs/WASH_HH_R3/clean_data/Header_07july2019.csv",
                        stringsAsFactors = FALSE,
                        row.names = NULL, na.strings = c(""," ",NA, "NA"),
                        strip.white = TRUE)


wash_r3_hh_clean<- wash_r3_hh_clean %>% left_join(wash_r3_hh_raw %>%
                                 select(X_uuid,kobo_coords))
source("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/functions/wash_hh_r3_2019_recoding.R")
source("../../01_WASH/02_70_DNW_UNICEF_2019/03_WASH_HH_R3/89_rscript/WASH_HH_R3_2019/functions/wash_container_volume_bgd_2019.R")



# debugonce(wash_hh_r3_2019_recoding)
wash<-wash_hh_r3_2019_recoding(hh_data = wash_r3_hh,
                         container_data = wash_r3_cont,
                         individual_data = wash_r3_indiv)

whh<-wash$HH_dataset
wash_indicators<-c("lat_soap",
                   "wst_disp",
                   "soap_yest",
                   "soap_yest_when",
                   "wash_hands",
                   "hnd_sp",
                   "IS.disab",
                   "time_coll_wat")


whh[,wash_indicators]
whh_sf<-st_as_sf(whh,coords = kobo_coords, crs=4326)
# st_write(whh_sf %>% select(wash_indicators),dsn = "outputs/gis/scrap/wash_hh_r3_pt.shp")


hex<-st_make_grid(x = cmp %>% st_transform(crs=32646),cellsize = 200,square = F) %>%
  st_transform(crs=4326)

tm_shape(cmp)+
  tm_polygons(col = "red",border.col = "black")+
  tm_shape(hex)+
  tm_polygons(alpha = 0, border.col="white")

# st_write(hex,dsn = "outputs/gis/hex200.shp")
hex_poly<- st_as_sf(data.frame(grid_id=paste0("gr_",1:length(hex)),geometry=hex))
hex_poly %>%
  st_join(whh_sf) %>%
  group_by(grid_id) %>%
  summarise(number_per_hex=n()) %>%
  ggplot()+
  geom_histogram(aes(x=number_per_hex),
                 bins=100)+
  scale_x_log10()

whh_sf_on_grid<-whh_sf %>%
  st_join(hex_poly)


# analyze_data ------------------------------------------------------------
library(srvyr)
whh_svy<-as_survey(whh_sf_on_grid %>% st_drop_geometry())


wash_indicators_analyzed_grid<-butteR::mean_proportion_table(design = whh_svy,list_of_variables = wash_indicators,aggregation_level = "grid_id",na_replace = F)

wash_hh_r3_analyze

grids_gt3<-hex_poly %>%
  st_join(whh_sf) %>%st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(number_per_hex=n()) %>%
  filter(number_per_hex>3) %>%
  pull(grid_id)

wash_analysis_grids_gt3<-wash_indicators_analyzed_grid %>% filter(grid_id %in%grids_gt3)
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
