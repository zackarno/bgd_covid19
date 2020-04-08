library(tidyverse)
library(sf)

cmp_gdb<-"../../../../02_GIS_DataUnit/01_GIS_BASE_Data/01_boundaries/03_camp"
facility_path<-"../../../01_GIS_BASE_Data/02_landscape/01_infrastructure/03_facilities/co_facilities_mapping_dataset_v25x_hdx.csv"
st_layers(cmp_gdb)
cmp<-st_read(cmp_gdb,"CampBoundaryA1_190310")
cmp_block_utm<-st_read(cmp_gdb,"190310_A2_camp_boundaries_for_atlas")
health_codes<-read_csv("inputs/health_facility_codebook.csv")
col_names <- names(read_csv("inputs/unhcr_rohingya_pop_stats_20200319.csv", n_max = 0))
pop<-read_csv("inputs/unhcr_rohingya_pop_stats_20200319.csv")

#MAKE POP DATA  SO IT CAN JOIN ADMIN SHP
pop_cmp<-pop %>%
  filter(str_ends(camp, "Total")&camp!="Total") %>%
  mutate(camp= str_replace_all(camp,c("Total"="")) %>% trimws(),
         v.female_young= female_lt1+female_lte4,
         v.male_young= male_lt1+male_lte4,
         v.female_young_old=v.female_young+female_gte60,
         v.male_young_old= v.male_young+male_gte60,
         v.old=male_gte60+female_gte60)

colnames(pop_cmp)
#READ IN FACIITY DATA JOIN IT
fd<- read_csv(facility_path)

fd<- fd %>% filter(!is.na(GPS_Latitude)&!is.na(GPS_Longitude))
fd<-st_as_sf(x = fd,coords = c("GPS_Longitude", "GPS_Latitude"),crs=4326)


fd_cmp<-fd
# fd_cmp<-fd %>% st_join(cmp,left=F)
fd_hea<-fd_cmp %>% filter(Scode=="HEA")
fd_hea$Agency_uid

fd_hea<-fd_hea %>% left_join(health_codes, by=c("FTcode"="Ftcode"))
fd_hea$`Facility Type (sector prefer)` %>% unique() %>% dput()
 ggplot()+
   geom_sf(data=fd_cmp)+
   geom_sf(data=fd_hea %>% filter(FTcode=='PH'))

fd_hea$Agency_uid
fd_phc %>% View()
 fd_phc<-fd_hea %>% filter(FTcode=="PH")
set.seed(500)
iso_bed_sample<-sample_n(fd_phc,9)

fd_phc<-fd_phc %>%
  mutate(
    iso_bed=ifelse(BARCODE %in% iso_bed_sample$BARCODE,"yes","no"),
    iso_bed_capacity= case_when(Camp_Name=="Camp 11"~2,
                                iso_bed=="yes"~5)

         ) %>%
  filter(iso_bed=="yes")

cmp_data<-cmp %>%
  left_join(fd_phc%>% st_drop_geometry(), by=c("New_Camp_N"="Camp_Name")) %>%
  left_join(pop_cmp, by=c("New_Camp_N"= "camp")) %>%
  mutate(
    camp_region=case_when(New_Camp_N %in% c("Choukhali", "Camp 21", "Camp 22","Camp 23")~"Isolated",
                          Upazila=="Ukhia"~"North",
                          Upazila=="Teknaf"~"South",
    )
  )

cmp_data<- cmp_data %>% filter(!is.na(v.old ))
elderly_total<-cmp_data$v.old %>% sum()

num_infected_list<-list()
test_percents<-seq(0,0.01,by=.0001)
mat_to_fill<-matrix(NA,101,2)
for(i in 1: length(test_percents)){
  print(i)
  num_infected<-elderly_total*test_percents[i]
  mat_to_fill[i,1]<-test_percents[i]
  mat_to_fill[i,2]<-num_infected
}
eld_case_load<-data.frame(mat_to_fill)
colnames(eld_case_load)<-c("infect_rate", "number_elderly")
?lm
lm(formula = number_elderly~infect_rate, eld_case_load)
lm(formula = infect_rate~number_elderly, eld_case_load)
3.119e-05  *47
ggplot(eld_case_load, aes(x=infect_rate, y=number_elderly))+
  geom_point()+
  scale_x_continuous(labels=scales::percent, limits = c(0,0.005))+
  geom_hline(yintercept = 47)+
  geom_vline(xintercept=0.00146593)+
  labs(x="infection rate", y= "number elderly infected")+
  theme_bw()+
  annotate(geom = "text", y = 100, x = 0.0013, label = "~ 0.15 %", color = "red",
           angle = 90)+
  annotate(geom = "text", y = 60, x = 0.0005, label = "47 available beds ", color = "red",
           angle = 0)

geom_text("")
locator()
seq_along(0,1,by=0.1)


fd_hea %>%
  group_by(`Facility Type (sector prefer)`) %>%
  summarise(num_facilities=n()) %>% data.frame()

fd_hea_wide<- fd_hea %>%
  group_by(New_Camp_N,
           `Facility Type (sector prefer)`) %>%
  summarise(num_fac=n()) %>%
  arrange(New_Camp_N) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = `Facility Type (sector prefer)`,
              values_from = num_fac)
cmp_poly<-cmp %>%
  left_join(fd_hea_wide) %>%
  left_join(pop_cmp, by=c("New_Camp_N"= "camp")) %>%
  mutate(
    camp_region=case_when(New_Camp_N %in% c("Choukhali", "Camp 21", "Camp 22","Camp 23")~"Isolated",
                          Upazila=="Ukhia"~"North",
                          Upazila=="Teknaf"~"South",
                          )
  )


cmp_poly$camp_region
library(tmap)
tm <- tm_shape(cmp_poly) +
  tm_polygons( legend.title = "Happy Planet Index")+
  tm_text("New_Camp_N", size="AREA")

fd_hea %>%
  group_by(Camp_Name,
           `Facility Type (sector prefer)`) %>%
  summarise(num_fac=n()) %>% arrange(Camp_Name) %>% data.frame()


fd %>% filter(Scode=="WAS")
