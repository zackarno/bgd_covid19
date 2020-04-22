library(dplyr)
library(lubridate)
library(sf)
library(butteR)
library(tmap)
library(tmaptools)
library(ggplot2)
library(maptools)
library(spatstat)

source("scripts/prepare_msna_wash_inputs.R")
wash_hh_sf<-st_as_sf(wash_hh,coords = kobo_coords, crs=4326) %>% st_transform(crs=32646)
wash_hh_sf_indic<-wash_hh_sf %>% filter(I.wst_disp.open_area=="yes")
st_coordinates(wash_hh_sf_indic) %>%
  data.frame() %>% distinct(x,y)

#this was attempted for smooth_map function we reported invalid geom
wash_hh_sf_indic<-sf:::distinct.sf(wash_hh_sf_indic)

#conver to spatial and ppp
wash_hh.sp <- as(wash_hh_sf_indic, "Spatial")
wash_hh.ppp <- as(wash_hh.sp, "ppp")
marks(wash_hh.ppp)<-NULL
cmp_gdb<-"../../../../02_GIS_DataUnit/01_GIS_BASE_Data/01_boundaries/03_camp"
cmp<-st_read(cmp_gdb,"CampBoundaryA1_190310") %>% st_transform(crs=32646)
camp.sp<-as(cmp,"Spatial")

camp.owin<- as.owin(camp.sp)
Window(wash_hh.ppp)<-camp.owin

qudrat_test_result<-quadrat.test(wash_hh.ppp, nx = 40, ny = 40)
ds <- smooth_map(wash_hh_sf_indic, bandwidth = 0.5, cover = cmp, unit = "mi", style = "quantile")


wash_hh.ppp.nndist <- nndist(wash_hh.ppp)
mean(wash_hh.ppp.nndist)

clarkevans.test(wash_hh.ppp)
envK <- envelope(wash_hh.ppp, fun = Kest, correction="border", nsim = 49)
plot(envK, main = "K Function")

