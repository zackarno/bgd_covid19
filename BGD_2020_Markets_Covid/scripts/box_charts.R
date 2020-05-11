rm(list = ls())

# library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

items<- c("food_item","non_food_item")[2]
current_round <- "round 1"

# function ----------------------------------------------------------------

fun_median <- function(x){
  return(data.frame(y=median(x),label=median(x,na.rm=T)))}
fun_max <- function(x){
  return(data.frame(y=max(x),label=max(x,na.rm=T)))}
fun_min <- function(x){
  return(data.frame(y=min(x),label=min(x,na.rm=T)))}



# path --------------------------------------------------------------------

outputfolder_box <-"outputs/03_charts/01_box/"

# read_data ---------------------------------------------------------------

raw_df <- read.csv("inputs/clean_data/2020_05_10_reach_bgd_market_assessment_cleaned_r2.csv", stringsAsFactors = FALSE,
                   na.strings = c("", " ", NA))
# date_log <- read.csv("outputs/01_data_logger/date_log.csv", stringsAsFactors = FALSE,
#                      na.strings = c("", " ", NA)) %>% select(-"reported_date")
#
# data_with_round<- raw_df %>%  left_join(date_log,"X_uuid") #add_round

# cleaned_df <- data_with_round %>% filter(round == current_round)

cleaned_df <- raw_df

# food item ---------------------------------------------------------------
if (items == "food_item"){
  box_fd_item_cols <- c("price_of_1kg","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                        "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                        "cheapest_price_for_12__of_eggs", "dry_fish_sale_in_past_week",
                        "cheapest_price_for_4mx5m_of_chicken") %>% sort()

  data_for_box <- cleaned_df[box_fd_item_cols]
  data_for_box2<- data_for_box %>% gather() %>% dplyr::filter(!is.na(value))

  data_for_box2 <- data_for_box2 %>% dplyr::mutate(
    name = if_else(grepl("price_of_1kg",data_for_box2$key),"Rice\n(1kg)",
                   if_else(grepl("cooking_oil",data_for_box2$key),"Oil\n(1L)",
                           if_else(grepl("lentils",data_for_box2$key),"Lentils \n (1kg)",
                                   if_else(grepl("leafy_greens",data_for_box2$key),"Leafy greens\n (.5kg)",
                                           if_else(grepl("eggs",data_for_box2$key),"Egg\n(12pcs)",
                                                   if_else(grepl("bananas",data_for_box2$key),"Banana\n(1kg)",
                                                           if_else(grepl("fish",data_for_box2$key),"Fish\n(1kg)",
                                                                   if_else(grepl("chicken",data_for_box2$key),"Chicken\n(12 pcs)","error",NULL
                                                                   )))))))))


  data_for_box2$name <- as.factor(data_for_box2$name)

  sample <- data_for_box2 %>%
    group_by(name) %>%
    tally()
  sample <- sample %>% mutate(
    ss = paste0("n=",n)
  )

  ymax <- max(data_for_box2$value)+25

  p <- ggplot(data_for_box2, aes(x = fct_reorder(name,-value),y = value))
  p <- p + geom_boxplot(width=0.3)+
    theme(axis.title.x = element_blank(),
          panel.background = element_rect(fill = "#FFFFFF", colour = "#505050",
                                          size = 2, linetype = "solid"),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                            colour = "#c1c1c1"))+ylab("Price (BDT)")+
    stat_summary(fun.data = fun_median, geom="text", size=3, hjust=-2)+
    stat_summary(fun.data = fun_max, geom="text", size=3,vjust=-.4)+
    stat_summary(fun.data = fun_min, geom="text", size=3, vjust=1.2 )

  dat <- ggplot_build(p)$data[[1]]

  p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                 y=middle, yend=middle), colour="#cf5858", size=1)+
    geom_text(data = sample,
              aes(name,Inf,label = ss),size=3, vjust = 1.8)+
    ylim(0,ymax)


  ggsave(path = outputfolder_box,filename ="food_item.jpg" ,width=14.8942,height=7.62,units="cm",scale = 1.8)
}

# non_food_item -----------------------------------------------------------

if(items == "non_food_item"){

  box_non_fd_item_cols <- c("cheapest_price_for_100g_soap_bar_of_soap", "cheapest_price_for_0_5l_of_bleachwashing_powder",
                            "cheapest_price_for_12_of_paracetamol","cheapest_price_for_4mx5m_of_tarpaulin")
  data_for_box_non_fd <- cleaned_df[box_non_fd_item_cols]
  data_for_box_non_fd2<- data_for_box_non_fd %>% gather() %>% dplyr::filter(!is.na(value))


  data_for_box_non_fd2 <- data_for_box_non_fd2 %>% dplyr::mutate(
    name = if_else(grepl("soap_bar",data_for_box_non_fd2$key),"Soap\n(100g)",
                   if_else(grepl("bleachwashing",data_for_box_non_fd2$key),"Bleach/washing powder\n(5l)",
                           if_else(grepl("paracetamol ",data_for_box_non_fd2$key),"Paracetamol\n(12 pcs)",
                                   if_else(grepl("tarpaulin ",data_for_box_non_fd2$key),"Tarpaulin\n(4mx5m)","error",NULL)))))


  data_for_box_non_fd2$name <- as.factor(data_for_box_non_fd2$name)

  sample2 <- data_for_box_non_fd2 %>%
    group_by(name) %>%
    tally()
  sample2 <- sample2 %>% mutate(
    ss = paste0("n=",n)
  )

  ymax <- max(data_for_box_non_fd2$value)+25

  q <- ggplot(data_for_box_non_fd2, aes(x = fct_reorder(name,-value),y = value))
  q <- q + geom_boxplot(width=0.1)+
    theme(axis.title.x = element_blank(),
          panel.background = element_rect(fill = "#FFFFFF", colour = "#505050",
                                          size = 2, linetype = "solid"),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                            colour = "#c1c1c1"))+ylab("Price (BDT)")+
    stat_summary(fun.data = fun_median, geom="text", size=3, hjust=-2)+
    stat_summary(fun.data = fun_max, geom="text", size=3,vjust=-.4)+
    stat_summary(fun.data = fun_min, geom="text", size=3, vjust=1.2 )

  dat2 <- ggplot_build(q)$data[[1]]

  q + geom_segment(data=dat2, aes(x=xmin, xend=xmax,
                                  y=middle, yend=middle), colour="#cf5858", size=1)+
    geom_text(data = sample2,
              aes(name,Inf,label = ss),size=3, vjust = 1.8)+
    ylim(0,ymax)

  ggsave(path = outputfolder_box,filename ="non_food_item.jpg" ,width=7.9706,height=7.62,units="cm",scale = 1.8)
}
